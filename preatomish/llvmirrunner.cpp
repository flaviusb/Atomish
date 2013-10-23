// Externed functions to jit some llvm ir
// Externed functions to call the jitted code
// Externed functions to build code without going through ir, exposed via a scope object to jitted code
// Externed functions to jit the built code
// Scope management for jitted ir
// 'main' 
// This starts a main loop that jits some llvm ir and then calls it, and has some functions that can be called from that


#include "llvm/Analysis/Passes.h"
#include "llvm/Analysis/Verifier.h"
#include "llvm/ExecutionEngine/ExecutionEngine.h"
#include "llvm/ExecutionEngine/JIT.h"
#include "llvm/ExecutionEngine/MCJIT.h"
#include "llvm/ExecutionEngine/ObjectCache.h"
#include "llvm/ExecutionEngine/SectionMemoryManager.h"
#include "llvm/IR/DataLayout.h"
#include "llvm/IR/DerivedTypes.h"
#include "llvm/IR/IRBuilder.h"
#include "llvm/IR/LLVMContext.h"
#include "llvm/IR/Module.h"
#include "llvm/IRReader/IRReader.h"
#include "llvm/PassManager.h"
#include "llvm/Support/CommandLine.h"
#include "llvm/Support/FileSystem.h"
#include "llvm/Support/Path.h"
#include "llvm/Support/raw_ostream.h"
#include "llvm/Support/SourceMgr.h"
#include "llvm/Support/TargetSelect.h"
#include "llvm/Transforms/Scalar.h"
#include <cctype>
#include <cstdio>
#include <map>
#include <string>
#include <vector>

using namespace llvm;

//===----------------------------------------------------------------------===//
// Command-line options
//===----------------------------------------------------------------------===//

namespace {
  cl::opt<std::string>
  InputIR("input-IR",
              cl::desc("Specify the name of an IR file to load for function definitions"),
              cl::value_desc("input IR file name"));

  cl::opt<bool>
  VerboseOutput("verbose", 
                cl::desc("Enable verbose output (results, IR, etc.) to stderr"),
                cl::init(false));

  cl::opt<bool>
  DumpModulesOnExit("dump-modules",
                  cl::desc("Dump IR from modules to stderr on shutdown"),
                  cl::init(false));

  cl::opt<bool> EnableLazyCompilation(
    "enable-lazy-compilation", cl::desc("Enable lazy compilation when using the MCJIT engine"),
    cl::init(true));

  cl::opt<bool> UseObjectCache(
    "use-object-cache", cl::desc("Enable use of the MCJIT object caching"),
    cl::init(false));
} // namespace


//===----------------------------------------------------------------------===//
// Quick and dirty hack
//===----------------------------------------------------------------------===//

// FIXME: Obviously we can do better than this
std::string GenerateUniqueName(const char *root)
{
  static int i = 0;
  char s[16];
  sprintf(s, "%s%d", root, i++);
  std::string S = s;
  return S;
}

std::string MakeLegalFunctionName(std::string Name)
{
  std::string NewName;
  if (!Name.length())
      return GenerateUniqueName("anon_func_");

  // Start with what we have
  NewName = Name;

  // Look for a numberic first character
  if (NewName.find_first_of("0123456789") == 0) {
    NewName.insert(0, 1, 'n');
  }

  // Replace illegal characters with their ASCII equivalent
  std::string legal_elements = "_abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789";
  size_t pos;
  while ((pos = NewName.find_first_not_of(legal_elements)) != std::string::npos) {
    char old_c = NewName.at(pos);
    char new_str[16];
    sprintf(new_str, "%d", (int)old_c);
    NewName = NewName.replace(pos, 1, new_str);
  }

  return NewName;
}

//===----------------------------------------------------------------------===//
// MCJIT object cache class
//===----------------------------------------------------------------------===//

class MCJITObjectCache : public ObjectCache {
public:
  MCJITObjectCache() {
    // Set IR cache directory
    sys::fs::current_path(CacheDir);
    sys::path::append(CacheDir, "toy_object_cache");
  }

  virtual ~MCJITObjectCache() {
  }

  virtual void notifyObjectCompiled(const Module *M, const MemoryBuffer *Obj) {
    // Get the ModuleID
    const std::string ModuleID = M->getModuleIdentifier();

    // If we've flagged this as an IR file, cache it
    if (0 == ModuleID.compare(0, 3, "IR:")) {
      std::string IRFileName = ModuleID.substr(3);
      SmallString<128>IRCacheFile = CacheDir;
      sys::path::append(IRCacheFile, IRFileName);
      if (!sys::fs::exists(CacheDir.str()) && sys::fs::create_directory(CacheDir.str())) {
        fprintf(stderr, "Unable to create cache directory\n");
        return;
      }
      std::string ErrStr;
      raw_fd_ostream IRObjectFile(IRCacheFile.c_str(), ErrStr, sys::fs::F_Binary);
      IRObjectFile << Obj->getBuffer();
    }
  }

  // MCJIT will call this function before compiling any module
  // MCJIT takes ownership of both the MemoryBuffer object and the memory
  // to which it refers.
  virtual MemoryBuffer* getObject(const Module* M) {
    // Get the ModuleID
    const std::string ModuleID = M->getModuleIdentifier();

    // If we've flagged this as an IR file, cache it
    if (0 == ModuleID.compare(0, 3, "IR:")) {
      std::string IRFileName = ModuleID.substr(3);
      SmallString<128> IRCacheFile = CacheDir;
      sys::path::append(IRCacheFile, IRFileName);
      if (!sys::fs::exists(IRCacheFile.str())) {
        // This file isn't in our cache
        return NULL;
      }
      OwningPtr<MemoryBuffer> IRObjectBuffer;
      MemoryBuffer::getFile(IRCacheFile.c_str(), IRObjectBuffer, -1, false);
      // MCJIT will want to write into this buffer, and we don't want that
      // because the file has probably just been mmapped.  Instead we make
      // a copy.  The filed-based buffer will be released when it goes
      // out of scope.
      return MemoryBuffer::getMemBufferCopy(IRObjectBuffer->getBuffer());
    }

    return NULL;
  }

private:
  SmallString<128> CacheDir;
};

//===----------------------------------------------------------------------===//
// IR input file handler
//===----------------------------------------------------------------------===//

Module* parseInputIR(std::string InputFile, LLVMContext &Context) {
  SMDiagnostic Err;
  Module *M = ParseIRFile(InputFile, Err, Context);
  if (!M) {
    Err.print("IR parsing failed: ", errs());
    return NULL;
  }

  char ModID[256];
  sprintf(ModID, "IR:%s", InputFile.c_str());
  M->setModuleIdentifier(ModID);
  return M;
}

//===----------------------------------------------------------------------===//
// Helper class for execution engine abstraction
//===----------------------------------------------------------------------===//

class BaseHelper
{
public:
  BaseHelper() {}
  virtual ~BaseHelper() {}
  virtual Function *getFunction(const std::string FnName) = 0;
  virtual Module *getModuleForNewFunction() = 0;
  virtual void *getPointerToFunction(Function* F) = 0;
  virtual void *getPointerToNamedFunction(const std::string &Name) = 0;
  virtual void closeCurrentModule() = 0;
  virtual void runFPM(Function &F) = 0;
  virtual void dump() = 0;
};

//===----------------------------------------------------------------------===//
// Helper class for JIT execution engine
//===----------------------------------------------------------------------===//

class JITHelper : public BaseHelper {
public:
  JITHelper(LLVMContext &Context) {
    // Make the module, which holds all the code.
    if (!InputIR.empty()) {
      TheModule = parseInputIR(InputIR, Context);
    } else {
      TheModule = new Module("my cool jit", Context);
    }

    // Create the JIT.  This takes ownership of the module.
    std::string ErrStr;
    TheExecutionEngine = EngineBuilder(TheModule).setErrorStr(&ErrStr).create();
    if (!TheExecutionEngine) {
      fprintf(stderr, "Could not create ExecutionEngine: %s\n", ErrStr.c_str());
      exit(1);
    }

    TheFPM = new FunctionPassManager(TheModule);

    // Set up the optimizer pipeline.  Start with registering info about how the
    // target lays out data structures.
    TheFPM->add(new DataLayout(*TheExecutionEngine->getDataLayout()));
    // Provide basic AliasAnalysis support for GVN.
    TheFPM->add(createBasicAliasAnalysisPass());
    // Promote allocas to registers.
    TheFPM->add(createPromoteMemoryToRegisterPass());
    // Do simple "peephole" optimizations and bit-twiddling optzns.
    TheFPM->add(createInstructionCombiningPass());
    // Reassociate expressions.
    TheFPM->add(createReassociatePass());
    // Eliminate Common SubExpressions.
    TheFPM->add(createGVNPass());
    // Simplify the control flow graph (deleting unreachable blocks, etc).
    TheFPM->add(createCFGSimplificationPass());

    TheFPM->doInitialization();
  }

  virtual ~JITHelper() {
    if (TheFPM)
      delete TheFPM;
    if (TheExecutionEngine)
      delete TheExecutionEngine;
  }

  virtual Function *getFunction(const std::string FnName) {
    assert(TheModule);
    return TheModule->getFunction(FnName);
  }

  virtual Module *getModuleForNewFunction() {
    assert(TheModule);
    return TheModule;
  }

  virtual void *getPointerToFunction(Function* F) {
    assert(TheExecutionEngine);
    return TheExecutionEngine->getPointerToFunction(F);
  }

  virtual void *getPointerToNamedFunction(const std::string &Name) {
    return TheExecutionEngine->getPointerToNamedFunction(Name);
  }

  virtual void runFPM(Function &F) {
    assert(TheFPM);
    TheFPM->run(F);
  }

  virtual void closeCurrentModule() {
    // This should never be called for JIT
    assert(false);
  }

  virtual void dump() {
    assert(TheModule);
    TheModule->dump();
  }

private:
  Module *TheModule;
  ExecutionEngine *TheExecutionEngine;
  FunctionPassManager *TheFPM;
};

//===----------------------------------------------------------------------===//
// MCJIT helper class
//===----------------------------------------------------------------------===//

class MCJITHelper : public BaseHelper
{
public:
  MCJITHelper(LLVMContext& C) : Context(C), CurrentModule(NULL) {
    if (!InputIR.empty()) {
      Module *M = parseInputIR(InputIR, Context);
      Modules.push_back(M);
      if (!EnableLazyCompilation)
        compileModule(M);
    }
  }
  virtual ~MCJITHelper();

  Function *getFunction(const std::string FnName);
  Module *getModuleForNewFunction();
  void *getPointerToFunction(Function* F);
  void *getPointerToNamedFunction(const std::string &Name);
  void closeCurrentModule();
  virtual void runFPM(Function &F) {} // Not needed, see compileModule
  void dump();

protected:
  ExecutionEngine *compileModule(Module *M);

private:
  typedef std::vector<Module*> ModuleVector;

  MCJITObjectCache OurObjectCache;

  LLVMContext  &Context;
  ModuleVector  Modules;

  std::map<Module *, ExecutionEngine *> EngineMap;

  Module       *CurrentModule;
};

class HelpingMemoryManager : public SectionMemoryManager
{
  HelpingMemoryManager(const HelpingMemoryManager&) LLVM_DELETED_FUNCTION;
  void operator=(const HelpingMemoryManager&) LLVM_DELETED_FUNCTION;

public:
  HelpingMemoryManager(MCJITHelper *Helper) : MasterHelper(Helper) {}
  virtual ~HelpingMemoryManager() {}

  /// This method returns the address of the specified function.
  /// Our implementation will attempt to find functions in other
  /// modules associated with the MCJITHelper to cross link functions
  /// from one generated module to another.
  ///
  /// If \p AbortOnFailure is false and no function with the given name is
  /// found, this function returns a null pointer. Otherwise, it prints a
  /// message to stderr and aborts.
  virtual void *getPointerToNamedFunction(const std::string &Name,
                                          bool AbortOnFailure = true);
private:
  MCJITHelper *MasterHelper;
};

void *HelpingMemoryManager::getPointerToNamedFunction(const std::string &Name,
                                        bool AbortOnFailure)
{
  // Try the standard symbol resolution first, but ask it not to abort.
  void *pfn = RTDyldMemoryManager::getPointerToNamedFunction(Name, false);
  if (pfn)
    return pfn;

  pfn = MasterHelper->getPointerToNamedFunction(Name);
  if (!pfn && AbortOnFailure)
    report_fatal_error("Program used external function '" + Name +
                        "' which could not be resolved!");
  return pfn;
}

MCJITHelper::~MCJITHelper()
{
  // Walk the vector of modules.
  ModuleVector::iterator it, end;
  for (it = Modules.begin(), end = Modules.end();
       it != end; ++it) {
    // See if we have an execution engine for this module.
    std::map<Module*, ExecutionEngine*>::iterator mapIt = EngineMap.find(*it);
    // If we have an EE, the EE owns the module so just delete the EE.
    if (mapIt != EngineMap.end()) {
      delete mapIt->second;
    } else {
      // Otherwise, we still own the module.  Delete it now.
      delete *it;
    }
  }
}

Function *MCJITHelper::getFunction(const std::string FnName) {
  ModuleVector::iterator begin = Modules.begin();
  ModuleVector::iterator end = Modules.end();
  ModuleVector::iterator it;
  for (it = begin; it != end; ++it) {
    Function *F = (*it)->getFunction(FnName);
    if (F) {
      if (*it == CurrentModule)
          return F;

      assert(CurrentModule != NULL);

      // This function is in a module that has already been JITed.
      // We just need a prototype for external linkage.
      Function *PF = CurrentModule->getFunction(FnName);
      if (PF && !PF->empty()) {
        //ErrorF("redefinition of function across modules");
        return 0;
      }

      // If we don't have a prototype yet, create one.
      if (!PF)
        PF = Function::Create(F->getFunctionType(),
                                      Function::ExternalLinkage,
                                      FnName,
                                      CurrentModule);
      return PF;
    }
  }
  return NULL;
}

Module *MCJITHelper::getModuleForNewFunction() {
  // If we have a Module that hasn't been JITed, use that.
  if (CurrentModule)
    return CurrentModule;

  // Otherwise create a new Module.
  std::string ModName = GenerateUniqueName("mcjit_module_");
  Module *M = new Module(ModName, Context);
  Modules.push_back(M);
  CurrentModule = M;

  return M;
}

ExecutionEngine *MCJITHelper::compileModule(Module *M) {
  assert(EngineMap.find(M) == EngineMap.end());

  if (M == CurrentModule)
    closeCurrentModule();

  std::string ErrStr;
  ExecutionEngine *EE = EngineBuilder(M)
                            .setErrorStr(&ErrStr)
                            .setUseMCJIT(true)
                            .setMCJITMemoryManager(new HelpingMemoryManager(this))
                            .create();
  if (!EE) {
    fprintf(stderr, "Could not create ExecutionEngine: %s\n", ErrStr.c_str());
    exit(1);
  }

  if (UseObjectCache)
    EE->setObjectCache(&OurObjectCache);
  // Get the ModuleID so we can identify IR input files
  const std::string ModuleID = M->getModuleIdentifier();

  // If we've flagged this as an IR file, it doesn't need function passes run.
  if (0 != ModuleID.compare(0, 3, "IR:")) {
    FunctionPassManager *FPM = 0;

    // Create a FPM for this module
    FPM = new FunctionPassManager(M);

    // Set up the optimizer pipeline.  Start with registering info about how the
    // target lays out data structures.
    FPM->add(new DataLayout(*EE->getDataLayout()));
    // Provide basic AliasAnalysis support for GVN.
    FPM->add(createBasicAliasAnalysisPass());
    // Promote allocas to registers.
    FPM->add(createPromoteMemoryToRegisterPass());
    // Do simple "peephole" optimizations and bit-twiddling optzns.
    FPM->add(createInstructionCombiningPass());
    // Reassociate expressions.
    FPM->add(createReassociatePass());
    // Eliminate Common SubExpressions.
    FPM->add(createGVNPass());
    // Simplify the control flow graph (deleting unreachable blocks, etc).
    FPM->add(createCFGSimplificationPass());

    FPM->doInitialization();

    // For each function in the module
    Module::iterator it;
    Module::iterator end = M->end();
    for (it = M->begin(); it != end; ++it) {
      // Run the FPM on this function
      FPM->run(*it);
    }

    delete FPM;
  }

  EE->finalizeObject();

  // Store this engine
  EngineMap[M] = EE;

  return EE;
}

void *MCJITHelper::getPointerToFunction(Function* F) {
  // Look for this function in an existing module
  ModuleVector::iterator begin = Modules.begin();
  ModuleVector::iterator end = Modules.end();
  ModuleVector::iterator it;
  std::string FnName = F->getName();
  for (it = begin; it != end; ++it) {
    Function *MF = (*it)->getFunction(FnName);
    if (MF == F) {
      std::map<Module*, ExecutionEngine*>::iterator eeIt = EngineMap.find(*it);
      if (eeIt != EngineMap.end()) {
        void *P = eeIt->second->getPointerToFunction(F);
        if (P)
          return P;
      } else {
        ExecutionEngine *EE = compileModule(*it);
        void *P = EE->getPointerToFunction(F);
        if (P)
          return P;
      }
    }
  }
  return NULL;
}

void MCJITHelper::closeCurrentModule() {
    // If we have an open module (and we should), pack it up
  if (CurrentModule) {
    CurrentModule = NULL;
  }
}

void *MCJITHelper::getPointerToNamedFunction(const std::string &Name)
{
  // Look for the functions in our modules, compiling only as necessary
  ModuleVector::iterator begin = Modules.begin();
  ModuleVector::iterator end = Modules.end();
  ModuleVector::iterator it;
  for (it = begin; it != end; ++it) {
    Function *F = (*it)->getFunction(Name);
    if (F && !F->empty()) {
      std::map<Module*, ExecutionEngine*>::iterator eeIt = EngineMap.find(*it);
      if (eeIt != EngineMap.end()) {
        void *P = eeIt->second->getPointerToFunction(F);
        if (P)
          return P;
      } else {
        ExecutionEngine *EE = compileModule(*it);
        void *P = EE->getPointerToFunction(F);
        if (P)
          return P;
      }
    }
  }
  return NULL;
}

void MCJITHelper::dump()
{
  ModuleVector::iterator begin = Modules.begin();
  ModuleVector::iterator end = Modules.end();
  ModuleVector::iterator it;
  for (it = begin; it != end; ++it)
    (*it)->dump();
}

//===----------------------------------------------------------------------===//
// Code Generation
//===----------------------------------------------------------------------===//

static BaseHelper *TheHelper;
static IRBuilder<> Builder(getGlobalContext());
static std::map<std::string, AllocaInst*> NamedValues;

//Value *ErrorV(const char *Str) { Error(Str); return 0; }

/// CreateEntryBlockAlloca - Create an alloca instruction in the entry block of
/// the function.  This is used for mutable variables etc.
//

// Hrmmm.

/*static AllocaInst *CreateEntryBlockAlloca(Function *TheFunction,
                                          const std::string &VarName) {
  IRBuilder<> TmpB(&TheFunction->getEntryBlock(),
                 TheFunction->getEntryBlock().begin());
  return TmpB.CreateAlloca(Type::getDoubleTy(getGlobalContext()), 0,
                           VarName.c_str());
}

Value *VariableExprAST::Codegen() {
  // Look this variable up in the function.
  Value *V = NamedValues[Name];
  if (V == 0) return ErrorV("Unknown variable name");

  // Load the value.
  return Builder.CreateLoad(V, Name.c_str());
}*/

//===----------------------------------------------------------------------===//
// Top-Level parsing and JIT Driver
//===----------------------------------------------------------------------===//

/*static void HandleExtern() {
  if (PrototypeAST *P = ParseExtern()) {
    Function *F = P->Codegen();
    if (F && VerboseOutput) {
      fprintf(stderr, "Read extern: ");
      F->dump();
    }
  } else {
    // Skip token for error recovery.
    getNextToken();
  }
}*/

static void MainLoop() {
  while (1) {
  }
}

//===----------------------------------------------------------------------===//
// "Library" functions that can be "extern'd" from user code.
//===----------------------------------------------------------------------===//


//===----------------------------------------------------------------------===//
// Main driver code.
//===----------------------------------------------------------------------===//

int main(int argc, char **argv) {
  cl::ParseCommandLineOptions(argc, argv,
                              "Atomish ir runner\n");

  InitializeNativeTarget();
  InitializeNativeTargetAsmPrinter();
  InitializeNativeTargetAsmParser();
  LLVMContext &Context = getGlobalContext();

  // Install the pre-prelude

  // Make the Helper, which holds all the code.
  TheHelper = new MCJITHelper(Context);

  // Run the main "interpreter loop" now.
  MainLoop();

  // Print out all of the generated code.
  if (DumpModulesOnExit)
    TheHelper->dump();

  return 0;
}
