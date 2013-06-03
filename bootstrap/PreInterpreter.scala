package net.flaviusb.atomish

import java.io.{File, FileInputStream, Writer, Reader, BufferedReader, StringReader, IOException, FileReader, FileWriter}
import scala.io.{BufferedSource}

import scala.collection.mutable.{Map => MMap}

object PreAtomishInterpreter {
  var u = new PreUniverse()
  var r = new PreReader()
  var e = PreEvaller.eval(u) _;
  // Stitch the parts together
  u.roots("read") = r.alien_read;
  u.roots("eval") = AlienProxy(_.args match {
    case List(Left(arg: AtomishCode))                           => e(arg, None)
    case List(Left(arg: AtomishCode), Left(base: AtomishThing)) => e(arg, Some(base))
    case _                                                      => null // Should error
  })
  u.roots("print") = AlienProxy(_.args match {
    case List(Left(arg: AtomishThing)) => AtomishString(PreScalaPrinter.print(arg))
    case _                             => null // Should error
  })
  u.roots("print_with_forms") = AlienProxy(_.args match {
    case List(Left(arg: AtomishThing)) => AtomishString(PreScalaPrinter.print_with_forms(arg))
    case _                             => null // Should error
  })
  def main(args: Array[String]): Unit = {
    if(args.length == 0) {
      println("No file specified.")
      return
    }
    var source_name = args(0);
    var file_source = new File(source_name)
    u.roots("System") = AtomishOrigin(MMap[String, AtomishThing](
      "programArguments" -> AtomishArray(args.drop(1).map(x => AtomishString(x)))
    ))
    u.roots("FileSystem") = AtomishOrigin(MMap[String, AtomishThing](
      "cwd"              -> AtomishString(file_source.getAbsoluteFile().getParent()),
      "exists?"          -> AlienProxy(_.args match {
        case List(Left(AtomishString(file_name))) => AtomishBoolean((new File(u.roots("FileSystem").cells("cwd").asInstanceOf[AtomishString].value, file_name)).exists)
        case List(Left(AtomishString(file_base)), Left(AtomishString(file_name))) => AtomishBoolean((new File(file_base, file_name)).exists)
        case _                                    => AtomishUnset //Should soft error
      }),
      "removeFile!"      -> AlienProxy(_.args match {
        case List(Left(AtomishString(file_name))) => AtomishBoolean((new File(u.roots("FileSystem").cells("cwd").asInstanceOf[AtomishString].value, file_name)).delete())
        case List(Left(AtomishString(file_base)), Left(AtomishString(file_name))) => AtomishBoolean((new File(file_base, file_name)).delete())
      }),
      "parentOf"         -> AlienProxy(_.args match {
        case List(Left(AtomishString(file_name))) => AtomishString((new File(u.roots("FileSystem").cells("cwd").asInstanceOf[AtomishString].value, file_name)).getParent())
        case List(Left(AtomishString(file_base)), Left(AtomishString(file_name))) => AtomishString((new File(file_base, file_name)).getParent())
      }),
      "withOpenFile"     -> AlienProxy(x => {
        var(file_base, file_name, lexical_thingy) = x.args match {
          case List(Left(AtomishString(file_name)), Left(lexical_thingy)) => (u.roots("FileSystem").cells("cwd").asInstanceOf[AtomishString].value, file_name, lexical_thingy)
          case List(Left(AtomishString(file_base)), Left(AtomishString(file_name)), Left(lexical_thingy)) => (file_base, file_name, lexical_thingy)
        }
        var io_file = new File(file_base, file_name)
        var writer = new FileWriter(io_file);
        var io = AtomishOrigin(MMap[String, AtomishThing](
          "put"   -> AlienProxy(_.args match {
            case List(Left(x)) => { writer.write(PreScalaPrinter.print(x)); AtomishUnset }
          }),
          "flush" -> AlienProxy(x => {writer.flush(); AtomishUnset})
        ))
        var ret = lexical_thingy match {
          case q: AlienProxy  => q.activate(AtomishArgs(List(Left(io))))
          case q: QAlienProxy => q.activate(AtomishCommated(Array(io)))
        }
        //io.cells("flush").asInstanceOf[AlienProxy].activate(AtomishArgs(List()))
        writer.flush()
        writer.close()
        ret
      }),
    "readFully"        -> AlienProxy(x => {
      var (file_base, file_name) = x.args match {
        case List(Left(AtomishString(file_name))) => (u.roots("FileSystem").cells("cwd").asInstanceOf[AtomishString].value, file_name)
        case List(Left(AtomishString(file_base)), Left(AtomishString(file_name))) => (file_base, file_name)
      }
      var file_source = new BufferedSource(new FileInputStream(new File(u.roots("FileSystem").cells("cwd").asInstanceOf[AtomishString].value,
        file_name)))
      AtomishString(file_source.addString(new StringBuilder(1024)).toString())
      })
    ))
    u.roots("Shell") = AtomishOrigin(MMap[String, AtomishThing](
      "_out"              -> AlienProxy(_.args match {
        case List(Left(AtomishCall("'", Array(_printer))), Left(AtomishCall("'", Array(_exit_status))), Left(AtomishString(pwd)), Left(stdin), Left(env), Left(AtomishArray(cmds))) => {
          var printer     = u.roots("eval").asInstanceOf[AlienProxy].activate(AtomishArgs(List(Left(_printer))))
          var exit_status = u.roots("eval").asInstanceOf[AlienProxy].activate(AtomishArgs(List(Left(_exit_status))))
          var pb = new java.lang.ProcessBuilder(cmds.map(_ match { case AtomishString(x) => x }): _*)
          pb.directory(new java.io.File(pwd))
          pb.redirectErrorStream(true)
          var proc = pb.start
          var br = new java.io.BufferedReader(new java.io.InputStreamReader(proc.getInputStream))
          var it: String = null
          if(stdin != AtomishBoolean(false)) {
            var stdinstream = new java.io.BufferedWriter(new java.io.OutputStreamWriter(proc.getOutputStream))
            stdinstream write(stdin.asInstanceOf[AtomishString].value, 0, stdin.asInstanceOf[AtomishString].value.length)
            stdinstream.flush
            stdinstream.close
          }
          it = br.readLine;
          while(it != null) {
            printer match {
              case x: AlienProxy  => x.activate(AtomishArgs(List(Left(AtomishString(it)))))
              case x: QAlienProxy => x.activate(AtomishCommated(Array(AtomishString(it))))
            }
            it = br.readLine;
          }
          var exit_code = proc.waitFor;
          exit_status.asInstanceOf[AlienProxy].activate(AtomishArgs(List(Left(AtomishInt(exit_code)))))
        }
        case x => { x.foreach(z => println(z.toString())) ; AtomishUnset }
      })
    ))
    AtomishThing.post_bootstrap ++= MMap[(String, String), AtomishThing => AtomishThing](
      ("Boolean", "==")       -> { thing => AlienProxy(booltobool(_ == thing.asInstanceOf[AtomishBoolean].value)) },
      ("Boolean", "and")      -> { thing => AlienProxy(booltobool(_ && thing.asInstanceOf[AtomishBoolean].value)) },
      ("Boolean", "or")       -> { thing => AlienProxy(booltobool(_ || thing.asInstanceOf[AtomishBoolean].value)) },
      ("Boolean", "not")      -> { thing => AlienProxy(a => AtomishBoolean(!thing.asInstanceOf[AtomishBoolean].value)) },
      ("Boolean", "isTrue")   -> { thing => AlienProxy(a => AtomishBoolean(thing.asInstanceOf[AtomishBoolean].value)) },
      ("Boolean", "isTruthy") -> { thing => AlienProxy(a => AtomishBoolean(thing.asInstanceOf[AtomishBoolean].value)) },
      ("Boolean", "isFalse")  -> { thing => AlienProxy(a => AtomishBoolean(!thing.asInstanceOf[AtomishBoolean].value)) },
      ("Boolean", "isFalsy")  -> { thing => AlienProxy(a => AtomishBoolean(!thing.asInstanceOf[AtomishBoolean].value)) },
      ("Boolean", "asText")   -> { thing => AlienProxy(a => AtomishString(thing.asInstanceOf[AtomishBoolean].value.toString())) },
      ("Array", "each")       -> { thing => QAlienProxy(_.args match {
        case Array(message) => {
          thing.asInstanceOf[AtomishArray].value.foreach(inner =>
              u.roots("eval").asInstanceOf[AlienProxy].activate(AtomishArgs(List(Left(message), Left(inner))))
          )
          AtomishUnset
        }
        case Array(AtomishMessage(variable), code) => {
          thing.asInstanceOf[AtomishArray].value.foreach(inner => {
              u.scopes = MMap(variable -> inner) +: u.scopes;
              u.roots("eval").asInstanceOf[AlienProxy].activate(AtomishArgs(List(Left(code))))
              var sco = u.scopes.tail;
              u.scopes = sco
          })
          AtomishUnset
        }
      }) },
      ("Array", "map")       -> { thing => QAlienProxy(_.args match {
        case Array(message) => {
          AtomishArray(thing.asInstanceOf[AtomishArray].value.map(inner =>
              u.roots("eval").asInstanceOf[AlienProxy].activate(AtomishArgs(List(Left(message), Left(inner))))
          ))
        }
        case Array(AtomishMessage(variable), code) => {
          AtomishArray(thing.asInstanceOf[AtomishArray].value.map(inner => {
              u.scopes = MMap(variable -> inner) +: u.scopes;
              var ret = u.roots("eval").asInstanceOf[AlienProxy].activate(AtomishArgs(List(Left(code))))
              var sco = u.scopes.tail;
              u.scopes = sco
              ret
          }))
        }
      }) },
      ("Array", "flatMap")       -> { thing => QAlienProxy(_.args match {
        case Array(message) => {
          AtomishArray(thing.asInstanceOf[AtomishArray].value.flatMap(inner =>
              u.roots("eval").asInstanceOf[AlienProxy].activate(AtomishArgs(List(Left(message),
                Left(inner)))).asInstanceOf[AtomishArray].value
          ))
        }
        case Array(AtomishMessage(variable), code) => {
          AtomishArray(thing.asInstanceOf[AtomishArray].value.flatMap(inner => {
              u.scopes = MMap(variable -> inner) +: u.scopes;
              var ret = u.roots("eval").asInstanceOf[AlienProxy].activate(AtomishArgs(List(Left(code)))).asInstanceOf[AtomishArray].value
              var sco = u.scopes.tail;
              u.scopes = sco
              ret
          }))
        }
      }) },
      ("Array", "filter")       -> { thing => QAlienProxy(_.args match {
        case Array() => {
          AtomishArray(thing.asInstanceOf[AtomishArray].value.filter(x => x != AtomishUnset))
        }
        case Array(message) => {
          AtomishArray(thing.asInstanceOf[AtomishArray].value.filter(inner => {
              var ret = u.roots("eval").asInstanceOf[AlienProxy].activate(AtomishArgs(List(Left(message),
                Left(inner))))
              if(ret.isInstanceOf[AtomishBoolean]) {
                ret.asInstanceOf[AtomishBoolean].value
              } else {
                ((ret.cells.isDefinedAt("isTruthy") &&
                 (u.roots("eval").asInstanceOf[AlienProxy].activate(AtomishArgs(List(Left(ret.cells("isTruthy"))))) == AtomishBoolean(true))) ||
                 (ret.cells.isDefinedAt("asBool") &&
                 (u.roots("eval").asInstanceOf[AlienProxy].activate(AtomishArgs(List(Left(ret.cells("asBool"))))) == AtomishBoolean(true))))
              }
          }))
        }
        case Array(AtomishMessage(variable), code) => {
          AtomishArray(thing.asInstanceOf[AtomishArray].value.filter(inner => {
              u.scopes = MMap(variable -> inner) +: u.scopes;
              var ret = u.roots("eval").asInstanceOf[AlienProxy].activate(AtomishArgs(List(Left(code))))
              var sco = u.scopes.tail;
              u.scopes = sco
              if(ret.isInstanceOf[AtomishBoolean]) {
                ret.asInstanceOf[AtomishBoolean].value
              } else {
                ((ret.cells.isDefinedAt("isTruthy") &&
                 (u.roots("eval").asInstanceOf[AlienProxy].activate(AtomishArgs(List(Left(ret.cells("isTruthy"))))) == AtomishBoolean(true))) ||
                 (ret.cells.isDefinedAt("asBool") &&
                 (u.roots("eval").asInstanceOf[AlienProxy].activate(AtomishArgs(List(Left(ret.cells("asBool"))))) == AtomishBoolean(true))))
              }
          }))
        }
      }) },
      ("Origin", "=")        -> { thing => QAlienProxy(_.args match {
        case Array(AtomishMessage(cell_name), x) => {
          var ret = u.roots("eval").asInstanceOf[AlienProxy].activate(AtomishArgs(List(Left(x))))
          thing.cells(cell_name) = ret
          ret
        }
      }) }
    )
    var libs_dir: String = System.getProperty("atomish.lib", ".")
    var prelude_source = new BufferedSource(new FileInputStream(new File(libs_dir, "prelude.atomish")))
    var prelude = AtomishString(prelude_source.addString(new StringBuilder(1024)).toString())
    e(r.read(prelude), None)
    var stream_source = new BufferedSource(new FileInputStream(file_source))
    var src = AtomishString(stream_source.addString(new StringBuilder(1024)).toString())
    //println(src)
    e(r.read(src), None);
  }
}
