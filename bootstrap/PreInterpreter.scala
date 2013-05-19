package net.flaviusb.atomish

import java.io.{File, FileInputStream}
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
        case _                                    => AtomishUnset //Should soft error
      }),
      "removeFile!"      -> AlienProxy(_.args match {
        case List(Left(AtomishString(file_name))) => AtomishBoolean((new File(u.roots("FileSystem").cells("cwd").asInstanceOf[AtomishString].value, file_name)).delete())
      }),
      "parentOf"         -> AlienProxy(_.args match {
        case List(Left(AtomishString(file_name))) => AtomishString((new File(u.roots("FileSystem").cells("cwd").asInstanceOf[AtomishString].value, file_name)).getParent())
      })
    ))
    var stream_source = new BufferedSource(new FileInputStream(file_source))
    var src = AtomishString(stream_source.addString(new StringBuilder(1024)).toString())
    //println(src)
    e(r.read(src), None);
  }
}
