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
    var source = args(0);
    u.roots("System") = AtomishOrigin(MMap[String, AtomishThing](
      "programArguments" -> AtomishArray(args.drop(1).map(x => AtomishString(x)))
    ))
    var src_file = new BufferedSource(new FileInputStream(new File(source)))
    var src = AtomishString(src_file.addString(new StringBuilder(1024)).toString())
    //println(src)
    e(r.read(src), None);
  }
}
