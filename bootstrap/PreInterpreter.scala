package net.flaviusb.atomish

import java.io.{File, FileInputStream}
import scala.io.{BufferedSource}

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
  def main(args: Array[String]): Unit = {
    var src_file = new BufferedSource(new FileInputStream(new File(args(0))))
    var src = AtomishString(src_file.addString(new StringBuilder(1024)).toString())
    //println(src)
    e(r.read(src), None);
  }
}
