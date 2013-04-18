package net.flaviusb.atomish

import java.io.{File, FileInputStream}
import scala.io.{BufferedSource}

object PreAtomishInterpreter {
  var u = new PreUniverse()
  var r = new PreReader()
  var e = PreEvaller.eval _;
  def main(args: Array[String]): Unit = {
    println(args)
    var src_file = new BufferedSource(new FileInputStream(new File(args(0))))
    var src = AtomishString(src_file.addString(new StringBuilder(1024)).toString())
    println(src)
    e(u)(r.read(src), None);
  }
}
