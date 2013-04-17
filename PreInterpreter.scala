package net.flaviusb.atomish

import java.io.{File, FileInputStream}
import scala.io.{BufferedSource}

object PreAtomishInterpreter {
  var u = new PreUniverse()
  var r = new PreReader()
  var e = PreEvaller.eval _;
  def main(args: Array[String]): Unit = {
    var src_file = new BufferedSource(new FileInputStream(new File(args(0))))
    var src = AtomishString(src_file.addString(new StringBuilder(1024)).toString())
    e(u)(r.read(src));
  }
}
