package net.flaviusb.atomish

import scala.collection.mutable.{Map => MMap}

object PreScalaPrinter {
  def print(thing: AtomishThing): String = {
    return thing match {
      case AtomishArray(arr) => {
        "AtomishArray(Array(" + arr.map(x => print(x)).mkString(",") + "))"
      }
      case AtomishMap(amap)  => {
        "AtomishMap(MutableMap(" + amap.map(
          x => print(x._1) + " -> " + print(x._2)
        ).mkString(",") + "))"
      }
      case x: AtomishThing   => x.toString()
    }
  }
}
