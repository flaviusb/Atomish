package net.flaviusb.atomish

import scala.collection.mutable.{Map => MMap}

object PreScalaPrinter {
  def print(thing: AtomishThing, print_form_explicit: Boolean = false): String = {
    return thing match {
      case AtomishArray(arr)       => {
        "AtomishArray(Array(" + arr.map(x => print(x, print_form_explicit)).mkString(", ") + "))"
      }
      case AtomishMap(amap)        => {
        "AtomishMap(MutableMap(" + amap.map(
          x => print(x._1, print_form_explicit) + " -> " + print(x._2, print_form_explicit)
        ).mkString(", ") + "))"
      }
      case AtomishCall(call, args) => {
        "AtomishCall(" + call + ", " + args.map(x => print(x, print_form_explicit)).mkString(", ") + ")"
      }
      case AtomishCommated(tuple)  => "AtomishCommated(" + tuple.map(x => print(x, print_form_explicit)).mkString(", ") + ")"
      case AtomishForm(x)          => {
        (if(print_form_explicit) {"AtomishForm("} else {""}) + x.map(part => print(part, print_form_explicit)).mkString(" ") + (if(print_form_explicit) {")"} else {""})
      }
      case AtomishNL               => " . "
      case AtomishOrigin(cells)    => {
        "AtomishOrigin(MutableMap(" + cells.map(
          x => x._1 + " -> " + print(x._2, print_form_explicit)
        ).mkString(", ") + "))"
      }
      case x: AtomishThing         => x.toString()
    }
  }
  def print_with_forms = print(_: AtomishThing, true)
}
