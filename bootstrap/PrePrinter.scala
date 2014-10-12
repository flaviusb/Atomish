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
      case AtomishFnPre(code, args, activatable, documentation) => {
        "AtomishFn(" + print(code, print_form_explicit) + ", " +
        print(args, print_form_explicit) + ", " + print(activatable, print_form_explicit) + (if(documentation == None) { "" } else { ", " +
        print(documentation.get, print_form_explicit)})
      }
      case x: AtomishThing         => x.toString()
    }
  }
  def print_with_forms = print(_: AtomishThing, true)
  def atomish_qstring_escape(str: String): String = {
    str.flatMap {
      case '\n' => "\\n"
      case '\r' => "\\r"
      case '\\' => "\\\\"
      case '"'  => "\\\""
      case x    => s"$x"
    }
  }
  def print_atomish(thing: AtomishThing): String = {
    return thing match {
      case AtomishArray(arr)       => {
        "[" + arr.map(x => print_atomish(x)).mkString(", ") + "]"
      }
      case AtomishMap(amap)        => {
        "{" + amap.map(
          x => print_atomish(x._1) + " ⇒ " + print_atomish(x._2)
        ).mkString(", ") + "}"
      }
      case AtomishCall(call, args) => {
        call + "(" + args.map(x => print_atomish(x)).mkString(", ") + ")"
      }
      case AtomishCommated(tuple)  => "(" + tuple.map(x => print_atomish(x)).mkString(", ") + ")"
      case AtomishForm(x)          => {
        x.map(part => print_atomish(part)).mkString(" ")
      }
      case AtomishNL               => " . "
      case AtomishOrigin(cells)    => {
        "Origin with(" + cells.map(
          x => x._1 + ":  " + print_atomish(x._2)
        ).mkString(", ") + ")"
      }
      case AtomishFnPre(code, AtomishArray(args), AtomishBoolean(activatable), documentation) => {
        (if(activatable) {"fn("} else {"fnx("}) + (if(documentation == None) { "" } else {
          print_atomish(documentation.get) + ", "}) + args.map(arg => print_atomish(arg)).mkString(", ") + ", " +
        print_atomish(code)
      }
      case AtomishString(str) => {
        "\"" + atomish_qstring_escape(str) + "\""
      }
      case AtomishBoolean(bool) => {
        bool.toString()
      }
      case x: AtomishThing         => x.toString()
    }
  }
}
