package net.flaviusb.atomish

import scala.collection.mutable.{Map => MMap}

// AtomishThing is the supertype of all the container types for all 'things' that are reified in Atomish
trait AtomishThing {
  var cells: MMap[String, AtomishThing] = MMap(
    "hasCell" -> AlienProxy[AtomishString, AtomishBoolean](x => AtomishBoolean(cells.isDefinedAt(x.value))),
    "cell" -> AlienProxy[AtomishString, AtomishThing](x => cells(x.value)),
    "activatable" -> AtomishBoolean(false)
  )
}

case class AtomishBoolean(value: Boolean) extends AtomishThing with AtomishCode {
}

case class AtomishInt(value: Int) extends AtomishThing with AtomishCode {
  cells ++= MMap[String, AtomishThing](
    "+" -> AlienProxy[AtomishInt, AtomishInt](x => AtomishInt(x.value + value)),
    "-" -> AlienProxy[AtomishInt, AtomishInt](x => AtomishInt(x.value - value)),
    "×" -> AlienProxy[AtomishInt, AtomishInt](x => AtomishInt(x.value * value)),
    "÷" -> AlienProxy[AtomishInt, AtomishInt](x => AtomishInt(x.value / value))
  )
}

case class AtomishDecimal(value: Double) extends AtomishThing with AtomishCode {
  cells ++= MMap[String, AtomishThing](
    "+" -> AlienProxy[AtomishDecimal, AtomishDecimal](x => AtomishDecimal(x.value + value)),
    "-" -> AlienProxy[AtomishDecimal, AtomishDecimal](x => AtomishDecimal(x.value - value)),
    "×" -> AlienProxy[AtomishDecimal, AtomishDecimal](x => AtomishDecimal(x.value * value)),
    "÷" -> AlienProxy[AtomishDecimal, AtomishDecimal](x => AtomishDecimal(x.value / value))
 )
}

case class AtomishString(value: String) extends AtomishThing with AtomishCode

case class AtomishMessage(name: String) extends AtomishThing with AtomishCode

case class AtomishCommated(args: Array[AtomishCode]) extends AtomishThing with AtomishCode

case class AtomishCall(name: String, args: Array[AtomishCode]) extends AtomishThing with AtomishCode

case class MessageChain(messages: Array[AtomishMessage]) extends AtomishThing with AtomishCode

trait AtomishCode

case class AlienProxy[A, B](call: A => B) extends AtomishThing {
  cells("activatable") = AtomishBoolean(true)
}
