package net.flaviusb.atomish

import scala.collection.mutable.{Map => MMap}

// AtomishThing is the supertype of all the container types for all 'things' that are reified in Atomish
trait AtomishThing {
  var cells: MMap[String, AtomishThing] = MMap()
}

object AtomishThing {
  var bootstrap_cells: MMap[String, AtomishThing => AtomishThing] = MMap(
    "hasCell" -> { thing => AlienProxy[AtomishString, AtomishBoolean](x => {
      // A thing can have a cell value of AtomishUnset, or it can have a cell value that is not AtomishUnset,  or if it does not have a
      // value the bootstrap can have a value, or it has no value, considered in that order.
      if(thing.cells.isDefinedAt(x.value)) {
        if(thing.cells(x.value) == AtomishUnset) {
          AtomishBoolean(false)
        } else {
        AtomishBoolean(true)
        }
      } else {
        AtomishBoolean(AtomishThing.bootstrap_cells.isDefinedAt(x.value))
      }
    }) },
    "cell" -> { thing => AlienProxy[AtomishString, AtomishThing](x => {
      if(thing.cells.isDefinedAt(x.value)) {
        thing.cells(x.value)
      } else {
        AtomishThing.bootstrap_cells(x.value)(thing)
      }
    }) },
    "activatable" -> { thing => AtomishBoolean(false) }
  )
}

object AtomishUnset extends AtomishThing

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
