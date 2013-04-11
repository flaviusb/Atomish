package net.flaviusb.atomish

import scala.collection.mutable.{Map => MMap}

// AtomishThing is the supertype of all the container types for all 'things' that are reified in Atomish
trait AtomishThing {
  var cells: MMap[String, AtomishThing] = MMap(
  )
}

case class AtomishInt(value: Int) extends AtomishThing with AtomishCode

case class AtomishDecimal(value: Double) extends AtomishThing with AtomishCode

case class AtomishMessage(name: String) extends AtomishThing with AtomishCode

case class AtomishCommated(args: Array[AtomishCode]) extends AtomishThing with AtomishCode

case class AtomishCall(name: String, args: Array[AtomishCode]) extends AtomishThing with AtomishCode

case class MessageChain(messages: Array[AtomishMessage]) extends AtomishThing with AtomishCode

trait AtomishCode
