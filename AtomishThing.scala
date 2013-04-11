package net.flaviusb.atomish

// AtomishThing is the supertype of all the container types for all 'things' that are reified in Atomish
trait AtomishThing

case class AtomishInt(value: Int) extends AtomishThing with AtomishCode

case class AtomishDecimal(value: Double) extends AtomishThing with AtomishCode

case class AtomishMessage(name: String) extends AtomishThing with AtomishCode

case class AtomishCommated(args: Array[AtomishCode])

case class AtomishCall(name: String, args: Array[AtomishCode])

case class MessageChain(messages: Array[AtomishMessage])

trait AtomishCode
