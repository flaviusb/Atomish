package net.flaviusb.atomish

class PreUniverse {
  def apply(key: AtomishPlace): Option[AtomishThing] = {
    key.form match {
      case message_chain: MessageChain => {
        None // Replace this
      }
      case _ => None
    }
  }
  def update(key: AtomishPlace, value: Option[AtomishThing]) = { }
}
