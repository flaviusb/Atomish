package net.flaviusb.atomish

object PreEvaller {
  def eval(universe: PreUniverse)(ast: AtomishCode): AtomishThing = {
    ast match {
      // Do message lookup first, left to right
      case (msg: AtomishMessage) => universe(AtomishPlace(msg)).get
      // Last we fall through all the 'self evaluating' types
      case AtomishInt(x)     => AtomishInt(x)
      case AtomishDecimal(x) => AtomishDecimal(x)
      case AtomishString(x)  => AtomishString(x)
    }
  }
}
