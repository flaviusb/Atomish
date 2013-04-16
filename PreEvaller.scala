package net.flaviusb.atomish

object PreEvaller {
  def eval(universe: PreUniverse)(ast: AtomishCode): AtomishThing = {
    ast match {
      // Do message lookup first, left to right, minding activatability
      case (msg: AtomishMessage) => {
        universe(AtomishPlace(msg)) match {
          case Some(subject) => {
            if(subject(AtomishString("activatable")) == AtomishBoolean(true)) {
              subject match {
                case (proxy: AlienProxy) => proxy.activate(AtomishArgs(List()))
              }
            } else {
              subject
            }
          }
        }
      }
      // Last we fall through all the 'self evaluating' types
      case AtomishInt(x)     => AtomishInt(x)
      case AtomishDecimal(x) => AtomishDecimal(x)
      case AtomishString(x)  => AtomishString(x)
    }
  }
}
