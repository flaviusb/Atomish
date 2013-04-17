package net.flaviusb.atomish

object PreEvaller {
  def eval(universe: PreUniverse)(ast: AtomishCode): AtomishThing = {
    ast match {
      // Do message lookup first, left to right, minding activatability
      case (msg: AtomishMessage)   => {
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
      case (msg: AtomishCall)      => {
        universe(AtomishPlace(msg)) match {
          case Some(subject) => {
            subject match {
              case (proxy: AlienProxy) => {
                // Evaluate the arguments and pack the results into an argument list
                // For now we just deal with plain positional arguments
                var parsed_args = msg.args.map(arg => Left(eval(universe)(arg))).toList;
                proxy.activate(AtomishArgs(parsed_args))
              }
            }
          }
        }
      }
      // Last we fall through all the 'self evaluating' types
      case AtomishInt(x)           => AtomishInt(x)
      case AtomishDecimal(x)       => AtomishDecimal(x)
      case AtomishString(x)        => AtomishString(x)
    }
  }
}
