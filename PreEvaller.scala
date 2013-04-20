package net.flaviusb.atomish

object PreEvaller {
  def relgrounder(universe: PreUniverse, ground: Option[AtomishThing])(key: AtomishPlace): Option[AtomishThing] = ground match {
    case Some(thing) => {
      //println(thing)
      Some(thing(key.form match {
        case x: AtomishString  => x
        case AtomishMessage(x) => AtomishString(x)
        case AtomishCall(x, _) => AtomishString(x)
      }))
    }
    case None        => universe(key)
  }
  def eval(universe: PreUniverse)(ast: AtomishCode, ground: Option[AtomishThing] = None): AtomishThing = {
    val relground = relgrounder(universe, ground) _
    //println(ast)
    //println(ground)
    ast match {
      // Do message lookup first, left to right, minding activatability
      case (msg: AtomishMessage)   => {
        relground(AtomishPlace(msg)) match {
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
        relground(AtomishPlace(msg)) match {
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
      case AtomishForm(List(msg: AtomishMessage))   => {
        relground(AtomishPlace(msg)) match {
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
      case AtomishForm(List(msg: AtomishCall))      => {
        relground(AtomishPlace(msg)) match {
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
      case AtomishForm(head::rest) => {
        var base: AtomishThing = head match {
          // Do message lookup first, left to right, minding activatability
          case (msg: AtomishMessage)   => {
            relground(AtomishPlace(msg)) match {
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
            relground(AtomishPlace(msg)) match {
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
          case AtomishCommated(x: Array[AtomishCode]) => {
            // Send as activation to ground if ground exists
            // Otherwise, if the commated only has one arg, 
            // we treat as possibly singal value term in an expression
            ground match {
              case Some(existing_ground) => {
                existing_ground match {
                  case prx: AlienProxy => prx.activate(AtomishArgs(x.map(q => Left(q)).toList))
                }
              }
              case None    => {
                // Attempt to treat as a single value
                if(x.length == 1) {
                  eval(universe)(x(0))
                } else {
                  null // should error
                }
              }
            }
          }
          case AtomishNL               => {
            // This handles continuing subforms
            // As recursive evalling should always be the last thing we do, and side effects are packed away
            // in Scala stack frames as such, we can simply start a new eval chain if rest is not empty
            if (rest != List()) {
              eval(universe)(AtomishForm(rest), None)
            } else {
              AtomishNL
            }
          }
        }
        if((rest != List()) && (base != AtomishNL)) {
          eval(universe)(AtomishForm(rest), Some(base))
        } else {
          base
        }
      }
      // Last we fall through all the 'self evaluating' types
      case AtomishInt(x)           => AtomishInt(x)
      case AtomishDecimal(x)       => AtomishDecimal(x)
      case AtomishString(x)        => AtomishString(x)
    }
  }
}
