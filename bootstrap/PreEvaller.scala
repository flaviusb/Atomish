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
    // Remove comment nodes
    def uncomment(code: AtomishCode): Option[AtomishCode] = Option(code match {
      case (rem: AtomishComment)   => null
      case AtomishCall(call, args) => AtomishCall(call, args.flatMap(_ match {
        case (rem: AtomishComment)   => Array[AtomishCode]()
        case x                       => uncomment(x).toArray[AtomishCode]
      }))
      case AtomishForm(x)          => AtomishForm(x.flatMap(xx => uncomment(xx)))
      case x                       => x
    })
    def unpick(code: AtomishCode): Array[AtomishCode] = code match {
      case AtomishForm(x) => x.toArray[AtomishCode]
      case x              => Array[AtomishCode](x)
    }
    def stitch(code: AtomishCode): AtomishCode = code match {
      case AtomishCall(call, args) => AtomishCall(call, args.map(_ match {
        case AtomishForm(x)   => AtomishForm(x.flatMap(unpick _).map(stitch _))
        case (x: AtomishCode) => stitch(x)
      }))
      case AtomishForm(x)          => AtomishForm(x.flatMap(unpick _).map(stitch _))
      case x                       => x
    }
    import scala.collection.mutable.Stack;
    def shuffle_limb(code: Stack[AtomishCode]): Stack[AtomishCode] = {
      // For now, we just rewrite bare '=' messages into ternary ie '(a s d x = y b c) → '(a s d =(x, y b c))
      // All other bare symbol message we rewrite into binary ie '(a + (b × c) - e ÷ f) → '(a +(b ×(c)) -(e ÷(f)))
      var message_chain: Stack[AtomishCode] = Stack()
      var message_frag:  Stack[AtomishCode] = Stack()
      code.foreach(code_bit => {
        code_bit 
      })
      code
    }
    def shuffle(code: AtomishCode): AtomishCode = code match {
      //case AtomishCall(call, args) => AtomishCall(call, args.map(arg => shuffle(arg)))
      //case AtomishCommated(args)   => AtomishCommated(args.map(arg => shuffle(arg)))
      case AtomishForm(unshuffled) => {
        var message_chain: Stack[AtomishCode] = Stack()
        var message_limb:  Stack[AtomishCode] = Stack()
        unshuffled.foreach(code_bit => {
          if(code_bit == AtomishNL) {
            if(message_limb.length > 0) {
              message_chain pushAll(shuffle_limb(message_limb))
              message_limb = Stack()
            }
            message_chain push(AtomishNL)
          } else {
            message_limb push(code_bit)
          }
        })
        if(message_limb.length > 0) {
          message_chain pushAll(shuffle_limb(message_limb))
        }
        var ret = AtomishForm(message_chain.toList)
        //println("Unshuffled: ")
        //println(PreScalaPrinter.print_with_forms(AtomishForm(unshuffled)))
        //println("Shuffled: ")
        //println(PreScalaPrinter.print_with_forms(ret))
        ret
        code
      }
      case other          => other
    }
    var canonical = shuffle(uncomment(ast).map(stitch _).getOrElse(AtomishNL))
    //println("Original AST:")
    //println(PreScalaPrinter.print_with_forms(ast))
    //println("Canonicalised AST:")
    //println(PreScalaPrinter.print_with_forms(canonical))
    //println("Ground:")
    //println(ground)
    canonical match {
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
          case None => {
            println("case None, for message: " + msg.toString())
            throw new scala.MatchError(None)
          }
        }
      }
      case (msg: AtomishCall)      => {
        relground(AtomishPlace(msg)) match {
          case Some(subject) => {
            subject match {
              case (proxy: AlienProxy)  => {
                // Evaluate the arguments and pack the results into an argument list
                // For now we just deal with plain positional arguments
                var parsed_args = msg.args.map(arg => Left(eval(universe)(arg))).toList;
                proxy.activate(AtomishArgs(parsed_args))
              }
              case (proxy: QAlienProxy) => {
                // Args remain quoted
                proxy.activate(AtomishCommated(msg.args))
              }
            }
          }
          case None => {
            println("case None, for message: " + msg.toString())
            throw new scala.MatchError(None)
          }
        }
      }
      case AtomishForm(List(msg: AtomishMessage))   => {
        relground(AtomishPlace(msg)) match {
          case Some(subject) => {
            if(subject(AtomishString("activatable")) == AtomishBoolean(true)) {
              subject match {
                case (proxy: AlienProxy)  => proxy.activate(AtomishArgs(List()))
                case (proxy: QAlienProxy) => proxy.activate(AtomishCommated(Array()))
              }
            } else {
              subject
            }
          }
          case None => {
            println("case None, for message: " + msg.toString())
            throw new scala.MatchError(None)
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
              case (proxy: QAlienProxy) => {
                // Args remain quoted
                proxy.activate(AtomishCommated(msg.args))
              }
            }
          }
          case None => {
            println("case None, for message: " + msg.toString())
            throw new scala.MatchError(None)
          }
        }
      }
      case AtomishForm(head::rest) => {
        var nl_val = false
        var base: AtomishThing = head match {
          // Do message lookup first, left to right, minding activatability
          case (msg: AtomishMessage)   => {
            relground(AtomishPlace(msg)) match {
              case Some(subject) => {
                if(subject(AtomishString("activatable")) == AtomishBoolean(true)) {
                  subject match {
                    case (proxy: AlienProxy)  => proxy.activate(AtomishArgs(List()))
                    case (proxy: QAlienProxy) => proxy.activate(AtomishCommated(Array()))
                  }
                } else {
                  subject
                }
              }
              case None => {
                println("case None, for message: " + msg.toString())
                throw new scala.MatchError(None)
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
                  case (proxy: QAlienProxy) => {
                    // Args remain quoted
                    proxy.activate(AtomishCommated(msg.args))
                  }
                }
              }
              case None => {
                println("case None, for message: " + msg.toString())
                throw new scala.MatchError(None)
              }
            }
          } 
          // Last we fall through all the 'self evaluating' types
          case (x: IdempotentEval) => x
          case AtomishInterpolatedString(chunks) => {
            AtomishString(chunks.map(_ match {
              case AtomishString(x) => x
              case x: AtomishCode   => (eval(universe)(x, ground) match {
                case AtomishString(y)  => y
                case AtomishInt(y)     => y.toString()
                case AtomishDecimal(y) => y.toString()
                case z                 => z.toString()
              })
            }).mkString)
          }
          case AtomishCommated(x: Array[AtomishCode]) => {
            // Send as activation to ground if ground exists
            // Otherwise, if the commated only has one arg, 
            // we treat as possibly a single value term in an expression
            ground match {
              case Some(existing_ground) => {
                existing_ground match {
                  case prx: AlienProxy => prx.activate(AtomishArgs(x.map(q => Left(eval(universe)(q))).toList))
                  case (proxy: QAlienProxy) => {
                    // Args remain quoted
                    proxy.activate(AtomishCommated(x))
                  }
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
              nl_val = true
              eval(universe)(AtomishForm(rest), None)
            } else {
              ground match {
                case Some(gr) => {
                  nl_val = true
                  gr
                }
                case None     => AtomishNL
              }
            }
          }
        }
        if((rest != List()) && (base != AtomishNL) && (!nl_val)) {
          eval(universe)(AtomishForm(rest), Some(base))
        } else {
          base
        }
      }
      // Last we fall through all the 'self evaluating' types
      case (x: IdempotentEval) => x
      case AtomishInterpolatedString(chunks) => {
        AtomishString(chunks.map(_ match {
          case x: AtomishString => x
          case x: AtomishCode   => eval(universe)(x, ground)
        }).mkString)
      }
      case AtomishNL           => AtomishNL
    }
  }
}
