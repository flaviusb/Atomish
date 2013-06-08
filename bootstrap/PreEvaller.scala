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
      //case AtomishCall(call, args) => AtomishCall(call, args.flatMap(_ match {
      //  case (rem: AtomishComment)   => Array[AtomishCode]()
      //  case x                       => uncomment(x).toArray[AtomishCode]
      //}))
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
    val operator_match: String = "^([~!@$%^&*_=\'`/?×÷≠→←⇒⇐⧺⧻§∘≢∨∪∩□∀⊃∈+<>-]+|…)$"
    val infix = Set("⇒", "=") // TODO: Move this into the MOP inside the Universe
    def shuffle_limb(code: Stack[AtomishCode]): Stack[AtomishCode] = {
      // For now, we just rewrite bare '=' messages into ternary ie '(a s d x = y b c) → '(a s d =(x, y b c))
      // All other bare symbol message we rewrite into binary ie '(a + (b × c) - e ÷ f) → '(a +(b ×(c)) -(e ÷(f)))
      var message_frag:  Stack[AtomishCode] = Stack()
      var the_code = code;
      while(the_code.length > 0) {
        var code_bit = the_code.pop;
        code_bit match {
          case AtomishMessage(name) => {
            // First we deal with the 'trinary' case
            if(infix.contains(name)) {
              // There must be something on the lhs
              if(message_frag.length == 0) {
                null // Should error
              } else {
                var lhs = message_frag.pop;
                var code2 = shuffle_limb(the_code)
                message_frag.push(AtomishCall(name, Array(lhs, AtomishForm(code2.toList))))
                //println(PreScalaPrinter.print_with_forms(AtomishCall("=", Array(AtomishForm(code2.toList)))))
                the_code = Stack()
              }
            } else if (name.matches(operator_match)) {
              // Shuffle binary operators
              var code2 = shuffle_limb(the_code)
              message_frag.push(AtomishCall(name, code2.toArray))
              the_code = Stack()
            } else {
              message_frag.push(AtomishMessage(name))
            }
          }
          case other => message_frag.push(other)
        }
      }
      message_frag.reverse
    }
    def shuffle(code: AtomishCode): AtomishCode = code match {
      //case AtomishCall(call, args) => AtomishCall(call, args.map(arg => shuffle(arg)))
      //case AtomishCommated(args)   => AtomishCommated(args.map(arg => shuffle(arg)))
      case AtomishForm(unshuffled) => {
        var message_chain: Stack[Stack[AtomishCode]] = Stack()
        var message_limb:  Stack[AtomishCode] = Stack()
        unshuffled.foreach(code_bit => {
          if(code_bit == AtomishNL) {
            if(message_limb.length > 0) {
              message_chain push(shuffle_limb(message_limb.reverse))
              message_limb = Stack()
            }
            message_chain push(Stack(AtomishNL))
          } else {
            message_limb push(code_bit)
          }
        })
        if(message_limb.length > 0) {
          message_chain push(shuffle_limb(message_limb.reverse))
        }
        var ret = AtomishForm(message_chain.reverse.flatten.toList)
        //println("Unshuffled: ")
        //println(PreScalaPrinter.print_with_forms(AtomishForm(unshuffled)))
        //println("Shuffled: ")
        //println(PreScalaPrinter.print_with_forms(ret))
        ret
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
    eval_canonical(universe)(canonical, ground)
  }
  def parse_args(universe: PreUniverse, msg: AtomishCall): List[Either[AtomishThing, (String, AtomishThing)]] = {
    msg.args.map(all => all match {
      case AtomishForm(thing) => {
        val chomped = thing dropWhile(_ == AtomishNL)
        if(chomped.length < 2) {
          Left(eval(universe)(AtomishForm(chomped)))
        } else {
          val (arg::rest) = chomped;
          val kwregex = "^([a-zA-Z0-9_!:?]+):$".r
          if(arg.isInstanceOf[AtomishMessage]) {
            arg.asInstanceOf[AtomishMessage].name match {
              case kwregex(key) => Right(key, eval(universe)(AtomishForm(rest)))
              case _            => Left(eval(universe)(all))
            }
          } else {
            Left(eval(universe)(all))
          }
        }
      }
      case AtomishCommated(args) => {
        throw new scala.MatchError("Match error - did not expect commated here - " + PreScalaPrinter.print_with_forms(AtomishCommated(args)))
      }
      case _ => {
        Left(eval(universe)(all))
      }
    }).toList
  }

  def eval_canonical(universe: PreUniverse)(canonical: AtomishCode, ground: Option[AtomishThing] = None): AtomishThing = {
    val relground = relgrounder(universe, ground) _
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
                var parsed_args = parse_args(universe, msg)
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
                var parsed_args = parse_args(universe, msg)
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
                    var parsed_args = parse_args(universe, msg)
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
              eval_canonical(universe)(AtomishForm(rest), None)
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
          eval_canonical(universe)(AtomishForm(rest), Some(base))
        } else {
          base
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
      case AtomishNL           => AtomishNL
    }
  }
}
