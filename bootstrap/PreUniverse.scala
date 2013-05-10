package net.flaviusb.atomish

import scala.collection.mutable.{Map => MMap, MutableList => MList}

class PreUniverse { self =>
  var gensyms: MMap[Int, AtomishThing] = MMap[Int, AtomishThing]()
  var currgs: Int = 1
  var scopes: MList[MMap[String, AtomishThing]] = MList() // New scopes go on the front of the list
  var roots: MMap[String, AtomishThing] = MMap[String, AtomishThing](
    "version" -> AtomishDecimal(0.1),
    "say"     -> AlienProxy(_.args match {
      case List(Left(AtomishString(x))) => {
        println(x)
        AtomishUnset
      }
      case x                            => {
        println(x.toString())
        AtomishUnset
      }
    }),
    "setCell"   -> AlienProxy(_.args match {
      case List(Left(AtomishString(name)), Left(value: AtomishThing)) => {
        self(AtomishPlace(AtomishMessage(name))) = Option(value)
        value
      }
    }),
    "cell"   -> AlienProxy(_.args match {
      case List(Left(AtomishString(name))) => {
        self(AtomishPlace(AtomishMessage(name))).get
      }
    }),
    "hasCell"   -> AlienProxy(_.args match {
      case List(Left(AtomishString(name))) => {
        val thing = self(AtomishPlace(AtomishMessage(name)))
        AtomishBoolean((thing != None) && (thing != Some(AtomishUnset)))
      }
    }),
    "let"       -> QAlienProxy(ctd => {
      var args = (ctd.args grouped(2) filter(_.length == 2) map((x: Array[AtomishCode]) => (x(0).asInstanceOf[AtomishForm].things(0).asInstanceOf[AtomishMessage].name,
        self.roots("eval").asInstanceOf[AlienProxy].activate(AtomishArgs(List(Left(x(1)))))))).toMap;
      var arrcode: Array[AtomishCode] = ((ctd.args grouped(2) filter(_.length == 1) flatMap((x: Array[AtomishCode]) => x)).toArray);
      var code = arrcode(0)
      scopes = (MMap() ++ args) +: scopes;
      //println(code.toString())
      var result = self.roots("eval").asInstanceOf[AlienProxy].activate(AtomishArgs(List(Left(code))));
      var sco = scopes.tail;
      scopes = sco;
      //println(result.toString())
      //AtomishUnset
      result
    }),
    "true"      -> AtomishBoolean(true),
    "false"     -> AtomishBoolean(false),
    "if"        -> QAlienProxy(ctd => {
      if(ctd.args.length == 0) {
        AtomishUnset
      } else {
        var test = self.roots("eval").asInstanceOf[AlienProxy].activate(AtomishArgs(List(Left(ctd.args(0)))))
        if (test == AtomishBoolean(true)) {
          if(ctd.args.length >= 2) {
            self.roots("eval").asInstanceOf[AlienProxy].activate(AtomishArgs(List(Left(ctd.args(1)))))
          } else {
            AtomishUnset
          }
        } else if((test == AtomishBoolean(false)) && (ctd.args.length >= 3)) {
          self.roots("eval").asInstanceOf[AlienProxy].activate(AtomishArgs(List(Left(ctd.args(2)))))
        } else {
          AtomishUnset
        }
      }
    }),
    "fn"        -> QAlienProxy(ctd => {
      if(ctd.args.length == 0) {
        AlienProxy(_ => AtomishUnset)
      } else if(ctd.args.length == 1) {
        AlienProxy(_ => self.roots("eval").asInstanceOf[AlienProxy].activate(AtomishArgs(List(Left(ctd.args(0))))))
      } else {
        // We have at least one arg and a body; that arg may be a docstring though
        var (docstring: Option[String], args: Array[AtomishCode], code: AtomishCode) = (ctd.args(0) match {
          case AtomishString(docstring) => {
            var args = ctd.args.drop(1).dropRight(1)
            var code = ctd.args.last
            (Some(docstring), args, code)
          }
          case _                        => {
            var args = ctd.args.dropRight(1)
            var code = ctd.args.last
            (None, args, code)
          }
        })
        val finargs: Array[AtomishCode] = args.flatMap(_ match {
          case (name: AtomishMessage) => Array[AtomishCode](name)
          case _                      => Array[AtomishCode]()
        })
        var fn = AlienProxy(x => {
          val let_args: Array[AtomishCode] = finargs.zip(x.args.flatMap(_ match {
            case Left(x: AtomishCode) => Array[AtomishCode](x) // This line is because we accept args which are atomishthings, but we can only construct calls which are atomishcodes
            case _                    => Array[AtomishCode]()
          })).flatMap((x: (AtomishCode, AtomishCode)) => Array[AtomishCode](x._1, x._2))
          val final_code = AtomishCall("let", let_args :+ code)
          self.roots("eval").asInstanceOf[AlienProxy].activate(AtomishArgs(List(Left(final_code))))
        })
        for(docs <- docstring) { fn.cells("docstring") = AtomishString(docs) }
        fn
      }
    }),
    "Array"     -> AlienProxy(arg_blob => AtomishArray(arg_blob.args.flatMap(_ match {
      case Left(x) => Array[AtomishThing](x)
      case _       => Array[AtomishThing]()
    }).toArray)),
    "Map"       -> AlienProxy(arg_blob => AtomishMap(arg_blob.args.map(_ match {
      case Right((x, y)) => MMap[AtomishThing, AtomishThing](AtomishString(x) -> y)
      case _             => MMap[AtomishThing, AtomishThing]()
    }).foldLeft(MMap[AtomishThing, AtomishThing]())(_ ++ _))),
    "primfn"    -> AlienProxy(_.args match {
      case List(Left(AtomishString(str))) => {
        new AtomishMacro(this,
          roots("read").asInstanceOf[AlienProxy].activate(AtomishArgs(List(Left(AtomishString(str))))).asInstanceOf[AtomishCode])
      }
      case _                              => null //boom
    })/*,
    "gensym"    -> AlienProxy(_.args match {
      case _ => {
        AtomishGenSym(currgs++)
      }
    })*/
  )
  def recapply(base: AtomishThing, path: Seq[AtomishMessage]): Option[AtomishThing] =  path match {
    case Seq(AtomishMessage(first), rest @ _*) => {
      base match {
        case thing: AtomishThing =>  {
          thing.cells.get(first) match {
            case Some(AtomishUnset) => None
            case Some(cell) => {
              if(rest.isEmpty) {
                Some(cell)
              } else {
                recapply(cell, rest)
              }
            }
            case None => {
              AtomishThing.bootstrap_cells.get(first) match {
                case Some(cell) => {
                  if(rest.isEmpty) {
                    Some(cell(base))
                  } else {
                    recapply(cell(base), rest)
                  }
                }
                case None => None
              }
            }
          }
        }
        case _                   => None
      }
    }
  }
  def apply(key: AtomishPlace): Option[AtomishThing] = {
    // Try from each scope backwards, or from root if all scopes fail
    for (base <- scopes) {
      var foo = apply_by_parts(key, base)
      if (foo != None) {
        return foo;
      }
    }
    return apply_by_parts(key, roots)
  }
  def apply_by_parts(key: AtomishPlace, base: MMap[String, AtomishThing]): Option[AtomishThing] = {
    key.form match {
      case AtomishMessage(name) => {
        return base.get(name)
      }
      case AtomishCall(name, _) => {
        return base.get(name)
      }
      //case AtomishForm(head :: rest) => {
      //  var root = roots.get(head)
      //}
      case MessageChain(Array(AtomishMessage(first), messages @ _*)) => {
        var root = base.get(first)
        root match {
          case Some(actual) => {
            if (messages.isEmpty) {
              return Some(actual)
            }
            return recapply(actual, messages)
          }
          case None => return None
        }
      }
      case _ => None
    }
  }
  def update(key: AtomishPlace, value: Option[AtomishThing]) {
    // Try from each scope backwards, or from root if all scopes fail
    for (base <- scopes) {
      var foo = apply_by_parts(key, base)
      if (foo != None) {
        update_internal(key, value, base)
        return;
      }
    }
    update_internal(key, value, roots)
  }
  def update_internal(key: AtomishPlace, value: Option[AtomishThing], base: MMap[String, AtomishThing]) {
    // For the moment, just deal with 'stringlike' keys
    var true_val = value match {
      case Some(x) => x
      case _       => AtomishUnset
    }
    key.form match {
      case AtomishMessage(name) => {
        base(name) = true_val
      }
      case AtomishCall(name, _) => {
        base(name) = true_val
      }
    }
  }
}
