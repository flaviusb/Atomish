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
      var args = (ctd.args grouped(2) filter(_.length == 2) map((x: Array[AtomishCode]) => (x(0).asInstanceOf[AtomishForm].things.filter(
        _ != AtomishNL)(0).asInstanceOf[AtomishMessage].name,
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
          case AtomishForm(List(AtomishInterpolatedString(chunks))) => {
            var docstring: String = chunks.map(_ match {
            case x: AtomishString => x
            case x: AtomishCode   => (self.roots("eval").asInstanceOf[AlienProxy].activate(AtomishArgs(List(Left(x)))) match {
                case y: AtomishString  => y
                case AtomishInt(y)     => y.toString()
                case AtomishDecimal(y) => y.toString()
                case z                 => z.toString()
              })}).mkString;
            var args = ctd.args.drop(1).dropRight(1)
            //println(args.toList)
            var code = ctd.args.last
            (Some(docstring), args, code)
          }
          case AtomishForm(List(AtomishString(docstring))) => {
            var args = ctd.args.drop(1).dropRight(1)
            //println(args.toList)
            var code = ctd.args.last
            (Some(docstring), args, code)
          }
          case _                        => {
            var args = ctd.args.dropRight(1)
            //println(args.toList)
            var code = ctd.args.last
            (None, args, code)
          }
        })
        // Separate arguments into kinds
        var (slurpy: Option[(String, Option[AtomishCode])], kwslurpy: Option[(String, Option[AtomishCode])]) = (None, None)
        val (finargs_a: Array[(String, (String, Option[AtomishCode]))], kwargs_a: Array[(String, (String, Option[AtomishCode]))]) = args.flatMap(_ match {
          case AtomishForm(List(AtomishMessage(name), rest @ _*)) => {
            if(name.startsWith("+:")) {
              kwslurpy = Some((name.substring(2, name.length), (if(rest.length == 0) { None } else { Some(AtomishForm(rest.toList)) } )))
              Array[(String, (String, Option[AtomishCode]))]()
            } else if(name.startsWith("+")) {
              slurpy = Some((name.substring(1, name.length), (if(rest.length == 0) { None } else { Some(AtomishForm(rest.toList)) } )))
              Array[(String, (String, Option[AtomishCode]))]()
            } else {
              Array[(String, (String, Option[AtomishCode]))](((if(name.endsWith(":")) { "kw" } else {"positional" }),
                ((if(name.endsWith(":")) { name.substring(0, name.length - 1) } else { name }), 
                (if(rest.length == 0) { None } else { Some(AtomishForm(rest.toList)) } ))))
            }
          }
          case _                                                 => Array[(String, (String, Option[AtomishCode]))]()
        }).partition(x => x._1 != "kw")
        val (finargs: Array[(String, Option[AtomishCode])], kwargs: Array[(String, Option[AtomishCode])]) = (finargs_a.map(x => x._2),
          kwargs_a.map(x => x._2))
        val needed_positional_args = finargs.count(x => x._2 == None)
        val needed_keyword_args    = kwargs.count(x => x._2 == None)
        var fn = AlienProxy(x => {
          //println(finargs.toList)
          //println(x.args)
          val (fpositional_a: Array[Either[AtomishThing, (String, AtomishThing)]], fkeyword_a: Array[Either[AtomishThing, (String, AtomishThing)]]) = x.args.toArray.partition(_ match {
            case _: Left[AtomishThing, (String, AtomishThing)]  => true
            case _: Right[AtomishThing, (String, AtomishThing)] => false
          })
          val (fpositional: Array[AtomishThing], fkeyword: Array[(String, AtomishThing)]) = (fpositional_a.map(_.left.get),
            fkeyword_a.map(_.right.get))
          if((fpositional.length < needed_positional_args) || (fkeyword.length < needed_keyword_args)) {
            println("Too few args.")
            println("Got "+fpositional.length.toString()+" positional, needed "+needed_positional_args.toString())
            println("Got "+fkeyword.length.toString()+" keyword, needed "+needed_keyword_args.toString())
            println(finargs.toList.toString())
            println(kwargs.toList.toString())
            null // Should raise a condition - too few arguments
          } else {
            val slurped_positional_args: Array[AtomishThing] = (if(slurpy != None) { fpositional.drop(finargs.length) } else { Array() })
            val slurped_keyword_args: Array[(AtomishThing, AtomishThing)] = (if(kwslurpy != None) { fkeyword.drop(kwargs.length) } else {
            Array() }).map(a => (AtomishString(a._1), a._2))
            val letified_args: Array[(String, AtomishThing)] = (fpositional.dropRight(slurped_positional_args.length).zip(finargs).map(a
              => (a._2._1, a._1)) ++ finargs.drop(fpositional.length).map(a => (a._1, a._2.get)) ++ fkeyword.dropRight(slurped_keyword_args.length).zip(kwargs).map(a
              => (a._2._1, a._1._2)) ++ kwargs.drop(fkeyword.length).map(a => (a._1, a._2.get)) ++ 
              slurpy.map(a => (if(slurped_positional_args.length != 0) { (a._1, AtomishArray(slurped_positional_args)) } else if (a._2
                != None) { (a._1,
              a._2.get) } else { (a._1, AtomishArray(Array())) })) ++
              kwslurpy.map(a => (if(slurped_keyword_args.length != 0) { (a._1, AtomishMap(MMap() ++ slurped_keyword_args)) } else if (a._2 !=
                None) { (a._1,
              a._2.get) } else { (a._1, AtomishMap(MMap[AtomishThing, AtomishThing]())) }))
              ).map(a => (a._1,
              self.roots("eval").asInstanceOf[AlienProxy].activate(AtomishArgs(List(Left(a._2 /*.asInstanceOf[AtomishThing]*/))))))
              
            scopes = (MMap() ++ letified_args) +: scopes;
            //println(code.toString())
            //println("Array(" + letified_args.map(ar => "(\"" + ar._1 + "\", " + ar._2.toString() + ")").mkString(", ") + ")")
            //println(scopes)
            var result = self.roots("eval").asInstanceOf[AlienProxy].activate(AtomishArgs(List(Left(code))));
            var sco = scopes.tail;
            scopes = sco;
            //println(result.toString())
            //AtomishUnset
            result
          }
        })
        for(docs <- docstring) { fn.cells("documentation") = AtomishString(docs) }
        fn
      }
    }),
    "'"         -> QAlienProxy(ctd => {
      AtomishCall("'", ctd.args)
    }),
    "unquote"   -> AlienProxy(_.args match {
      case List(Left(AtomishCall("'", x))) => {
        if(x.length != 1) {
          AtomishCommated(x)
        } else {
          x(0)
        }
      }
    }),
    "''"        -> QAlienProxy(ctd => {
      def unqq(code: AtomishCode): AtomishCode = code match {
        case AtomishCall("'", x)     => AtomishCall("'", x)
        case AtomishCall("`", x)     => {
          var qq_bits = x.flatMap(arg =>
            self.roots("eval").asInstanceOf[AlienProxy].activate(AtomishArgs(List(Left(arg)))) match {
              case a: AtomishCode => Array[AtomishCode](a)
              case _              => Array[AtomishCode]()
            })
          if(qq_bits.length != 1) {
            AtomishCommated(qq_bits)
          } else {
            qq_bits(0)
          }
        }
        case AtomishCall(call, args) => AtomishCall(call, args.map(x => unqq(x)))
        case AtomishForm(x)          => AtomishForm(x.map(y => unqq(y)))
      }
      AtomishCall("'",
        ctd.args.map(x => unqq(x)))
    }),
    "macro"     -> QAlienProxy(ctd => {
      var mac: QAlienProxy = QAlienProxy(null)
      mac.call = (macargs => {
        var arg_scope  = MMap[String, AtomishThing](
          "arguments" -> AtomishArray(macargs.args.map(_ match { // We have to unwrap unneeded AtomishForms
            case AtomishForm(List(x)) => x
            case x                    => x
          }).asInstanceOf[Array[AtomishThing]])
        )
        scopes = arg_scope +: scopes;
        var result: AtomishThing = AtomishUnset
        if(mac.cells.isDefinedAt("code")) {
          result = self.roots("eval").asInstanceOf[AlienProxy].activate(AtomishArgs(List(Left(mac.cells("code")))))
        }
        var sco = scopes.tail;
        scopes = sco;
        result
      })
      mac.cells("code") = if(ctd.args.length > 0) { ctd.args(0) } else { AtomishNL }
      mac
    }),
    "Array"     -> AlienProxy(arg_blob => AtomishArray(arg_blob.args.flatMap(_ match {
      case Left(x) => Array[AtomishThing](x)
      case _       => Array[AtomishThing]()
    }).toArray)),
    "Map"       -> AlienProxy(arg_blob => AtomishMap(arg_blob.args.map(_ match {
      case Right((x, y)) => MMap[AtomishThing, AtomishThing](AtomishString(x) -> y)
      case _             => MMap[AtomishThing, AtomishThing]()
    }).foldLeft(MMap[AtomishThing, AtomishThing]())(_ ++ _))),
    "Origin"    -> AtomishOrigin(),
    "primfn"    -> AlienProxy(_.args match {
      case List(Left(AtomishString(str))) => {
        new AtomishMacro(this,
          roots("read").asInstanceOf[AlienProxy].activate(AtomishArgs(List(Left(AtomishString(str))))).asInstanceOf[AtomishCode])
      }
      case _                              => null //boom
    }),
    "="         -> QAlienProxy(ctd => ctd.args match {
      case Array(AtomishMessage(cell_name), x) => {
        var ret = roots("eval").asInstanceOf[AlienProxy].activate(AtomishArgs(List(Left(x))))
        roots(cell_name) = ret
        ret
      }
      //case _                                   => {
      //  println(PreScalaPrinter.print_with_forms(ctd))
      //  null
      //}
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
