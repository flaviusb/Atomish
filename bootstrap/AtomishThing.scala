package net.flaviusb.atomish

import scala.language.experimental.macros
import scala.collection.mutable.{Map => MMap}

import org.jregex.{Pattern, Replacer}

// AtomishThing is the supertype of all the container types for all 'things' that are reified in Atomish
trait AtomishThing {
  var pre_type = "Thing"
  var cells: MMap[String, AtomishThing] = MMap()
  // apply should respect the MOP, but does not deal with activatability
  def apply(key: AtomishString): AtomishThing = {
    if(cells.isDefinedAt("cell")) {
      cells("cell") match {
        case (proxy: AlienProxy) => proxy.activate(AtomishArgs(List(Left(key))))
      }
    } else {
      AtomishThing.bootstrap_cells("cell")(this).asInstanceOf[AlienProxy].activate(AtomishArgs(List(Left(key))))
    }
  }
  def update(key: AtomishString, value: AtomishThing) {
    cells(key.value) = value
  }
}

trait IdempotentEval // That is, an AtomishThing that 'eval's to itself

object AtomishThing {
  var post_bootstrap: MMap[(String, String), AtomishThing => AtomishThing] = MMap()
  var bootstrap_cells: MMap[String, AtomishThing => AtomishThing] = MMap(
    "hasCell"     -> { thing => AlienProxy(_.args match {
      case List(Left(AtomishString(x))) => {
        // A thing can have a cell value of AtomishUnset, or it can have a cell value that is not AtomishUnset,  or if it does not have a
        // value the bootstrap can have a value, or it has no value, considered in that order.
        if(thing.cells.isDefinedAt(x)) {
          if(thing.cells(x) == AtomishUnset) {
            AtomishBoolean(false)
          } else {
            AtomishBoolean(true)
          }
        } else if(AtomishThing.post_bootstrap.isDefinedAt((thing.pre_type, x))) {
          AtomishBoolean(true)
        } else {
          AtomishBoolean(AtomishThing.bootstrap_cells.isDefinedAt(x))
        }
      }
      case _ => AtomishBoolean(false)
    }) },
    "cell"        -> { thing => AlienProxy(_.args match {
      case List(Left(AtomishString(x))) => {
        if(thing.cells.isDefinedAt(x)) {
          thing.cells(x)
        } else if(AtomishThing.post_bootstrap.isDefinedAt((thing.pre_type, x))) {
          AtomishThing.post_bootstrap((thing.pre_type, x))(thing)
        } else {
          AtomishThing.bootstrap_cells(x)(thing)
        }
      }
    }) },
    "setCell"     -> { thing => AlienProxy(_.args match {
      case List(Left(name: AtomishString), Left(value: AtomishThing)) => {
        thing(name) = value
        value
      }
    }) },
    "activatable" -> { thing => AtomishBoolean(false) }
  )
}

object AtomishUnset extends AtomishThing {
  pre_type = "Unset"
}

case class AtomishBoolean(value: Boolean) extends AtomishThing with AtomishCode with IdempotentEval {
  pre_type = "Boolean"
}

case class AtomishInt(value: Int) extends AtomishThing with AtomishCode with IdempotentEval {
  pre_type = "Int"
  cells ++= MMap[String, AtomishThing](
    "+" -> AlienProxy(inttoint(value + _)),
    "-" -> AlienProxy(inttoint(value -_)),
    "×" -> AlienProxy(inttoint(value * _)),
    "÷" -> AlienProxy(inttoint(value / _)),
    "asText" -> AlienProxy(_.args match {
      case List() => AtomishString(value.toString())
      case _      => null // Not sure what to do here - maybe swallow arguments silently?
    }),
    "=="     -> AlienProxy(inttobool(_ == value)),
    "!="     -> AlienProxy(inttobool(_ != value)),
    "<=>"    -> AlienProxy(inttoint(a => if(a < value) { -1 } else if(a == value) { 0 } else { -1 })),
    "<"      -> AlienProxy(inttobool(a => value < a)),
    "<="     -> AlienProxy(inttobool(a => value <= a)),
    ">"      -> AlienProxy(inttobool(a => value > a)),
    ">="     -> AlienProxy(inttobool(a => value >= a)),
    "…"      -> AlienProxy(_.args match {
      case List(Left(end: AtomishInt)) => {
        AtomishOrigin(MMap[String, AtomishThing]("start" -> AtomishInt(value), "end" -> end))
      }
    })
  )
}

case class AtomishDecimal(value: Double) extends AtomishThing with AtomishCode with IdempotentEval {
  pre_type = "Decimal"
  cells ++= MMap[String, AtomishThing](
    "+" -> AlienProxy(dectodec(value + _)),
    "-" -> AlienProxy(dectodec(value - _)),
    "×" -> AlienProxy(dectodec(value * _)),
    "÷" -> AlienProxy(dectodec(value / _)),
    "asText" -> AlienProxy(_.args match {
      case List() => AtomishString(value.toString())
      case _      => null // Not sure what to do here - maybe swallow arguments silently?
    }),
    "=="     -> AlienProxy(dectobool(_ == value)),
    "!="     -> AlienProxy(dectobool(_ != value)),
    "<=>"    -> AlienProxy(dectoint(a => if(a < value) { -1 } else if(a == value) { 0 } else { -1 })),
    "<"      -> AlienProxy(dectobool(a => value < a)),
    "<="     -> AlienProxy(dectobool(a => value <= a)),
    ">"      -> AlienProxy(dectobool(a => value > a)),
    ">="     -> AlienProxy(dectobool(a => value >= a))
 )
}

case class dectodec(call: Double => Double) extends (AtomishArgs => AtomishDecimal) {
  override def apply(args: AtomishArgs): AtomishDecimal = {
    args.args match {
      case List(Left(AtomishDecimal(x))) => AtomishDecimal(call(x))
      case _                             => null // Should error
    }
  }
}
case class dectoint(call: Double => Int) extends (AtomishArgs => AtomishInt) {
  override def apply(args: AtomishArgs): AtomishInt = {
    args.args match {
      case List(Left(AtomishDecimal(x))) => AtomishInt(call(x))
      case _                             => null // Should error
    }
  }
}

case class inttoint(call: Int => Int) extends (AtomishArgs => AtomishInt) {
  override def apply(args: AtomishArgs): AtomishInt = {
    args.args match {
      case List(Left(AtomishInt(x))) => AtomishInt(call(x))
      case _                         => null // Should error
    }
  }
}

case class nonetoint(call: () => Int) extends (AtomishArgs => AtomishInt) {
  override def apply(args: AtomishArgs): AtomishInt = {
    args.args match {
      case List() => AtomishInt(call())
      case _      => null // Should error
    }
  }
}
case class strtostr(call: String => String) extends (AtomishArgs => AtomishString) {
  override def apply(args: AtomishArgs): AtomishString = {
    args.args match {
      case List(Left(AtomishString(x))) => AtomishString(call(x))
      case _                            => null // Should error
    }
  }
}
case class inttostr(call: Int => String) extends (AtomishArgs => AtomishString) {
  override def apply(args: AtomishArgs): AtomishString = {
    args.args match {
      case List(Left(AtomishInt(x))) => AtomishString(call(x))
      case _                         => null // Should error
    }
  }
}
case class intinttostr(call: (Int, Int) => String) extends (AtomishArgs => AtomishString) {
  override def apply(args: AtomishArgs): AtomishString = {
    args.args match {
      case List(Left(AtomishInt(x)), Left(AtomishInt(y))) => AtomishString(call(x, y))
      case _                                              => null // Should error
    }
  }
}
case class booltobool(call: Boolean => Boolean) extends (AtomishArgs => AtomishBoolean) {
  override def apply(args: AtomishArgs): AtomishBoolean = {
    args.args match {
      case List(Left(AtomishBoolean(x))) => AtomishBoolean(call(x))
      case _                             => null // Should error
    }
  }
}
case class inttobool(call: Int => Boolean) extends (AtomishArgs => AtomishBoolean) {
  override def apply(args: AtomishArgs): AtomishBoolean = {
    args.args match {
      case List(Left(AtomishInt(x))) => AtomishBoolean(call(x))
      case _                         => null // Should error
    }
  }
}
case class dectobool(call: Double => Boolean) extends (AtomishArgs => AtomishBoolean) {
  override def apply(args: AtomishArgs): AtomishBoolean = {
    args.args match {
      case List(Left(AtomishDecimal(x))) => AtomishBoolean(call(x))
      case _                             => null // Should error
    }
  }
}

case class AtomishString(value: String) extends AtomishThing with AtomishCode with IdempotentEval {
  pre_type = "Text"
  cells ++= MMap[String, AtomishThing](
    "length"     -> AlienProxy(nonetoint(value.length)),
    "+"          -> AlienProxy(strtostr(value + _)),
    "at"         -> AlienProxy(inttostr(x => value.substring(x, x + 1))),
    "substring"  -> AlienProxy(intinttostr((x, y) => value.substring(x, y))),
    "asText"     -> AlienProxy(_.args match {
      case List() => AtomishString(value)
      case _      => null // Not sure what to do here - maybe swallow arguments silently?
    }),
    "=="         -> AlienProxy(_.args match {
      case List(Left(AtomishString(x))) => AtomishBoolean(value == x)
      case _                            => AtomishBoolean(false)
    }),
    "!="         -> AlienProxy(_.args match {
      case List(Left(AtomishString(x))) => AtomishBoolean(value != x)
      case _                            => AtomishBoolean(true)
    }),
    "~="         -> AlienProxy(_.args match {
      case List(Left(AtomishRegex(pattern, flags))) => {
        var regex = new Pattern(pattern, flags.mkString)
        AtomishBoolean(regex.matcher(value).matches())
      }
    }),
    "replace"    -> AlienProxy(_.args match {
      case List(Left(AtomishString(str_regex)), Left(AtomishString(replace))) => {
        var regex = new Pattern(Pattern.quote(str_regex))
        AtomishString(regex.replacer(replace).replaceFirst(value))
      }
      case List(Left(AtomishRegex(pattern, flags)), Left(AtomishString(replace))) => {
        var regex = new Pattern(pattern, flags.mkString)
        AtomishString(regex.replacer(replace).replaceFirst(value))
      }
    }),
    "replaceAll" -> AlienProxy(_.args match {
      case List(Left(AtomishString(str_regex)), Left(AtomishString(replace))) => {
        var regex = new Pattern(Pattern.quote(str_regex))
        AtomishString(regex.replacer(replace).replace(value))
      }
      case List(Left(AtomishRegex(pattern, flags)), Left(AtomishString(replace))) => {
        var regex = new Pattern(pattern, flags.mkString)
        AtomishString(regex.replacer(replace).replace(value))
      }
    })
  )
}

// AtomishInterpolatedString class should decay in the evaller into an AtomishString
case class AtomishInterpolatedString(value: List[AtomishCode]) extends AtomishThing with AtomishCode 

case class AtomishRegex(regex: String, flags: List[String]) extends AtomishThing with AtomishCode with IdempotentEval {
  pre_type = "Regex"
  cells ++= MMap[String, AtomishThing](
    "pattern"   -> AtomishString(regex),
    "flags"     -> AtomishArray(flags.map(x => AtomishString(x)).toArray)
  )
}

case class AtomishArray(var value: Array[AtomishThing]) extends AtomishThing with AtomishCode with IdempotentEval {
  pre_type = "Array"
  cells ++= MMap[String, AtomishThing](
    "length"    -> AlienProxy(nonetoint(() => value.length)),
    "+"         -> AlienProxy(_.args match {
      case List(Left(AtomishArray(app))) => AtomishArray(value ++ app)
    }),
    "at"        -> AlienProxy(_.args match {
      case List(Left(AtomishInt(x)))    => value(x)
      case List(Left(x: AtomishOrigin)) => {
        // We assume x should follow the "Range" pseudo protocol
        val start = (if(x.cells.isDefinedAt("start")) {
          x.cells("start") match {
            case AtomishInt(st_int) => (if(st_int >= 0) { st_int } else { value.length + st_int })
          }
        } else { 0 })
        val end = (if(x.cells.isDefinedAt("end")) {
          x.cells("end") match {
            case AtomishInt(en_int) => (if(en_int >= 0) { en_int } else { value.length + en_int })
          }
        } else { 0 })
        val reversed = (start > end)
        val real_start = (if(!reversed) { start } else { end })
        val real_end   = (if(reversed)  { start } else { end })
        val new_value = value.slice(real_start, real_end + 1)
        AtomishArray((if(reversed) { new_value.reverse } else { new_value }))
      }
      case _                         => AtomishUnset
    }),
    "at="       -> AlienProxy(_.args match {
      case List(Left(AtomishInt(x)), Left(new_value)) => { value(x) = new_value; new_value }
      case _                                          => AtomishUnset
    }),
    "keys"      -> AlienProxy(x => AtomishArray(0.to(value.length - 1).toArray.map(key => AtomishInt(key)))),
    "values"    -> AlienProxy(x => AtomishArray(value)),
    "dropLeft"  -> AlienProxy(_.args match {
      case List(Left(AtomishInt(to_drop))) => AtomishArray(value.drop(to_drop))
    }),
    "dropRight" -> AlienProxy(_.args match {
      case List(Left(AtomishInt(to_drop))) => AtomishArray(value.dropRight(to_drop))
    }),
    "reverse"   -> AlienProxy(_ => AtomishArray(value.reverse)),
    "join"      -> AlienProxy(_ => AtomishString(value.map(_ match {
      case AtomishString(x)  => x
      case AtomishInt(x)     => x.toString()
      case AtomishDecimal(x) => x.toString()
      case x                 => x.toString()
    }).mkString)),
    "pop!"      -> AlienProxy(_ => {
      var ret = value(0)
      value = value.drop(1)
      ret
    }),
    "push!"     -> AlienProxy(_.args match {
      case List(Left(x)) => {
        value = x +: value
        AtomishUnset
      }
    })
  )
}

case class AtomishMap(var value: MMap[AtomishThing, AtomishThing]) extends AtomishThing with AtomishCode with IdempotentEval {
  pre_type = "Map"
  cells ++= MMap[String, AtomishThing](
    "length"    -> AlienProxy(nonetoint(() => value.size)),
    "+"         -> AlienProxy(_.args match {
      case List(Left(AtomishMap(app))) => AtomishMap(value ++ app)
    }),
    "at"        -> AlienProxy(_.args match {
      case List(Left(x: AtomishThing)) => value(x)
      case _                           => AtomishUnset
    }),
    "at="       -> AlienProxy(_.args match {
      case List(Left(x: AtomishThing), Left(new_value)) => { value(x) = new_value; new_value }
      case _                                            => AtomishUnset
    }),
    "keys"      -> AlienProxy(x => AtomishArray(value.keys.toArray)),
    "values"    -> AlienProxy(x => AtomishArray(value.values.toArray))
  )
}

case class AtomishMessage(name: String) extends AtomishThing with AtomishCode {
  cells("name") = AtomishString(name)
}

case class AtomishCommated(args: Array[AtomishCode]) extends AtomishThing with AtomishCode {
  cells("name") = AtomishString("")
  cells("args") = AtomishArray(args.asInstanceOf[Array[AtomishThing]])
}

case class AtomishCall(name: String, args: Array[AtomishCode]) extends AtomishThing with AtomishCode {
  cells("name") = AtomishString(name)
  cells("args") = AtomishArray(args.asInstanceOf[Array[AtomishThing]])
}

case class MessageChain(messages: Array[AtomishMessage]) extends AtomishThing with AtomishCode

trait AtomishCode extends AtomishThing

object AtomishNL extends AtomishThing with AtomishCode {
  cells("name") = AtomishString(".")
}

// As in, the reader reads a string and outputs a form, the evaller evals a form...
case class AtomishForm(things: List[AtomishCode]) extends AtomishThing with AtomishCode {
  cells("args") = AtomishArray(things.toArray.asInstanceOf[Array[AtomishThing]])
}

case class shallowwrapstrtocode(call: AtomishString => AtomishCode) extends (AtomishArgs => AtomishCode) {
  override def apply(args: AtomishArgs): AtomishCode = {
    args.args match {
      case List(Left(x: AtomishString)) => call(x)
      case _                            => null // Should error
    }
  }
}


case class AtomishArgs(args: List[Either[AtomishThing, (String, AtomishThing)]])

case class AtomishComment(text: String) extends AtomishThing with AtomishCode

case class AlienProxy(var call: AtomishArgs => AtomishThing) extends AtomishThing {
  cells("activatable") = AtomishBoolean(true)
  // Deal with 'activate' in evaller; this is graceless, but necessary for bootstrapping
  def activate(args: AtomishArgs): AtomishThing = call(args)
}

case class QAlienProxy(var call: AtomishCommated => AtomishThing) extends AtomishThing {
  cells("activatable") = AtomishBoolean(true)
  // Deal with 'activate' in evaller; this is graceless, but necessary for bootstrapping
  def activate(args: AtomishCommated): AtomishThing = call(args)
}

class AtomishMacro(universe: PreUniverse, code: AtomishCode) extends AlienProxy(null) with AtomishCode {
  call = macro_exec;
  cells("activatable") = AtomishBoolean(true)
  cells("code")        = code
  def macro_exec(a: AtomishArgs): AtomishThing = {
    universe(AtomishPlace(AtomishMessage("eval"))).get.asInstanceOf[AlienProxy].activate(AtomishArgs(List(Left(cells("code")))))
  }
}

case class AtomishOrigin(var origin_with: MMap[String, AtomishThing] = MMap[String, AtomishThing]()) extends AtomishThing with AtomishCode with IdempotentEval {
  pre_type = "Origin"
  cells("with") = AlienProxy(x => {
    x.args.foreach(_ match {
      case Right((cellName, cellValue)) => {
        origin_with(cellName) = cellValue;
        cells(cellName) = cellValue;
      }
    })
    this
  })
  cells("mimic") = AlienProxy(x => {
    var y = AtomishOrigin(origin_with)
    y.cells = cells;
    y
  })
  cells ++= origin_with;
}
