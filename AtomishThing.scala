package net.flaviusb.atomish

import scala.language.experimental.macros
import scala.collection.mutable.{Map => MMap}

// AtomishThing is the supertype of all the container types for all 'things' that are reified in Atomish
trait AtomishThing {
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

object AtomishThing {
  var bootstrap_cells: MMap[String, AtomishThing => AtomishThing] = MMap(
    "hasCell" -> { thing => AlienProxy(_.args match {
      case List(Left(AtomishString(x))) => {
        // A thing can have a cell value of AtomishUnset, or it can have a cell value that is not AtomishUnset,  or if it does not have a
        // value the bootstrap can have a value, or it has no value, considered in that order.
        if(thing.cells.isDefinedAt(x)) {
          if(thing.cells(x) == AtomishUnset) {
            AtomishBoolean(false)
          } else {
          AtomishBoolean(true)
          }
        } else {
          AtomishBoolean(AtomishThing.bootstrap_cells.isDefinedAt(x))
        }
      }
      case _ => AtomishBoolean(false)
    }) },
    "cell" -> { thing => AlienProxy(_.args match {
      case List(Left(AtomishString(x))) => {
        if(thing.cells.isDefinedAt(x)) {
          thing.cells(x)
        } else {
          AtomishThing.bootstrap_cells(x)(thing)
        }
      }
    }) },
    "activatable" -> { thing => AtomishBoolean(false) }
  )
}

object AtomishUnset extends AtomishThing

case class AtomishBoolean(value: Boolean) extends AtomishThing with AtomishCode {
}

case class AtomishInt(value: Int) extends AtomishThing with AtomishCode {
  cells ++= MMap[String, AtomishThing](
    "+" -> AlienProxy(inttoint(value + _)),
    "-" -> AlienProxy(inttoint(value -_)),
    "×" -> AlienProxy(inttoint(value * _)),
    "÷" -> AlienProxy(inttoint(value / _))
  )
}

case class AtomishDecimal(value: Double) extends AtomishThing with AtomishCode {
  cells ++= MMap[String, AtomishThing](
    "+" -> AlienProxy(dectodec(value + _)),
    "-" -> AlienProxy(dectodec(value - _)),
    "×" -> AlienProxy(dectodec(value * _)),
    "÷" -> AlienProxy(dectodec(value / _))
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

case class AtomishString(value: String) extends AtomishThing with AtomishCode {
  cells ++= MMap[String, AtomishThing](
    "length" -> AlienProxy(nonetoint(value.length)),
    "+"      -> AlienProxy(strtostr(value + _))
  )
}

case class AtomishMessage(name: String) extends AtomishThing with AtomishCode

case class AtomishCommated(args: Array[AtomishCode]) extends AtomishThing with AtomishCode

case class AtomishCall(name: String, args: Array[AtomishCode]) extends AtomishThing with AtomishCode

case class MessageChain(messages: Array[AtomishMessage]) extends AtomishThing with AtomishCode

trait AtomishCode extends AtomishThing

object AtomishNL extends AtomishThing with AtomishCode

// As in, the reader reads a string and outputs a form, the evaller evals a form...
case class AtomishForm(things: List[AtomishCode]) extends AtomishThing with AtomishCode

case class shallowwrapstrtocode(call: AtomishString => AtomishCode) extends (AtomishArgs => AtomishCode) {
  override def apply(args: AtomishArgs): AtomishCode = {
    args.args match {
      case List(Left(x: AtomishString)) => call(x)
      case _                            => null // Should error
    }
  }
}


case class AtomishArgs(args: List[Either[AtomishThing, (String, AtomishThing)]])

case class AlienProxy(call: AtomishArgs => AtomishThing) extends AtomishThing {
  cells("activatable") = AtomishBoolean(true)
  // Deal with 'activate' in evaller; this is graceless, but necessary for bootstrapping
  def activate(args: AtomishArgs): AtomishThing = call(args)
}
