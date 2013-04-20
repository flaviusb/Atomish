package net.flaviusb.atomish

import scala.collection.mutable.{Map => MMap}

class PreUniverse {
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
        roots(name) = value
        value
      }
    }),
    "cell"   -> AlienProxy(_.args match {
      case List(Left(AtomishString(name))) => {
        roots(name)
      }
    }),
    "hasCell"   -> AlienProxy(_.args match {
      case List(Left(AtomishString(name))) => {
        AtomishBoolean((roots.isDefinedAt(name)) && (roots(name) != AtomishUnset))
      }
    }),
    "Array"     -> AlienProxy(arg_blob => AtomishArray(arg_blob.args.flatMap(_ match {
      case Left(x) => Array[AtomishThing](x)
      case _       => Array[AtomishThing]()
    }).toArray)),
    "Map"       -> AlienProxy(arg_blob => AtomishMap(arg_blob.args.map(_ match {
      case Right((x, y)) => MMap[AtomishThing, AtomishThing](AtomishString(x) -> y)
      case _             => MMap[AtomishThing, AtomishThing]()
    }).foldLeft(MMap[AtomishThing, AtomishThing]())(_ ++ _)))
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
    key.form match {
      case AtomishMessage(name) => {
        return roots.get(name)
      }
      case AtomishCall(name, _) => {
        return roots.get(name)
      }
      //case AtomishForm(head :: rest) => {
      //  var root = roots.get(head)
      //}
      case MessageChain(Array(AtomishMessage(first), messages @ _*)) => {
        var root = roots.get(first)
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
  def update(key: AtomishPlace, value: Option[AtomishThing]) = { }
}
