package net.flaviusb.atomish

import scala.collection.mutable.{Map => MMap}

class PreUniverse {
  var roots: MMap[String, AtomishThing] = MMap[String, AtomishThing](
    "version" -> AtomishDecimal(0.1)
  )
  def apply(key: AtomishPlace): Option[AtomishThing] = {
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
    key.form match {
      case AtomishMessage(name) => {
        return roots.get(name)
      }
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