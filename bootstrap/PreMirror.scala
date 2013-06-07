package net.flaviusb.atomish

import scala.collection.mutable.{Map => MMap}

object PreScalaMirror {
  // This mirror only works on quote protected arguments.
  val mirror = Origin(with(MMap[String, AtomishThing](
    "idempotent?" -> QAlienProxy(_.args match {
      case AtomishCall("'", Array(x: IdempotentEval)) => AtomishBoolean(true)
      case _                                          => AtomishBoolean(false)
    }),
    "cells"       -> QAlienProxy(ctd => { AtomishUnset }),
    "code?"       -> QAlienProxy(ctd => { AtomishUnset }),
    "origin?"     -> QAlienProxy(ctd => { AtomishUnset }),
    "message?"    -> QAlienProxy(ctd => { AtomishUnset }),
    "pre_type"    -> QAlienProxy(ctd => { AtomishUnset }),
    "≡"           -> QAlienProxy(ctd => { AtomishUnset })
  )))
}
