package net.flaviusb.atomish

import scala.collection.mutable.{Map => MMap}

object PreScalaMirror {
  // This mirror only works on quote protected arguments.
  val mirror = AtomishOrigin(MMap[String, AtomishThing](
    "idempotent?" -> QAlienProxy(_.args match {
      case Array(AtomishCall("'", Array(x: IdempotentEval))) => AtomishBoolean(true)
      case _                                                 => AtomishBoolean(false)
    }),
    "message?"    -> QAlienProxy(_.args match {
      case Array(AtomishCall("'", Array(x: AtomishMessage))) => AtomishBoolean(true)
      case _                                                 => AtomishBoolean(false)
    }),
    "call?"       -> QAlienProxy(_.args match {
      case Array(AtomishCall("'", Array(x: AtomishCall))) => AtomishBoolean(true)
      case _                                              => AtomishBoolean(false)
    }),
    "dot?"        -> QAlienProxy(_.args match {
      case Array(AtomishCall("'", Array(AtomishNL))) => AtomishBoolean(true)
      case _                                         => AtomishBoolean(false)
    }),
    "brackets?"   -> QAlienProxy(_.args match {
      case Array(AtomishCall("'", Array(x: AtomishCommated))) => AtomishBoolean(true)
      case _                                                  => AtomishBoolean(false)
    }),
    "cells"       -> QAlienProxy(ctd => { AtomishUnset }),
    "code?"       -> QAlienProxy(ctd => { AtomishUnset }),
    "origin?"     -> QAlienProxy(ctd => { AtomishUnset }),
    "pre_type"    -> QAlienProxy(ctd => { AtomishUnset }),
    "≡"           -> QAlienProxy(ctd => { AtomishUnset })
  ))
}
