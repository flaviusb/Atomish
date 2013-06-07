package net.flaviusb.atomish

import scala.collection.mutable.{Map => MMap}

object PreScalaMirror {
  val mirror = Origin(with(MMap[String, AtomishThing](
    "idempotent?" -> QAlienProxy(ctd => { AtomishUnset }),
    "cells"       -> QAlienProxy(ctd => { AtomishUnset }),
    "code?"       -> QAlienProxy(ctd => { AtomishUnset }),
    "origin?"     -> QAlienProxy(ctd => { AtomishUnset }),
    "message?"    -> QAlienProxy(ctd => { AtomishUnset }),
    "pre_type"    -> QAlienProxy(ctd => { AtomishUnset }),
    "≡"           -> QAlienProxy(ctd => { AtomishUnset })
  )))
}
