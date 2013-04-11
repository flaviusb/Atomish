package net.flaviusb.atomish

import scala.util.parser.combinator._

class PreReader {
}

object AtomishParser extends RegexParsers {
  override type Elem = Char
  def identifier = "[a-zA-Z][a-zA-Z0-9_:$!?]*".r | "[~!@$%^&*_=\'`/?×÷+-]*".r
}
