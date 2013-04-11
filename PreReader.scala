package net.flaviusb.atomish

import scala.util.parsing.combinator._

import scala.language.postfixOps

class PreReader {
}

object AtomishParser extends RegexParsers {
  override type Elem = Char
  def identifier: Parser[AtomishMessage] = ("[a-zA-Z][a-zA-Z0-9_:$!?]*".r | "[~!@$%^&*_=\'`/?×÷+-]*".r) ^^ { AtomishMessage(_) }
  def code: Parser[AtomishCode] = identifier // This will eventually be a big union of all types that can constitute standalone code
  def commated_bit: Parser[List[AtomishCode]] = (("," ~ code)*) ^^ { _.map { case "," ~ frag => frag } }
  def commated: Parser[AtomishCommated] = "(" ~ ((code ~ commated_bit)?) ~ ")" ^^ {
    case opb ~ Some((code1: AtomishCode) ~ (code2: List[AtomishCode])) ~ opc => AtomishCommated(Array[AtomishCode](code1) ++
      (code2.toArray[AtomishCode]))
    case opb ~ None ~ opc => AtomishCommated(Array())
  }
  def atomish_call: Parser[AtomishCall] = identifier ~ commated ^^ {
    case AtomishMessage(name) ~ AtomishCommated(args) => AtomishCall(name, args)
  }
}
