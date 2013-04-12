package net.flaviusb.atomish

import scala.util.parsing.combinator._

import scala.language.postfixOps

class PreReader {
  def read(str: AtomishString): AtomishCode = {
    AtomishParser.parseAll(AtomishParser.code, str.value) match {
      case AtomishParser.Success(atomval, _) => atomval
      case _                   => null // Should trigger condition system
    }
  }
  val alien_read = AlienProxy(read)
}

object AtomishParser extends RegexParsers {
  override type Elem = Char
  def rational = "[+-]?[0-9]+\\.[0-9]+".r ^^ { (double: String) => AtomishDecimal(double.toDouble) }
  def integer  = "[+-]?[0-9]+".r ^^ { (int: String) => AtomishInt(int.toInt) }
  def string   = """"([^"\\]|\\")*"""".r ^^ { (str: String) => AtomishString(str) }
  def identifier: Parser[AtomishMessage] = ("[a-zA-Z][a-zA-Z0-9_:$!?]*".r | "[~!@$%^&*_=\'`/?×÷+-]*".r) ^^ { AtomishMessage(_) }
  def code: Parser[AtomishCode] = (commated | atomish_call | string | rational | integer | identifier) // This will eventually be a big union of all types that can constitute standalone code
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
