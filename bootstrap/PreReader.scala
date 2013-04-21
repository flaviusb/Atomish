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

  val alien_read = AlienProxy(shallowwrapstrtocode(read))
}

object AtomishParser extends RegexParsers {
  override type Elem = Char
  override def skipWhitespace = false
  def nll: Parser[AtomishCode] = "[.]|\\n|\\n\\r".r ^^ { x => AtomishNL }
  def wss: Parser[String] = "[ ]+".r ^^ { x => "" }
  def rational = "[+-]?[0-9]+\\.[0-9]+".r ^^ { (double: String) => AtomishDecimal(double.toDouble) }
  def integer  = "[+-]?[0-9]+".r ^^ { (int: String) => AtomishInt(int.toInt) }
  def string   = ("\"" ~ """([^"\\]|\\")*""".r ~ "\"") ^^ { case "\"" ~ str ~ "\"" => AtomishString(str) }
  def identifier: Parser[AtomishMessage] = ("[a-zA-Z][a-zA-Z0-9_:$!?]*".r | "[~!@$%^&*_=\'`/?×÷+-]+".r) ^^ { AtomishMessage(_) }
  def code_tiny_bit: Parser[AtomishCode] = (commated | atomish_call | string | rational | integer | identifier | nll) // This will eventually be a big union of all types that can constitute standalone code
  def code_bit: Parser[List[AtomishCode]] = (((nll ~ code_tiny_bit) | nll | (wss ~ code_tiny_bit))*) ^^ { _.flatMap { 
    case "" ~ (code_piece: AtomishCode)        => List(code_piece)
    case AtomishNL ~ (code_piece: AtomishCode) => List(AtomishNL, code_piece)
    case AtomishNL                             => List(AtomishNL)
  } }
  def code: Parser[AtomishForm] = code_tiny_bit ~ code_bit ^^ {
    case a ~ b => AtomishForm(a::b)
  }
  def commated_bit: Parser[List[AtomishCode]] = (("," ~ (wss?) ~ code)*) ^^ { _.map { case "," ~ x ~ frag => frag } }
  def commated: Parser[AtomishCommated] = "(" ~ (wss?) ~ ((code ~ (wss?) ~ commated_bit)?) ~ (wss?) ~ ")" ^^ {
    case opb ~ wsa ~ Some((code1: AtomishCode) ~ wsb ~ (code2: List[AtomishCode])) ~ wsc ~ opc => AtomishCommated(Array[AtomishCode](code1) ++
      (code2.toArray[AtomishCode]))
    case opb ~ wsa ~ None ~ wsb ~ opc => AtomishCommated(Array())
  }
  def atomish_call: Parser[AtomishCall] = identifier ~ commated ^^ {
    case AtomishMessage(name) ~ AtomishCommated(args) => AtomishCall(name, args)
  }
}
