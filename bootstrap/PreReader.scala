package net.flaviusb.atomish

import scala.util.parsing.combinator._
import scala.collection.mutable.{Map => MMap, MutableList => MList}

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
  def qstring_escapes = ("""\\""" | """\n""" | """\"""") ^^ {
    case """\\""" => """\"""
    case """\n""" => "\n"
    case """\"""" => "\""
  }
  def sstring_escapes = ("""\\""" | """\n""" | """\]""") ^^ {
    case """\\""" => """\"""
    case """\n""" => "\n"
    case """\]""" => "]"
  }
  def interpolated_section: Parser[AtomishCode] = "#{" ~ code ~ "}" ^^ { case "#{" ~ interpolated_code ~ "}" => interpolated_code }
  def qstring   = ("\"" ~ ((interpolated_section | (("""([^"\\])""".r | qstring_escapes) ^^ { AtomishString(_) }))*) ~ "\"") ^^ {
    case "\"" ~ List(AtomishString(x)) ~  "\"" => AtomishString(x)
    case "\"" ~ chunks ~ "\"" => {
      var interpolated_list = MList[AtomishCode]()
      var accumulated_string = ""
      for(chunk <- chunks) {
        chunk match {
          case AtomishString(x) => accumulated_string += x
          case x: AtomishCode   => {
            if(accumulated_string != "") {
              interpolated_list += AtomishString(accumulated_string)
              accumulated_string = ""
            }
            interpolated_list += x
          }
        }
      }
      if(accumulated_string != "") {
        interpolated_list += AtomishString(accumulated_string)
        accumulated_string = ""
      }
      interpolated_list match {
        case MList(x: AtomishString) => x
        case _                       => AtomishInterpolatedString(interpolated_list.toList)
      }
    }
  }
  def sstring   = ("#[" ~ (("""([^\]\\])""".r | sstring_escapes)*) ~ "]") ^^ { case "#[" ~ str ~ "]" => AtomishString(str.foldLeft("")(_ + _)) }
  def string = (sstring | qstring)
  def identifier: Parser[AtomishMessage] = ("[a-zA-Z][a-zA-Z0-9_:$!?]*".r | "[~!@$%^&*_=\'`/?×÷+-]+".r) ^^ { AtomishMessage(_) }
  def code_tiny_bit: Parser[AtomishCode] = (comment | commated | atomish_call | string | rational | integer | identifier | nll) // This will eventually be a big union of all types that can constitute standalone code
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
  def comment: Parser[AtomishComment] = ("""#[.][^.]*[.]""".r | "#;[^\\n\\r]*".r) ^^ { AtomishComment(_) }
}
