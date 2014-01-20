package net.flaviusb.atomish

import scala.util.parsing.combinator._
import scala.collection.mutable.{Map => MMap, MutableList => MList}

import scala.language.postfixOps

class PreReader {
  def read(str: AtomishString): AtomishCode = {
    AtomishParser.parseAll(AtomishParser.code, str.value) match {
      case AtomishParser.Success(atomval, _) => atomval
      case AtomishParser.Failure(msg, next)  => { println(msg); println(next.toString()); null } // Should trigger condition system
      case AtomishParser.Error(msg, wtf)     => { println(msg); println(wtf.toString()); null }  // Should trigger condition system
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
  def dinteger  = "[+-]?[0-9]+".r ^^ { (int: String) => AtomishInt(int.toInt) }
  def hinteger  = "0x" ~ "[0-9a-fA-F]+".r ^^ { case "0x" ~ (int: String) => AtomishInt(Integer.parseInt(int, 16)) }
  def integer  = (hinteger | dinteger)
  // In the actual AtomishLanguage, flagcheck should be implemented as a reader macro
  def flagcheck = "#" ~ "[_+:]*[a-zA-Z][a-zA-Z0-9_:$!?%=<>-]*".r ^^ {
    case "#" ~ flagName => {
      AtomishCall("flag", Array(AtomishString(flagName)))
    }
  }
  def regex_escapes = ("""\/""" | """\\""" | """\n""" | """\r""") ^^ {
    case """\\""" => """\"""
    case """\n""" => "\n"
    case """\r""" => "\r"
    case """\/""" => "/"
  }
  def regex    = ("/" ~ ((regex_escapes | """[^/\\]""".r)*) ~ "/" ~ (("[a-zA-Z]".r)*)) ^^ {
    case "/" ~ regex_chunks ~ "/" ~ flags => AtomishRegex(regex_chunks.mkString, flags.distinct)
  }
  def at_square = ("[" ~ code ~ "]") ^^ { case "[" ~ the_code ~ "]" => AtomishCall("at", Array(the_code)) }
  def qstring_escapes = ("""\\""" | """\n""" | """\"""" | """\r""") ^^ {
    case """\\""" => """\"""
    case """\n""" => "\n"
    case """\r""" => "\r"
    case """\"""" => "\""
  }
  def sstring_escapes = ("""\\""" | """\n""" | """\]""" | """\r""") ^^ {
    case """\\""" => """\"""
    case """\n""" => "\n"
    case """\r""" => "\r"
    case """\]""" => "]"
  }
  def interpolated_section: Parser[AtomishCode] = "#{" ~ code ~ "}" ^^ { case "#{" ~ interpolated_code ~ "}" => interpolated_code }
  def qstring   = ("\"" ~ ((interpolated_section | (("""([^"\\])""".r | qstring_escapes) ^^ { AtomishString(_) }))*) ~ "\"") ^^ {
    case "\"" ~ List() ~  "\"" => AtomishString("")
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
  def symbol: Parser[AtomishCall] = ":" ~ identifier ^^ { case ":" ~ symb => AtomishCall(":", Array(symb)) }
  def identifier: Parser[AtomishMessage] = ("([_+]+[_+:]*)?[a-zA-Z][a-zA-Z0-9_:$!?%=<>-]*".r | "[~!@$%^&*_=\'`/?×÷≠→←⇒⇐⧺⧻§∘≢∨∪∩□∀⊃∈+<>-]+".r | "[]"
    | "{}" | "…") ^^ { AtomishMessage(_) }
  def code_tiny_bit: Parser[AtomishCode] = (comment | at_square | regex | commated | atomish_call | string | rational | integer | symbol | identifier
    | flagcheck | nll) // This will eventually be a big union of all types that can constitute standalone code
  def code_bit: Parser[List[AtomishCode]] = (((wss*) ~ code_tiny_bit)*) ^^ { _.flatMap { 
    case x ~ (code_piece: AtomishCode)         => List(code_piece)
  } }
  def code: Parser[AtomishCode] = code_tiny_bit ~ code_bit ^^ {
    case a ~ b => {
      if((b == List(AtomishNL)) || (b == List())) {
        a
      } else {
        AtomishForm(a::b)
      }
    }
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
  def comment: Parser[AtomishComment] = ("""#[.][^.]*[.]""".r | "#;[^\\n\\r]*".r | "؟[^\\n\\r]*".r | "#!/[^\\n\\r]*".r) ^^ { AtomishComment(_) }
}
