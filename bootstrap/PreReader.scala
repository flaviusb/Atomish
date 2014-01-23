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
  def dinteger: Parser[AtomishCode]  = "[+-]?[0-9]+".r ^^ { (int: String) => AtomishInt(int.toInt) }
  def hinteger: Parser[AtomishCode]  = "0x" ~ "[0-9a-fA-F]+".r ^^ { case "0x" ~ (int: String) => AtomishInt(Integer.parseInt(int, 16)) }
  def integer: Parser[AtomishCode]  = (hinteger | dinteger)
  // In the actual AtomishLanguage, flagcheck should be implemented as a reader macro
  def flagcheck = "#" ~ "[_+:]*[a-zA-Z][a-zA-Z0-9_:$!?%=<>-]*".r ^^ {
    case "#" ~ flagName => {
      AtomishCall("flag", Array(AtomishString(flagName)))
    }
  }
  // In the actual AtomishLanguage, %w etc should be implemented as part of the MMOP
  def pct_w: Parser[AtomishCode] = "%w{" ~ (("[^\\s}]+".r ~ "[\\s]*".r)*) ~ "}" ^^ {
    case "%w{" ~ List() ~ "}"  => AtomishArray(Array())
    case "%w{" ~ x ~ "}"       => {
      AtomishArray(x.map(_ match {
        case value ~ _ => AtomishString(value)
      }).toArray)
    }
  }
  def commated_code = code ~ ((("," ~ code) ^^ {case x ~ y => y})*) ^^ { case x ~ y => x::y }
  def square_array: Parser[AtomishCode] = "[" ~ ((nlws*) ~ (commated_code ~ ((nlws*) ~ "]"))) ^^ {
    case "[" ~ (x ~ (y ~ (z ~ "]"))) => AtomishCall("Array", y.toArray)
  }
  def regex_escapes = ("""\/""" | """\\""" | """\n""" | """\r""") ^^ {
    case """\\""" => """\"""
    case """\n""" => "\n"
    case """\r""" => "\r"
    case """\/""" => "/"
  }
  def regex: Parser[AtomishRegex] = ("/" ~ ((regex_escapes | """[^/\\]""".r)*) ~ "/" ~ (("[a-zA-Z]".r)*)) ^^ {
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
  def qstring: Parser[AtomishCode] = ("\"" ~ ((interpolated_section | (("""([^"\\])""".r | qstring_escapes) ^^ { AtomishString(_) }))*) ~ "\"") ^^ {
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
  def sstring: Parser[AtomishCode] = ("#[" ~ ((("""([^\]\\])""".r | sstring_escapes)*) ~ "]")) ^^ { case "#[" ~ (str ~ "]") => AtomishString(str.foldLeft("")(_ + _)) }
  def string: Parser[AtomishCode] = (sstring | qstring)
  def symbol: Parser[AtomishCall] = ":" ~ identifier ^^ { case ":" ~ symb => AtomishCall(":", Array(symb)) }
  def identifier: Parser[AtomishMessage] = ("([_+]+[_+:]*)?[a-zA-Z][a-zA-Z0-9_:$!?%=<>-]*".r | "[~!@$%^&*_=\'`/?×÷≠→←⇒⇐⧺⧻§∘≢∨∪∩□∀⊃∈+<>-]+".r | "[]"
    | "{}" | "…") ^^ { AtomishMessage(_) }
  def literal = ((regex: Parser[AtomishRegex]) | string | rational | integer | symbol | pct_w | square_array)
  def lside   = (literal | rside_bit)
  def rside_bit = (comment | at_square | literal | commated | atomish_call | identifier | flagcheck)
  def rside: Parser[List[AtomishCode]]   = (rside_bit ~ ((((wss) ~ rside_bit) ^^ {
    case x ~ y => y })*)) ^^ {
      case x ~ List() => List(x)
      case (x:AtomishCode) ~ (y:List[AtomishCode])      => x +: y
  }      
  def limb: Parser[AtomishCode] = (((((wss?) ~ ((lside) ~ (wss?))) ^^ { case w ~ (x ~ y) => x })) ~ (((rside ~ (wss?)) ^^ { case x ~ y => x })?)) ^^ {
    //case None    ~ Some(List(x)) => x
    //case None    ~ Some(x)       => AtomishForm(x)
    case x ~ Some(List())  => x
    case x ~ Some(y)       => AtomishForm(x::y)
    //case None    ~ None          => AtomishForm(List())
    case x ~ None          => x
  }
  def nlws = ((wss?) ~ (nll ~ (wss?))) ^^ { case _ ~ (x ~ _) => x }
  def code : Parser[AtomishCode] = ((((nlws*) ~ (limb ~ (((
    ((((nlws+) ~ limb) ^^ {case x ~ y => x :+ y})*) ^^ { case x: List[List[AtomishCode]] => x.flatten }): Parser[List[AtomishCode]]) ~ (nlws*))
  )) ^^ {
    case n ~ (x ~ (List() ~ List())) => {
      //println(x.toString())
      x
    }
    case n ~ (x ~ (y ~ z))           => {
      val ret = AtomishForm(n ++ (x::y ++ z))
      //println(ret.toString())
      ret
    }
  }) | (((wss?) ~ (((nll ~ (wss?)) ^^ {case x ~ y => x})+)) ^^ { 
    case a ~ List(x) => x
    case a ~ x       => AtomishForm(x)
  }))
  /*def code_tiny_bit: Parser[AtomishCode] = (comment | at_square | regex | commated | atomish_call | string | rational | integer | symbol | pct_w | identifier
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
  }*/
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
