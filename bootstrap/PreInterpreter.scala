package net.flaviusb.atomish

import java.io.{File, FileInputStream, Writer, Reader, BufferedReader, StringReader, IOException, FileReader, FileWriter}
import scala.io.{BufferedSource}

import scala.collection.mutable.{Map => MMap}

object PreAtomishInterpreter {
  var u = new PreUniverse()
  var r = new PreReader()
  var e = PreEvaller.eval(u) _;
  // Stitch the parts together
  u.roots("read") = r.alien_read;
  u.roots("eval") = AlienProxy(_.args match {
    case List(Left(arg: AtomishCode))                           => e(arg, None)
    case List(Left(arg: AtomishCode), Left(base: AtomishThing)) => e(arg, Some(base))
    case _                                                      => null // Should error
  })
  u.roots("print") = AlienProxy(_.args match {
    case List(Left(arg: AtomishThing)) => AtomishString(PreScalaPrinter.print(arg))
    case _                             => null // Should error
  })
  u.roots("print_with_forms") = AlienProxy(_.args match {
    case List(Left(arg: AtomishThing)) => AtomishString(PreScalaPrinter.print_with_forms(arg))
    case _                             => null // Should error
  })
  def main(args: Array[String]): Unit = {
    if(args.length == 0) {
      println("No file specified.")
      return
    }
    var source_name = args(0);
    var file_source = new File(source_name)
    u.roots("System") = AtomishOrigin(MMap[String, AtomishThing](
      "programArguments" -> AtomishArray(args.drop(1).map(x => AtomishString(x)))
    ))
    u.roots("FileSystem") = AtomishOrigin(MMap[String, AtomishThing](
      "cwd"              -> AtomishString(file_source.getAbsoluteFile().getParent()),
      "exists?"          -> AlienProxy(_.args match {
        case List(Left(AtomishString(file_name))) => AtomishBoolean((new File(u.roots("FileSystem").cells("cwd").asInstanceOf[AtomishString].value, file_name)).exists)
        case _                                    => AtomishUnset //Should soft error
      }),
      "removeFile!"      -> AlienProxy(_.args match {
        case List(Left(AtomishString(file_name))) => AtomishBoolean((new File(u.roots("FileSystem").cells("cwd").asInstanceOf[AtomishString].value, file_name)).delete())
      }),
      "parentOf"         -> AlienProxy(_.args match {
        case List(Left(AtomishString(file_name))) => AtomishString((new File(u.roots("FileSystem").cells("cwd").asInstanceOf[AtomishString].value, file_name)).getParent())
      }),
      "withOpenFile"     -> AlienProxy(_.args match {
        case List(Left(AtomishString(file_name)), Left(lexical_thingy)) => {
          var io_file = new File(u.roots("FileSystem").cells("cwd").asInstanceOf[AtomishString].value, file_name)
          var writer = new FileWriter(io_file);
          var io = AtomishOrigin(MMap[String, AtomishThing](
            "put"   -> AlienProxy(_.args match {
              case List(Left(x)) => { writer.write(PreScalaPrinter.print(x)); AtomishUnset }
            }),
            "flush" -> AlienProxy(x => {writer.flush(); AtomishUnset})
          ))
          var ret = lexical_thingy match {
            case q: AlienProxy  => q.activate(AtomishArgs(List(Left(io))))
            case q: QAlienProxy => q.activate(AtomishCommated(Array(io)))
          }
          //io.cells("flush").asInstanceOf[AlienProxy].activate(AtomishArgs(List()))
          writer.flush()
          writer.close()
          ret
        }
      })
    ))
    AtomishThing.post_bootstrap ++= MMap[(String, String), AtomishThing => AtomishThing](
      ("Boolean", "==")      -> { thing => AlienProxy(booltobool(_ == thing.asInstanceOf[AtomishBoolean].value)) },
      ("Boolean", "and")     -> { thing => AlienProxy(booltobool(_ && thing.asInstanceOf[AtomishBoolean].value)) },
      ("Boolean", "or")      -> { thing => AlienProxy(booltobool(_ || thing.asInstanceOf[AtomishBoolean].value)) },
      ("Boolean", "not")     -> { thing => AlienProxy(a => AtomishBoolean(!thing.asInstanceOf[AtomishBoolean].value)) },
      ("Boolean", "isTrue")  -> { thing => AlienProxy(a => AtomishBoolean(thing.asInstanceOf[AtomishBoolean].value)) },
      ("Boolean", "isFalse") -> { thing => AlienProxy(a => AtomishBoolean(!thing.asInstanceOf[AtomishBoolean].value)) },
      ("Boolean", "asText")  -> { thing => AlienProxy(a => AtomishString(thing.asInstanceOf[AtomishBoolean].value.toString())) },
      ("Array", "each")      -> { thing => QAlienProxy(_.args match {
        case Array(message) => {
          thing.asInstanceOf[AtomishArray].value.foreach(inner =>
              u.roots("eval").asInstanceOf[AlienProxy].activate(AtomishArgs(List(Left(message), Left(inner))))
          )
          AtomishUnset
        }
      }) },
      ("Array", "map")       -> { thing => QAlienProxy(_.args match {
        case Array(message) => {
          AtomishArray(thing.asInstanceOf[AtomishArray].value.map(inner =>
              u.roots("eval").asInstanceOf[AlienProxy].activate(AtomishArgs(List(Left(message), Left(inner))))
          ))
        }
      }) }
    )

    var stream_source = new BufferedSource(new FileInputStream(file_source))
    var src = AtomishString(stream_source.addString(new StringBuilder(1024)).toString())
    //println(src)
    e(r.read(src), None);
  }
}
