import net.flaviusb.atomish._

var u = new PreUniverse()
var r = new PreReader()
var e = PreEvaller.eval(u) _

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

u.roots("System") = AtomishOrigin(MMap[String, AtomishThing](
  "programArguments" -> AtomishArray()
))

u.roots("FileSystem") = AtomishOrigin(MMap[String, AtomishThing](
  "cwd"              -> AtomishString((new File(".")).getAbsoluteFile().getParent()),
  "exists?"          -> AlienProxy(_.args match {
    case List(Left(AtomishString(file_name))) => AtomishBoolean((new File(u.roots("FileSystem").cells("cwd").asInstanceOf[AtomishString].value, file_name)).exists)
    case _                                    => AtomishUnset //Should soft error
  }),
  "removeFile!"      -> AlienProxy(_.args match {
    case List(Left(AtomishString(file_name))) => AtomishBoolean((new File(u.roots("FileSystem").cells("cwd").asInstanceOf[AtomishString].value, file_name)).delete())
  }),
  "parentOf"         -> AlienProxy(_.args match {
    case List(Left(AtomishString(file_name))) => AtomishString((new File(u.roots("FileSystem").cells("cwd").asInstanceOf[AtomishString].value, file_name)).getParent())
  })
))

