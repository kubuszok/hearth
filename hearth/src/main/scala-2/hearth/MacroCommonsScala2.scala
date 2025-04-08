package hearth

trait MacroUntypedCommonsScala2
    extends MacroUntypedCommons
    with untyped.TypesScala2
    with untyped.ExprsScala2
    with untyped.MethodsScala2 { this: MacroCommonsScala2 => }

trait MacroTypedCommonsScala2
    extends MacroTypedCommons
    with typed.TypesScala2
    with typed.ExprsScala2
    with typed.MethodsScala2 { this: MacroCommonsScala2 => }

trait MacroCommonsScala2
    extends MacroCommons
    with EnvironmentsScala2
    with MacroUntypedCommonsScala2
    with MacroTypedCommonsScala2 {

  val c: scala.reflect.macros.blackbox.Context
}
