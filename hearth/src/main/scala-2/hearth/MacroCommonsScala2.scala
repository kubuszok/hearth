package hearth

trait MacroUntypedCommonsScala2
    extends MacroUntypedCommons
    with untyped.UntypedTypesScala2
    with untyped.UntypedExprsScala2
    with untyped.UntypedMethodsScala2 { this: MacroCommonsScala2 => }

trait MacroTypedCommonsScala2
    extends MacroTypedCommons
    with typed.TypesScala2
    with typed.ExprsScala2
    with typed.MethodsScala2 { this: MacroCommonsScala2 => }

trait MacroCommonsScala2
    extends MacroCommons
    with EnvironmentsScala2
    with MacroUntypedCommonsScala2
    with MacroTypedCommonsScala2
    with treeprinter.ShowCodePrettyScala2
    with std.StdExtensions {

  val c: scala.reflect.macros.blackbox.Context
}
