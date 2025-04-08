package hearth

trait MacroUntypedCommonsScala3
    extends MacroUntypedCommons
    with untyped.TypesScala3
    with untyped.ExprsScala3
    with untyped.MethodsScala3 { this: MacroCommonsScala3 => }

object MacroUntypedCommonsScala3 {

  def apply()(using quotes: scala.quoted.Quotes): MacroUntypedCommonsScala3 =
    new MacroCommonsScala3
}

trait MacroTypedCommonsScala3
    extends MacroTypedCommons
    with typed.TypesScala3
    with typed.ExprsScala3
    with typed.MethodsScala3 { this: MacroCommonsScala3 => }

object MacroTypedCommonsScala3 {

  def apply()(using quotes: scala.quoted.Quotes): MacroTypedCommonsScala3 =
    new MacroCommonsScala3
}

class MacroCommonsScala3(using val quotes: scala.quoted.Quotes)
    extends MacroCommons
    with EnvironmentsScala3
    with MacroUntypedCommonsScala3
    with MacroTypedCommonsScala3

object MacroCommonsScala3 {

  def apply()(using quotes: scala.quoted.Quotes): MacroCommonsScala3 =
    new MacroCommonsScala3
}
