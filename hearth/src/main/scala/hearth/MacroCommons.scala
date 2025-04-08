package hearth

trait MacroUntypedCommons extends untyped.Types with untyped.Exprs with untyped.Methods {
  this: MacroCommons =>
}

trait MacroTypedCommons
    extends Environments
    with typed.Types
    with typed.Exprs
    with typed.Methods
    with typed.Existentials
    with typed.Classes {
  this: MacroCommons =>
}

trait MacroCommons extends MacroUntypedCommons with MacroTypedCommons
