package hearth
package untyped

trait MethodsScala3 extends Methods { this: MacroCommonsScala3 =>

  import quotes.*, quotes.reflect.*

  final override type UntypedMethod = Symbol
}
