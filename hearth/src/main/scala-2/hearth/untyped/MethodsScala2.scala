package hearth
package untyped

import hearth.MacroCommonsScala2

trait MethodsScala2 extends Methods { this: MacroCommonsScala2 =>

  final override type UntypedMethod = c.Symbol
}
