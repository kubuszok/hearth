package hearth
package typed

import hearth.cq.CrossQuotesMacros

import scala.language.experimental.macros

trait TypesCrossQuotes { this: Types =>

  trait TypeCrossQuotes { this: Type.type =>

    @scala.annotation.compileTimeOnly("Should have been expanded by the hearth-cross-quotes macros")
    def of[A]: Type[A] = macro CrossQuotesMacros.typeOfImpl[A]
  }
}
