package hearth.demo.allfeatures
package internal.compiletime

import hearth.MacroCommonsScala3
import scala.quoted.*

final private[allfeatures] class FastShowPrettyMacros(q: Quotes)
    extends MacroCommonsScala3(using q),
      FastShowPrettyMacrosImpl
private[allfeatures] object FastShowPrettyMacros {

  def deriveInlineImpl[A: Type](
      value: Expr[A],
      config: Expr[RenderConfig],
      level: Expr[Int]
  )(using q: Quotes): Expr[String] =
    new FastShowPrettyMacros(q).deriveInline[A](value, config, level)

  def deriveTypeClassImpl[A: Type](using q: Quotes): Expr[FastShowPretty[A]] =
    new FastShowPrettyMacros(q).deriveTypeClass[A]
}
