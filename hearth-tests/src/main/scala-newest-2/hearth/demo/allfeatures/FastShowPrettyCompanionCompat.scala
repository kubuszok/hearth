package hearth.demo.allfeatures

import scala.language.experimental.macros

private[allfeatures] trait FastShowPrettyCompanionCompat { this: FastShowPretty.type =>

  /** Renders a value to a String with custom indentation configuration. */
  def render[A](value: A, config: RenderConfig): String =
    macro internal.compiletime.FastShowPrettyMacros.deriveInlineWithConfigImpl[A]

  implicit def derived[A]: FastShowPretty[A] = macro internal.compiletime.FastShowPrettyMacros.deriveTypeClassImpl[A]
}
