package hearth.demo.allfeatures

private[allfeatures] trait FastShowPrettyCompanionCompat { this: FastShowPretty.type =>

  /** Renders a value to a String with custom indentation configuration. */
  inline def render[A](inline value: A, config: RenderConfig): String = ${
    internal.compiletime.FastShowPrettyMacros.deriveInlineImpl[A]('value, 'config, '{ config.startLevel })
  }

  inline given derived[A]: FastShowPretty[A] = ${ internal.compiletime.FastShowPrettyMacros.deriveTypeClassImpl[A] }
}
