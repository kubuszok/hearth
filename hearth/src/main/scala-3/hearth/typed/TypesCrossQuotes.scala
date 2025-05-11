package hearth
package typed

private[typed] trait TypesCrossQuotes { this: Types =>

  trait TypeCrossQuotes { this: Type.type =>

    @scala.annotation.compileTimeOnly("Install cross-quotes-plugin to use this method")
    final def of[A]: Type[A] = sys.error("Install cross-quotes-plugin to use this method")
  }
}
