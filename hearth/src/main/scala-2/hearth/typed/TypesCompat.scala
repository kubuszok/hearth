package hearth
package typed

private[typed] trait TypesCompat { this: MacroCommons =>

  trait TypeCompat { this: Type.type => }
  trait TypeCodecCompat
}
