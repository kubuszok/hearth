package hearth
package typed

/** Annotation to be used on a type to indicate that it is a cross-type implicit.
  *
  * In the future we might use it to let users indicate that some other value should be considered by Cross-Quotes as an
  * implicit to inject into the context.
  *
  * Currently, we can use it to mark and later find all definitions that would have to be manually injected by macros
  * and/or the compiler plugin.
  *
  * @since 0.3.0
  */
final class ImportedCrossTypeImplicit extends scala.annotation.StaticAnnotation
