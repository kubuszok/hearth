package hearth
package std

/** Standard macro extension.
  *
  * Intended to register Standard [[StdExtensions.IsCollection]], [[StdExtensions.IsOption]],
  * [[StdExtensions.IsValueType]] providers.
  *
  * This is the register-a-provider SPI: a library extends it to teach the built-in shape companions about its own types
  * (e.g. add `IsCollection` support for a third-party collection). It is bounded by the fixed-arity shapes -
  * `IsCollection`/`IsOption`/`IsValueType` expose a single inner type, `IsMap`/`IsEither` exactly two - so for anything
  * needing N inner types, recursion, or a total/partial split, define an engine-aware [[MacroExtension]] instead. See
  * the mechanism-choosing guidance on [[MacroExtension]] and the mkdocs docs/user-guide/standard-extensions.md guide.
  *
  * @see
  *   [[MacroExtension]]
  *
  * @since 0.3.0
  */
trait StandardMacroExtension extends MacroExtension[MacroCommons & StdExtensions]
