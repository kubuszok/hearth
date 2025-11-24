package hearth
package std

/** Standard macro extension.
  *
  * Intended to register Standard [[StdExtensions.IsCollection]], [[StdExtensions.IsOption]],
  * [[StdExtensions.IsWrapper]] providers.
  *
  * @since 0.3.0
  */
trait StandardMacroExtension extends MacroExtension[MacroCommons & StdExtensions]
