package hearth

import scala.reflect.{classTag, ClassTag}

/** Macro extension.
  *
  * Allows defining a new class, which would be loaded inside a macro using [[java.util.ServiceLoader]] mechanism.
  *
  * The class should be defined as abstract class, applying the type of the macro to extend. Then each extension would
  * be a concrete class, extending the abstract class and implementing the `extend` method.
  *
  * Each artifact containing should have a `META-INF/services/name.of.the.extension` file, which would contain one or
  * more lines, each containing the fully qualified name of the extension class.
  *
  * Extension are loaded using [[Environments.loadMacroExtensions]] method. Since [[MacroExtension.extend]] is of
  * [[Unit]] type, they have to mutate the macro context in some way, so users should make sure that the order of
  * loading extension would not matter.
  *
  * @since 0.1.0
  *
  * @tparam Macro
  *   the type of the macro to extend
  *
  * @see
  *   [[Environments.loadMacroExtensions]]
  */
abstract class MacroExtension[Macro: ClassTag] extends PartialFunction[Any, Unit] {
  private val Macro = classTag[Macro].runtimeClass.asInstanceOf[Class[Macro]]

  final def isDefinedAt(ctx: Any): Boolean = Macro.isAssignableFrom(ctx.getClass)

  final def apply(ctx: Any): Unit = extend(Macro.cast(ctx))

  def extend(ctx: Macro): Unit
}
