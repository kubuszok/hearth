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
  * [[Unit]] type, they have to mutate the macro context in some way, so extensions should be written to be
  * order-independent wherever possible. When ordering genuinely matters (e.g. a specialized provider that must take
  * precedence over a more general one), use [[priority]] to control it - extensions are applied highest-priority-first,
  * and same-priority extensions keep their discovery order.
  *
  * Choosing a mechanism: to add support for a container-''shaped'' type (a collection, map, option, either, value type,
  * or smart constructor) prefer registering a provider through [[std.StandardMacroExtension]] - the `Is*` shape
  * companions already drive built-in derivation. Note these shapes are '''fixed-arity''': `IsCollection`/`IsOption`/
  * `IsValueType` expose a '''single''' inner type and `IsMap`/`IsEither` exactly two; they cannot model "two inners
  * plus an outer", recursive derivation, or a total/partial split. When your extension point needs those, define your
  * own `MacroExtension[YourEngine]` subtype so the loaded extension receives your engine's context instead of a fixed
  * shape. See the mkdocs docs/user-guide/standard-extensions.md guide.
  *
  * @see
  *   [[Environments.loadMacroExtensions]]
  *
  * @since 0.1.0
  *
  * @tparam Macro
  *   the type of the macro to extend
  */
abstract class MacroExtension[Macro: ClassTag] extends PartialFunction[Any, Unit] {
  private val Macro = classTag[Macro].runtimeClass.asInstanceOf[Class[Macro]]

  final def isDefinedAt(ctx: Any): Boolean = Macro.isAssignableFrom(ctx.getClass)

  final def apply(ctx: Any): Unit = extend(Macro.cast(ctx))

  /** Ordering hint for [[Environments.loadMacroExtensions]]: extensions are applied highest-priority-first, and
    * same-priority extensions (the default is `0`) keep their service-discovery order. Override with a higher value
    * only when this extension must run before others (e.g. to register a more specific provider that should win over a
    * general one); prefer writing order-independent extensions otherwise.
    *
    * @since 0.1.0
    */
  def priority: Int = 0

  /** The extension point every concrete extension implements: mutate the macro context `ctx` to register whatever this
    * extension contributes (e.g. a provider on one of the `Is*` shape companions).
    *
    * Because the return type is [[Unit]] the only observable effect is the mutation of `ctx`, and extensions are
    * discovered independently, so the body should be '''idempotent''' (applying it twice must not corrupt the context)
    * and '''order-independent''' with respect to other extensions. When one extension genuinely must run before
    * another, express that through [[priority]] rather than relying on discovery order.
    *
    * @since 0.1.0
    *
    * @param ctx
    *   the macro context to mutate
    */
  def extend(ctx: Macro): Unit
}
