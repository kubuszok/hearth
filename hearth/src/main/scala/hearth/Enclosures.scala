package hearth

import hearth.fp.data.NonEmptyVector

/** Lexical-scope (enclosure) inspection for the current macro expansion.
  *
  * [[enclosingScope]] returns the chain of enclosures surrounding the expansion point, from the INNERMOST (closest to
  * the expansion) OUTWARDS to the root package. This is the enabler for call-site-aware macros (macwire-style
  * dependency injection, proxy/mock generation, etc.).
  *
  * @since 0.4.0
  */
trait Enclosures { this: MacroCommons =>

  /** A single lexical enclosure (scope) surrounding the current macro expansion point.
    *
    * Obtained from [[enclosingScope]], which returns a [[hearth.fp.data.NonEmptyVector]] of enclosures ordered from the
    * INNERMOST (closest to the expansion point) OUTWARDS to the root package.
    *
    * The variants carry the enclosure's [[name]], optionally its [[fullName]] and source [[position]], and — for
    * [[Enclosure.Class]] and [[Enclosure.Object]] — a way to enumerate ([[Enclosure.Class.members]]) and CALL the
    * members of the enclosing instance.
    *
    * ==Calling an in-scope member (macwire-style)==
    *
    * {{{
    * // Inside a macro, find a no-argument member returning `Dependency` in the immediately-enclosing class/object and
    * // call it on `this`:
    * val receiverAndMembers: Option[(Expr_??, List[Method])] = enclosingScope.iterator.collectFirst {
    *   case enc: Enclosure.Class if enc.thisRef.isDefined => enc.thisRef.get -> enc.members
    *   case enc: Enclosure.Object                         => enc.thisRef     -> enc.members
    * }
    * val call: Option[Expr_??] = receiverAndMembers.flatMap { case (receiver, members) =>
    *   members.collectFirst {
    *     case m: Method.OnInstance if m.isNullary && m.knownReturning.exists(_.Underlying =:= Type[Dependency]) =>
    *       import receiver.value as self
    *       m.apply(self.asInstanceOf[Expr[m.Instance]]) match {
    *         case r: Method.Result[?] => r.build().toOption.map(_.as_??)
    *         case _                   => None
    *       }
    *   }.flatten
    * }
    * }}}
    *
    * @since 0.4.0
    */
  sealed trait Enclosure extends Product with Serializable {

    /** Decoded simple name of the enclosure (e.g. method/val/class/object/package name). */
    def name: String

    /** Fully-qualified name, when cheaply available from the underlying symbol. */
    def fullName: Option[String]

    /** Source position of the enclosure's definition, when available. */
    def position: Option[Position]

    /** A short, stable, cross-platform tag for the enclosure kind ("method", "value", "class", "object", "package"). */
    final def kind: String = this match {
      case _: Enclosure.Method  => "method"
      case _: Enclosure.Value   => "value"
      case _: Enclosure.Class   => "class"
      case _: Enclosure.Object  => "object"
      case _: Enclosure.Package => "package"
    }
  }

  /** Stable alias to the cake's `Method` type, so the nested `Enclosure.Method` case class does not shadow it. */
  type CakeMethod = Method

  object Enclosure {

    /** An enclosing `def` (or the initializer scope of a `val`/`var`/`lazy val` seen as a method owner).
      *
      * Local `val`s declared INSIDE this method body, BEFORE the macro call (the literal macwire case), are exposed
      * best-effort via [[localValues]]. This is populated only for the IMMEDIATELY-enclosing method; outer enclosing
      * methods always have an empty [[localValues]].
      *
      * '''Platform asymmetry''': on Scala 2 (`c.enclosingMethod` exposes the enclosing method body) the in-scope local
      * `val`s are discoverable. On Scala 3, `Symbol.tree` returns the method's `DefDef` but its `rhs` is `None` while
      * the method is being compiled (the body is not yet available during its own macro expansion), so [[localValues]]
      * is empty there. See [[Enclosure.LocalValue]].
      *
      * @since 0.4.0
      */
    final case class Method(
        name: String,
        fullName: Option[String],
        position: Option[Position],
        tpe: Option[??],
        localValues: List[Enclosure.LocalValue]
    ) extends Enclosure

    /** An enclosing `val`/`var`/`lazy val`.
      *
      * @since 0.4.0
      */
    final case class Value(
        name: String,
        fullName: Option[String],
        position: Option[Position],
        tpe: ??
    ) extends Enclosure

    /** An enclosing `class`/`trait`.
      *
      * [[members]] lists the callable methods/values of the enclosing type. [[thisRef]] is a `this` reference for the
      * enclosing instance, present only for the IMMEDIATELY-enclosing class (a sound `this` for an OUTER enclosing
      * class cannot be materialized cross-platform, so it is [[None]] there).
      *
      * @since 0.4.0
      */
    final case class Class(
        name: String,
        fullName: Option[String],
        position: Option[Position],
        tpe: ??,
        members: List[CakeMethod],
        thisRef: Option[Expr_??]
    ) extends Enclosure

    /** An enclosing `object`/module.
      *
      * Because an object has a stable path, [[members]] are always callable via [[thisRef]] (the module reference).
      *
      * @since 0.4.0
      */
    final case class Object(
        name: String,
        fullName: Option[String],
        position: Option[Position],
        tpe: ??,
        members: List[CakeMethod],
        thisRef: Expr_??
    ) extends Enclosure

    /** An enclosing `package`. Terminal element of the chain (the root package). Has no type. */
    final case class Package(
        name: String,
        fullName: Option[String],
        position: Option[Position]
    ) extends Enclosure

    /** A local `val` declared inside an enclosing [[Enclosure.Method]] body, BEFORE the macro call, with a reference
      * expression ([[ref]]) so it can be used as a constructor argument (the macwire use case).
      *
      * '''Platform asymmetry''': discovered only on Scala 2 today. On Scala 3 the enclosing method's body is not
      * available during its own macro expansion (`DefDef.rhs` is `None`), so [[Enclosure.Method.localValues]] is empty.
      * See [[Enclosure.Method]].
      *
      * @since 0.4.0
      */
    final case class LocalValue(name: String, tpe: ??, ref: Expr_??)
  }

  /** The chain of lexical enclosures (scopes) surrounding the current macro-expansion point, from the INNERMOST
    * (closest to the expansion) OUTWARDS to the root package.
    *
    * There is always at least one element (the root package), so the result is a [[hearth.fp.data.NonEmptyVector]].
    *
    * Synthetic/compiler-internal owners (macro-expansion wrappers, `$anonfun`, the synthetic `<root>`/`<empty>` package
    * wrappers, etc.) are filtered out so that the user-visible chain is clean and identical on Scala 2 and Scala 3.
    *
    * See [[Enclosure]] for the variants and a macwire-style member-calling example.
    *
    * @since 0.4.0
    */
  def enclosingScope: NonEmptyVector[Enclosure]
}
