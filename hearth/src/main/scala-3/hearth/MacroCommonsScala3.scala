package hearth

trait MacroUntypedCommonsScala3
    extends MacroUntypedCommons
    with untyped.UntypedTypesScala3
    with untyped.UntypedExprsScala3
    with untyped.UntypedMethodsScala3 { this: MacroCommonsScala3 => }

object MacroUntypedCommonsScala3 {

  def apply()(using quotes: scala.quoted.Quotes): MacroUntypedCommonsScala3 =
    new MacroCommonsScala3

  /** Matches a [[MacroCommons]] (or any other value) that is the Scala 3 implementation, so that a macro extension
    * shared between platforms can use the Scala 3-specific API when it runs on Scala 3:
    *
    * {{{
    * commons match {
    *   case MacroUntypedCommonsScala3(platformCommons) => // platformCommons has the platform-specific API
    *   case _ => // not on Scala 3
    * }
    * }}}
    *
    * @since 0.4.4
    */
  def unapply[A](commons: A): Option[A & MacroUntypedCommonsScala3] = commons match {
    case platformCommons: MacroUntypedCommonsScala3 => Some(platformCommons.asInstanceOf[A & MacroUntypedCommonsScala3])
    case _                                          => None
  }
}

trait MacroTypedCommonsScala3
    extends MacroTypedCommons
    with typed.TypesScala3
    with typed.ExprsScala3
    with typed.MethodsScala3 { this: MacroCommonsScala3 => }

object MacroTypedCommonsScala3 {

  def apply()(using quotes: scala.quoted.Quotes): MacroTypedCommonsScala3 =
    new MacroCommonsScala3

  /** Matches a [[MacroCommons]] (or any other value) that is the Scala 3 implementation, so that a macro extension
    * shared between platforms can use the Scala 3-specific API when it runs on Scala 3:
    *
    * {{{
    * commons match {
    *   case MacroTypedCommonsScala3(platformCommons) => // platformCommons has the platform-specific API
    *   case _ => // not on Scala 3
    * }
    * }}}
    *
    * @since 0.4.4
    */
  def unapply[A](commons: A): Option[A & MacroTypedCommonsScala3] = commons match {
    case platformCommons: MacroTypedCommonsScala3 => Some(platformCommons.asInstanceOf[A & MacroTypedCommonsScala3])
    case _                                        => None
  }
}

class MacroCommonsScala3(using val quotes: scala.quoted.Quotes)
    extends MacroCommons
    with EnvironmentsScala3
    with MacroUntypedCommonsScala3
    with MacroTypedCommonsScala3
    with treeprinter.ShowCodePrettyScala3
    with treeprinter.RuntimeAwareTypePrinterScala3
    with std.StdExtensions

object MacroCommonsScala3 {

  def apply()(using quotes: scala.quoted.Quotes): MacroCommonsScala3 =
    new MacroCommonsScala3

  /** Matches a [[MacroCommons]] (or any other value) that is the Scala 3 implementation, so that a macro extension
    * shared between platforms can use the Scala 3-specific API when it runs on Scala 3:
    *
    * {{{
    * commons match {
    *   case MacroCommonsScala3(platformCommons) => // platformCommons has the platform-specific API
    *   case _ => // not on Scala 3
    * }
    * }}}
    *
    * @since 0.4.4
    */
  def unapply[A](commons: A): Option[A & MacroCommonsScala3] = commons match {
    case platformCommons: MacroCommonsScala3 => Some(platformCommons.asInstanceOf[A & MacroCommonsScala3])
    case _                                   => None
  }
}
