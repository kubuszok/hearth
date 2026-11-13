package hearth

trait MacroUntypedCommonsScala2
    extends MacroUntypedCommons
    with untyped.UntypedTypesScala2
    with untyped.UntypedExprsScala2
    with untyped.UntypedMethodsScala2 { this: MacroCommonsScala2 => }

object MacroUntypedCommonsScala2 {

  /** Matches a [[MacroCommons]] (or any other value) that is the Scala 2 implementation, so that a macro extension
    * shared between platforms can use the Scala 2-specific API when it runs on Scala 2:
    *
    * {{{
    * commons match {
    *   case MacroUntypedCommonsScala2(platformCommons) => // platformCommons has the platform-specific API
    *   case _ => // not on Scala 2
    * }
    * }}}
    *
    * @since 0.4.4
    */
  def unapply[A](commons: A): Option[A with MacroUntypedCommonsScala2] = commons match {
    case platformCommons: MacroUntypedCommonsScala2 =>
      Some(platformCommons.asInstanceOf[A with MacroUntypedCommonsScala2])
    case _ => None
  }
}

trait MacroTypedCommonsScala2
    extends MacroTypedCommons
    with typed.TypesScala2
    with typed.ExprsScala2
    with typed.MethodsScala2 { this: MacroCommonsScala2 => }

object MacroTypedCommonsScala2 {

  /** Matches a [[MacroCommons]] (or any other value) that is the Scala 2 implementation, so that a macro extension
    * shared between platforms can use the Scala 2-specific API when it runs on Scala 2:
    *
    * {{{
    * commons match {
    *   case MacroTypedCommonsScala2(platformCommons) => // platformCommons has the platform-specific API
    *   case _ => // not on Scala 2
    * }
    * }}}
    *
    * @since 0.4.4
    */
  def unapply[A](commons: A): Option[A with MacroTypedCommonsScala2] = commons match {
    case platformCommons: MacroTypedCommonsScala2 => Some(platformCommons.asInstanceOf[A with MacroTypedCommonsScala2])
    case _                                        => None
  }
}

trait MacroCommonsScala2
    extends MacroCommons
    with EnvironmentsScala2
    with MacroUntypedCommonsScala2
    with MacroTypedCommonsScala2
    with treeprinter.ShowCodePrettyScala2
    with treeprinter.RuntimeAwareTypePrinterScala2
    with std.StdExtensions {

  val c: scala.reflect.macros.blackbox.Context
}

object MacroCommonsScala2 {

  /** Matches a [[MacroCommons]] (or any other value) that is the Scala 2 implementation, so that a macro extension
    * shared between platforms can use the Scala 2-specific API when it runs on Scala 2:
    *
    * {{{
    * commons match {
    *   case MacroCommonsScala2(platformCommons) => // platformCommons has the platform-specific API
    *   case _ => // not on Scala 2
    * }
    * }}}
    *
    * @since 0.4.4
    */
  def unapply[A](commons: A): Option[A with MacroCommonsScala2] = commons match {
    case platformCommons: MacroCommonsScala2 => Some(platformCommons.asInstanceOf[A with MacroCommonsScala2])
    case _                                   => None
  }
}
