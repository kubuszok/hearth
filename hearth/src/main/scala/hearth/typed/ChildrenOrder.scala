package hearth
package typed

/** The order in which [[Types.TypeModule.directChildrenInOrder]] lists the subtypes of a sealed hierarchy or enum.
  *
  * @since 0.4.5
  */
sealed trait ChildrenOrder extends Product with Serializable
object ChildrenOrder {

  /** The order in which the subtypes are declared, whenever the compiler knows it - the order of
    * [[Types.TypeModule.directChildren]].
    *
    * On Scala 3 this is always the declaration order (the order `Mirror.SumOf` uses). On Scala 2 it is the declaration
    * order only for a sealed type compiled in the same compilation run as the macro expansion: class files store the
    * subtypes sorted by name, so for a sealed type from another module, or one not recompiled by an incremental build,
    * the subtypes come sorted by name. Use [[ByName]] when the result has to be the same in every build.
    */
  case object DeclarationWhenAvailable extends ChildrenOrder

  /** Sorted by the subtype's name: its simple name (in natural order, so `Item2` comes before `Item10`), then its
    * full name.
    *
    * The same on Scala 2 and Scala 3, whether the sealed type was compiled in the same run or read from class files.
    */
  case object ByName extends ChildrenOrder
}
