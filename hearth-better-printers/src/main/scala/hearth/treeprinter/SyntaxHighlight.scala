package hearth
package treeprinter

// Copy-paste of https://github.com/scala/scala3/blob/main/compiler/src/scala/quoted/runtime/impl/printers/SyntaxHighlight.scala

/** Strategy for ANSI-coloring rendered Scala trees/types/exprs.
  *
  * Underpins Hearth's print trio: `prettyPrint` renders with [[SyntaxHighlight.ANSI]] (colored), while `plainPrint`
  * renders with [[SyntaxHighlight.plain]] (empty color codes) - i.e. the *same* rendering code parameterized by a
  * highlighter, so passing the no-color strategy yields the plain form.
  *
  * @see
  *   [[SyntaxHighlight.ANSI]] the colored strategy, [[SyntaxHighlight.plain]] the no-color strategy
  * @see
  *   the printing-trio contract on `Type`/`Expr`/`Method` (`prettyPrint`/`plainPrint`)
  * @see
  *   docs/user-guide/better-printers.md
  * @since 0.2.0
  */
trait SyntaxHighlight {
  def KeywordColor: String
  def TypeDefColor: String
  def LiteralColor: String
  def ValDefColor: String
  def OperatorColor: String
  def AnnotationColor: String
  def StringColor: String
  def TripleQsColor: String
  def NoColor: String

  def highlightKeyword(str: String): String = KeywordColor + str + NoColor
  def highlightTypeDef(str: String): String = TypeDefColor + str + NoColor
  def highlightLiteral(str: String): String = LiteralColor + str + NoColor
  def highlightValDef(str: String): String = ValDefColor + str + NoColor
  def highlightOperator(str: String): String = OperatorColor + str + NoColor
  def highlightAnnotation(str: String): String = AnnotationColor + str + NoColor
  def highlightString(str: String): String = StringColor + str + NoColor
  def highlightTripleQs: String = TripleQsColor + "???" + NoColor
}

object SyntaxHighlight {

  // Moved out of ANSI and removed `private`

  // Keep in sync with SyntaxHighlighting
  val NoColor = Console.RESET
  val CommentColor = Console.BLUE
  val KeywordColor = Console.YELLOW
  val ValDefColor = Console.CYAN
  val LiteralColor = Console.RED
  val StringColor = Console.GREEN
  val TypeColor = Console.MAGENTA
  val AnnotationColor = Console.MAGENTA

  /** ANSI-colored strategy - used by `prettyPrint`.
    *
    * @since 0.2.0
    */
  object ANSI extends SyntaxHighlight {
    val KeywordColor = SyntaxHighlight.KeywordColor
    val TypeDefColor = SyntaxHighlight.TypeColor
    val LiteralColor = SyntaxHighlight.LiteralColor
    val ValDefColor = SyntaxHighlight.ValDefColor
    val OperatorColor = SyntaxHighlight.TypeColor
    val AnnotationColor = SyntaxHighlight.AnnotationColor
    val StringColor = SyntaxHighlight.StringColor
    val TripleQsColor = Console.RED_B
    val NoColor = SyntaxHighlight.NoColor
  }

  /** No-op (empty-string) strategy - passing it yields the uncolored `plainPrint` form.
    *
    * @see
    *   [[ANSI]]
    * @since 0.2.0
    */
  object plain extends SyntaxHighlight {
    val KeywordColor = ""
    val TypeDefColor = ""
    val LiteralColor = ""
    val ValDefColor = ""
    val OperatorColor = ""
    val AnnotationColor = ""
    val StringColor = ""
    val TripleQsColor = ""
    val NoColor = ""
  }
}
