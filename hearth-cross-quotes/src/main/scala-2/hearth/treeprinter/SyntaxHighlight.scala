package hearth
package treeprinter

// Copy-paste of https://github.com/scala/scala3/blob/main/compiler/src/scala/quoted/runtime/impl/printers/SyntaxHighlight.scala

trait SyntaxHighlight {
  def highlightKeyword(str: String): String
  def highlightTypeDef(str: String): String
  def highlightLiteral(str: String): String
  def highlightValDef(str: String): String
  def highlightOperator(str: String): String
  def highlightAnnotation(str: String): String
  def highlightString(str: String): String
  def highlightTripleQs: String
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

  def ANSI: SyntaxHighlight = new SyntaxHighlight {

    def highlightKeyword(str: String): String = KeywordColor + str + NoColor
    def highlightTypeDef(str: String): String = TypeColor + str + NoColor
    def highlightLiteral(str: String): String = LiteralColor + str + NoColor
    def highlightValDef(str: String): String = ValDefColor + str + NoColor
    def highlightOperator(str: String): String = TypeColor + str + NoColor
    def highlightAnnotation(str: String): String = AnnotationColor + str + NoColor
    def highlightString(str: String): String = StringColor + str + NoColor
    def highlightTripleQs: String = Console.RED_B + "???" + NoColor
  }

  def plain: SyntaxHighlight = new SyntaxHighlight {
    def highlightKeyword(str: String): String = str
    def highlightTypeDef(str: String): String = str
    def highlightLiteral(str: String): String = str
    def highlightValDef(str: String): String = str
    def highlightOperator(str: String): String = str
    def highlightAnnotation(str: String): String = str
    def highlightString(str: String): String = str
    def highlightTripleQs: String = "???"
  }
}
