package hearth.demo.allfeatures.internal.runtime

object FastShowPrettyUtils {

  /** Appends indentation (indentString repeated level times) to the StringBuilder. */
  def appendIndent(sb: StringBuilder, indentString: String, level: Int): StringBuilder = {
    var i = 0
    while (i < level) {
      sb.append(indentString)
      i += 1
    }
    sb
  }

  def renderBoolean(sb: StringBuilder)(value: Boolean): StringBuilder =
    if (value) sb.append("true") else sb.append("false")
  def renderByte(sb: StringBuilder)(value: Byte): StringBuilder =
    sb.append(value.toString).append(".toByte")
  def renderShort(sb: StringBuilder)(value: Short): StringBuilder =
    sb.append(value.toString).append(".toShort")
  def renderInt(sb: StringBuilder)(value: Int): StringBuilder =
    sb.append(value.toString)
  def renderLong(sb: StringBuilder)(value: Long): StringBuilder =
    sb.append(value.toString).append('L')
  def renderFloat(sb: StringBuilder)(value: Float): StringBuilder = {
    val result = value.toString
    sb.append(result)
    // Workaround for https://www.scala-js.org/doc/semantics.html#tostring-of-float-double-and-unit
    if (result.contains(".")) {
      sb.append(".0")
    }
    sb.append('f')
  }
  def renderDouble(sb: StringBuilder)(value: Double): StringBuilder = {
    val result = value.toString
    sb.append(result)
    // Workaround for https://www.scala-js.org/doc/semantics.html#tostring-of-float-double-and-unit
    if (result.contains(".")) {
      sb.append(".0")
    }
    sb.append('d')
  }
  def renderChar(sb: StringBuilder)(value: Char): StringBuilder =
    sb.append("'").append(value.toString).append("'")
  def renderString(sb: StringBuilder)(value: String): StringBuilder = {
    sb.append('"')
    var i = 0
    while (i < value.length) {
      val c = value.charAt(i)
      c match {
        case '"'  => sb.append("\\\"")
        case '\\' => sb.append("\\\\")
        case '\n' => sb.append("\\n")
        case '\r' => sb.append("\\r")
        case '\t' => sb.append("\\t")
        case _    => sb.append(c)
      }
      i += 1
    }
    sb.append('"')
  }

  /** Opens a collection rendering with the collection type name and opening bracket. */
  def openCollection(sb: StringBuilder, typeName: String): StringBuilder =
    sb.append(typeName).append("(")

  def fillCollection[A](sb: StringBuilder, iterable: Iterable[A])(renderItem: A => StringBuilder): StringBuilder = {
    val iterator = iterable.iterator
    while (iterator.hasNext) {
      val item = iterator.next()
      sb.append("\n")
      val _ = renderItem(item)
      if (iterator.hasNext) {
        sb.append(",")
      } else {
        sb.append("\n")
      }
    }
    sb
  }

  /** Appends a separator between collection elements. */
  def appendCollectionSeparator(sb: StringBuilder): StringBuilder =
    sb.append(", ")

  /** Closes a collection rendering with the closing bracket. */
  def closeCollection(sb: StringBuilder): StringBuilder =
    sb.append(")")

  /** Opens a map entry rendering. */
  def openMapEntry(sb: StringBuilder): StringBuilder =
    sb.append("(")

  /** Appends the separator between key and value in a map entry. */
  def appendMapArrow(sb: StringBuilder): StringBuilder =
    sb.append(", ")

  /** Closes a map entry rendering. */
  def closeMapEntry(sb: StringBuilder): StringBuilder =
    sb.append(")")
}
