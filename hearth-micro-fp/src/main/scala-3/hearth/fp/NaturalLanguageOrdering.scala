package hearth
package fp

/** Natural Language Ordering.
  *
  * Compares numbers not lexicographically but by their value, ignores spaces and separators.
  *
  * Based on https://github.com/paour/natorder and https://github.com/616slayer616/natorder.
  */
final class NaturalLanguageOrdering private (caseSensitive: Boolean) extends Ordering[String] {
  import NaturalLanguageOrdering.ZerosSkippedResult

  def compare(a: String, b: String): Int = {

    // Unavoidable allocations.
    val aNumber = new StringBuilder
    val bNumber = new StringBuilder
    given ZerosSkippedResult = ZerosSkippedResult.allocate

    @scala.annotation.tailrec
    def loop(aIdxFrom: Int, bIdxFrom: Int): Int = {
      // Only count the number of zeroes leading the last number compared.
      val ZerosSkippedResult(aIdx, aChar, aZeroes) =
        skipLeadingSpacesOrZeroes(a, aIdxFrom, aNumber)
      val ZerosSkippedResult(bIdx, bChar, bZeroes) =
        skipLeadingSpacesOrZeroes(b, bIdxFrom, bNumber)

      if aChar.isDigitOrSeparator then aNumber.append(aChar)
      if bChar.isDigitOrSeparator then bNumber.append(bChar)

      lazy val magnitudeComparison = compareNumberMagnitude(a, b, aIdx, bIdx)

      // We have 2 digits of different magnitude, so we take the longer.
      if aChar.isDigit && bChar.isDigit && magnitudeComparison != 0 then magnitudeComparison
      // We reached the end of both Strings, and they are "equal" so far, so use a different means of comparison.
      else if aChar == 0 && bChar == 0 then compareEqual(a, b, aZeroes, bZeroes, aNumber, bNumber)
      // If chars are different, so one of them is smaller.
      // Otherwise, we haven't reached the end of either String, characters so far were either equal or skipped.
      else aChar.compareOr(bChar, loop(aIdx + 1, bIdx + 1))
    }

    loop(0, 0)
  }

  // All utilities are inlined to help JVM remove the overhead.

  /** Skip leading spaces and zeroes. */
  private inline def skipLeadingSpacesOrZeroes(inline s: String, inline idxFrom: Int, inline number: StringBuilder)(
      using ZerosSkippedResult
  ): ZerosSkippedResult = {

    @scala.annotation.tailrec
    def loop(idx: Int, zeroes: Int): ZerosSkippedResult = {
      val char = s.guardedCharAt(idx)
      if char.isSpaceOrSeparatorOrZero then {
        if char.isZero then {
          number.append(char)
          loop(idx + 1, zeroes + 1)
        } else loop(idx + 1, 0) // Only count consecutive zeroes
      } else ZerosSkippedResult(idx, char, zeroes)
    }

    loop(idxFrom, 0)
  }

  /** Prefixes are the same, so compare the rest of the strings.
    *
    * The longest run of digits wins. That aside, the greatest value wins, but we can't know that it will until we've
    * scanned both numbers to know that they have the same magnitude, so we remember it in BIAS.
    */
  private inline def compareNumberMagnitude(a: String, b: String, aIdx0: Int, bIdx0: Int): Int = {

    @scala.annotation.tailrec
    def loop(bias: Int, aIdx: Int, bIdx: Int): Int = {
      val aChar = a.guardedCharAt(aIdx)
      val bChar = b.guardedCharAt(bIdx)

      val notADigit = !aChar.isSpaceOrSeparator
      val notBDigit = !bChar.isSpaceOrSeparator

      if notADigit && notBDigit then bias
      else if notADigit then -1
      else if notBDigit then +1
      else if aChar == 0 && bChar == 0 then bias
      else
        loop(
          if bias == 0 then aChar.compareOr(bChar, bias) else bias,
          aIdx + 1,
          bIdx + 1
        )
    }

    loop(0, aIdx0, bIdx0)
  }

  /** Strings are the same according to comparison so far, so let's fall back to other means.
    */
  private inline def compareEqual(
      a: String,
      b: String,
      aZeroes: Int,
      bZeroes: Int,
      aNumber: StringBuilder,
      bNumber: StringBuilder
  ): Int =
    if !aNumber.sameContent(bNumber) then aNumber.compareAsDouble(bNumber)
    else if aZeroes - bZeroes != 0 then aZeroes - bZeroes
    else if a.length != b.length then a.length - b.length
    else if caseSensitive then a.compareTo(b)
    else a.compareToIgnoreCase(b)

  extension (s: String) {
    private inline def guardedCharAt(i: Int): Char =
      if i >= s.length then 0
      else if caseSensitive then s.charAt(i)
      else s.charAt(i).toLower
  }

  extension (inline sb: StringBuilder) {
    private inline def sameContent(inline sb2: StringBuilder): Boolean = sb.underlying.compareTo(sb2.underlying) == 0
    private inline def compareAsDouble(inline sb2: StringBuilder): Int = toDouble.compareTo(sb2.toDouble)
    private inline def toDouble: Double = sb.toString.toDouble
  }

  extension (c: Char) {
    private inline def isDigitOrSeparator: Boolean = c.isDigit || c == '.' || c == ',' || c == ':'
    private inline def isZero: Boolean = c == '0'
    private inline def isSpaceOrSeparator: Boolean = c.isSpaceChar || c == '_'
    private inline def isSpaceOrSeparatorOrZero: Boolean = isSpaceOrSeparator || isZero
    private inline def compareOr(other: Char, inline whenEqual: Int): Int =
      if c < other then -1 else if c > other then +1 else whenEqual
  }
}
object NaturalLanguageOrdering {

  val caseSensitive: NaturalLanguageOrdering = new NaturalLanguageOrdering(true)
  val caseInsensitive: NaturalLanguageOrdering = new NaturalLanguageOrdering(false)

  def apply(isCaseSensitive: Boolean = true): NaturalLanguageOrdering =
    if isCaseSensitive then caseSensitive else caseInsensitive

  /** Like a tuple (Int, Char, Int) but specialized to avoid boxing AND allocate only once.
    *
    * We are allocating it once, then we pass it to the method, and let it return the result without reallocating.
    *
    * Extraction is done with allocation-free approach available on Scala 3 (_1, _2, ... instead of unapply returning
    * tuple).
    */
  final private case class ZerosSkippedResult private (
      var idx: Int,
      var char: Char,
      var zeroes: Int
  )
  private object ZerosSkippedResult {

    def allocate: ZerosSkippedResult = new ZerosSkippedResult(0, 0, 0)

    /** Set the result and return `zs`. */
    inline def apply(inline idx: Int, inline char: Char, inline zeroes: Int)(using
        zs: ZerosSkippedResult
    ): ZerosSkippedResult = {
      zs.idx = idx
      zs.char = char
      zs.zeroes = zeroes
      zs
    }
  }
}
