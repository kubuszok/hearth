/** Shared utilities for generating arity-based Hearth source code.
  *
  * Used by TypeConstructorsGen, CrossQuotesMacrosGen, and CrossCtorTestGen.
  */
object ArityGen {

  val maxArity: Int = 22

  /** Type parameter names: A, B, C, ..., V (22 letters) */
  val typeParamNames: IndexedSeq[String] = ('A' to 'V').map(_.toString)

  /** Get the type parameter name for position i (0-based). */
  def paramName(i: Int): String = typeParamNames(i)

  /** For arity N, return the type parameter names: A, B, C, ... */
  def params(n: Int): Seq[String] = typeParamNames.take(n)

  /** Lower bound name for position i (1-based): L1, L2, ... */
  def lower(i: Int): String = s"L$i"

  /** Upper bound name for position i (1-based): U1, U2, ... */
  def upper(i: Int): String = s"U$i"

  /** Generate comma-separated bounds pairs: "L1, U1 >: L1, L2, U2 >: L2, ..." for positions 1..n */
  def boundsPairs(n: Int): String =
    (1 to n).map(i => s"${lower(i)}, ${upper(i)} >: ${lower(i)}").mkString(", ")

  /** Generate HKT type parameter list: "_ >: L1 <: U1, _ >: L2 <: U2, ..." for positions 1..n */
  def hktSlots(n: Int): String =
    (1 to n).map(i => s"_ >: ${lower(i)} <: ${upper(i)}").mkString(", ")

  /** Generate simple HKT slots for the non-bounded version: "_, _, _" for arity n */
  def simpleHktSlots(n: Int): String = Seq.fill(n)("_").mkString(", ")

  /** Generate question marks for Stub type: "?, ?, ?" for arity n */
  def questionMarks(n: Int): String = Seq.fill(n)("?").mkString(", ")

  /** Generate "Nothing, Any, Nothing, Any, ..." for arity n (2n items) */
  def nothingAnyPairs(n: Int): String =
    (1 to n).map(_ => "Nothing, Any").mkString(", ")

  /** Generate apply parameter list: "A >: L1 <: U1: Type, B >: L2 <: U2: Type, ..." */
  def applyParams(n: Int): String =
    (0 until n).map { i =>
      val p = paramName(i)
      val j = i + 1
      s"$p >: ${lower(j)} <: ${upper(j)}: Type"
    }.mkString(", ")

  /** Generate the list of parameter names only: "A, B, C, ..." */
  def paramNameList(n: Int): String = params(n).mkString(", ")

  /** Generate bounds tuple type: "(L1 <:??<: U1, L2 <:??<: U2, ...)" or single bound "L1 <:??<: U1" */
  def boundsTuple(n: Int): String =
    if (n == 1) s"(${lower(1)} <:??<: ${upper(1)})"
    else (1 to n).map(i => s"${lower(i)} <:??<: ${upper(i)}").mkString("(", ", ", ")")

  /** Generate UpperBounded bound pairs: "U1, U2, ..." for positions 1..n */
  def upperBoundedParams(n: Int): String = (1 to n).map(i => upper(i)).mkString(", ")

  /** Generate UpperBounded HKT slots: "_ <: U1, _ <: U2, ..." */
  def upperBoundedHktSlots(n: Int): String =
    (1 to n).map(i => s"_ <: ${upper(i)}").mkString(", ")

  /** Generate "Nothing, U1, Nothing, U2, ..." for UpperBounded type alias */
  def nothingUpperPairs(n: Int): String =
    (1 to n).map(i => s"Nothing, ${upper(i)}").mkString(", ")

  /** "Ctor" prefix plus the arity number: "Ctor1", "Ctor2", ... */
  def ctorName(n: Int): String = s"Ctor$n"

  /** Write content to file only if changed, returns the file. */
  def writeIfChanged(file: sbt.File, content: String): sbt.File = {
    val existing = if (file.exists()) sbt.IO.read(file) else ""
    if (existing != content) {
      sbt.IO.write(file, content)
    }
    file
  }
}
