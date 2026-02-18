/** Generates the CrossCtorInjectionFixturesImplGen trait for hearth-tests.
  *
  * The generated trait uses a self-type `this: MacroCommons =>` to access
  * all utility methods from the hand-written MacroCommons class.
  *
  * Uses [[ArityGen]] for shared naming helpers (paramName, params, etc.).
  */
object CrossCtorTestGen {

  // ---------------------------------------------------------------------------
  // Naming helpers
  // ---------------------------------------------------------------------------

  /** Type name for arity N. */
  def typeName(n: Int): String = n match {
    case 1 => "Option"
    case 2 => "Either"
    case _ => s"Arity$n"
  }

  /** Variable prefix for data map keys */
  def keyPrefix(n: Int): String = n match {
    case 1 => "option"
    case 2 => "either"
    case _ => s"arity$n"
  }

  /** Variable name for the Ctor */
  def ctorVarName(n: Int): String = n match {
    case 1 => "OptionCtor"
    case 2 => "EitherCtor"
    case _ => s"Arity${n}Ctor"
  }

  /** Container trait name */
  def containerName(n: Int): String = n match {
    case 1 => "Container"
    case _ => s"Container$n"
  }

  /** N copies of "Int" comma-separated */
  def ints(n: Int): String = Seq.fill(n)("Int").mkString(", ")

  /** N copies of "Type.of[Int]" comma-separated */
  def typeOfInts(n: Int): String = Seq.fill(n)("Type.of[Int]").mkString(", ")

  /** Lowercase version of key prefix for fromUntyped local vars */
  def fromVar(n: Int): String = n match {
    case 1 => "option"
    case 2 => "either"
    case _ => s"arity$n"
  }

  // ---------------------------------------------------------------------------
  // Main entry point
  // ---------------------------------------------------------------------------

  def generate(maxArity: Int = ArityGen.maxArity): String = {
    val sb = new StringBuilder
    sb ++= fileHeader
    sb ++= "\n"
    sb ++= genTestCtorAsUntyped(maxArity)
    sb ++= "\n"
    sb ++= genTestCtorSetAsUntyped(maxArity)
    sb ++= "\n"
    // testTypeOfWithCtor1Higher is hand-written (it has extra functorOfResult line)
    for (n <- 2 to maxArity) { sb ++= genTestTypeOfWithCtorNHigher(n); sb ++= "\n" }
    for (n <- 1 to maxArity) { sb ++= genTestExprQuoteWithCtorNBody(n); sb ++= "\n" }
    sb ++= genMakeCtorResultContainer()
    sb ++= "\n"
    for (n <- 2 to maxArity) { sb ++= genMakeCtorNResultContainer(n); sb ++= "\n" }
    sb ++= genCtorResultContainerClass()
    sb ++= "\n"
    for (n <- 2 to maxArity) { sb ++= genCtorNResultContainerClass(n); sb ++= "\n" }
    sb ++= genTestCtorFromUntyped(maxArity)
    sb ++= "\n"
    for (n <- 1 to maxArity) { sb ++= genVerifyExtractedCtorN(n); sb ++= "\n" }
    sb ++= genTestCtorSetApplyUnapplyRange(2, 8)
    sb ++= "\n"
    sb ++= genTestCtorSetApplyUnapplyRange(9, 15)
    sb ++= "\n"
    sb ++= genTestCtorSetApplyUnapplyRange(16, 22)
    sb ++= "\n"
    sb ++= genTestCtorSetMiddleAsUntypedRange(3, 7)
    sb ++= "\n"
    sb ++= genTestCtorSetMiddleAsUntypedRange(8, 12)
    sb ++= "\n"
    sb ++= genTestCtorSetMiddleAsUntypedRange(13, 17)
    sb ++= "\n"
    sb ++= genTestCtorSetMiddleAsUntypedRange(18, 22)
    sb ++= "\n"
    sb ++= fileFooter
    sb.toString
  }

  // ---------------------------------------------------------------------------
  // File header / footer
  // ---------------------------------------------------------------------------

  private val fileHeader: String =
    s"""// AUTO-GENERATED — DO NOT EDIT
       |// format: off
       |package hearth
       |package crossquotes
       |
       |import hearth.data.Data
       |import hearth.examples.kinds.*
       |
       |trait CrossCtorInjectionFixturesImplGen { this: MacroCommons =>
       |""".stripMargin

  private val fileFooter: String =
    s"""}
       |""".stripMargin

  // ---------------------------------------------------------------------------
  // 1. testCtorAsUntyped
  // ---------------------------------------------------------------------------

  private def genTestCtorAsUntyped(maxArity: Int): String = {
    val sb = new StringBuilder
    sb ++= "  /** Test: Type.CtorN.asUntyped returns correct representation for all arities. */\n"
    sb ++= "  def testCtorAsUntyped: Expr[Data] = {\n"
    for (n <- 1 to maxArity) {
      val v = ctorVarName(n)
      val t = typeName(n)
      sb ++= s"    val $v: Type.Ctor$n[$t] = Type.Ctor$n.of[$t]\n"
    }
    sb ++= "\n"
    sb ++= "    Expr(\n"
    sb ++= "      Data.map(\n"
    for (n <- 1 to maxArity) {
      val k = keyPrefix(n)
      val v = ctorVarName(n)
      val comma = if (n < maxArity) "," else ""
      sb ++= s"""        "${k}CtorUntyped" -> Data(UntypedType.plainPrint($v.asUntyped))$comma\n"""
    }
    sb ++= "      )\n"
    sb ++= "    )\n"
    sb ++= "  }\n"
    sb.toString
  }

  // ---------------------------------------------------------------------------
  // 2. testCtorSetAsUntyped
  // ---------------------------------------------------------------------------

  private def genTestCtorSetAsUntyped(maxArity: Int): String = {
    val sb = new StringBuilder
    sb ++= "  /** Test: Type.CtorN.setX[...].asUntyped works for all arities. */\n"
    sb ++= "  def testCtorSetAsUntyped: Expr[Data] = {\n"
    for (n <- 2 to maxArity) {
      val v = ctorVarName(n)
      val t = typeName(n)
      sb ++= s"    val $v: Type.Ctor$n[$t] = Type.Ctor$n.of[$t]\n"
    }
    sb ++= "\n"
    sb ++= "    Expr(\n"
    sb ++= "      Data.map(\n"
    val entries = for (n <- 2 to maxArity) yield {
      val k = keyPrefix(n)
      val v = ctorVarName(n)
      val lastLetter = ArityGen.paramName(n - 1)
      Seq(
        s"""        "${k}SetAUntyped" -> Data(UntypedType.plainPrint($v.setA[String](using Type.of[String]).asUntyped))""",
        s"""        "${k}Set${lastLetter}Untyped" -> Data(UntypedType.plainPrint($v.set$lastLetter[Int](using Type.of[Int]).asUntyped))"""
      )
    }
    val allEntries = entries.flatten
    for ((entry, idx) <- allEntries.zipWithIndex) {
      val comma = if (idx < allEntries.size - 1) "," else ""
      sb ++= s"$entry$comma\n"
    }
    sb ++= "      )\n"
    sb ++= "    )\n"
    sb ++= "  }\n"
    sb.toString
  }

  // ---------------------------------------------------------------------------
  // 3. testTypeOfWithCtorNHigher (N=2..22)
  // ---------------------------------------------------------------------------

  private def genTestTypeOfWithCtorNHigher(n: Int): String = {
    val cn = containerName(n)
    s"""  /** Test: Type.of[${cn}[F]] works when F comes from Type.Ctor$n. */
       |  def testTypeOfWithCtor${n}Higher: Expr[Data] = {
       |    val container = makeCtor${n}ResultContainer
       |    import container.Result
       |
       |    Expr(
       |      Data.map(
       |        "container${n}OfResult" -> Data(Type.of[$cn[Result]].plainPrint)
       |      )
       |    )
       |  }
       |""".stripMargin
  }

  // ---------------------------------------------------------------------------
  // 4. testExprQuoteWithCtorNBody (N=1..22)
  // ---------------------------------------------------------------------------

  private def genTestExprQuoteWithCtorNBody(n: Int): String = {
    val cn = containerName(n)
    val t = typeName(n)
    if (n == 1) {
      s"""  /** Test: Expr.quote with Container[Option] (Ctor1 body). */
         |  def testExprQuoteWithCtor1Body: Expr[Data] =
         |    // Option is a globally available type, so Expr.quote handles it directly without Ctor1 injection.
         |    // This test validates that Expr.quote works with Container[Option] (a higher-kinded type application).
         |    Expr.quote {
         |      val c: Container[Option] = new Container[Option] {
         |        override def value: Option[String] = Some("ctor injection works")
         |      }
         |      Data(c.value.toString)
         |    }
         |""".stripMargin
    } else {
      s"""  /** Test: Expr.quote with $cn[$t] (Ctor$n body). */
         |  def testExprQuoteWithCtor${n}Body: Expr[Data] =
         |    Expr.quote {
         |      val c: $cn[$t] = new $cn[$t] {
         |        override def value: String = "ctor$n works"
         |      }
         |      Data(c.value)
         |    }
         |""".stripMargin
    }
  }

  // ---------------------------------------------------------------------------
  // 5. makeCtorResultContainer / makeCtor{N}ResultContainer
  // ---------------------------------------------------------------------------

  private def genMakeCtorResultContainer(): String =
    s"""  /** Factory that builds the container outside of the class body, avoiding infinite loop from the plugin injecting a
       |    * given for `Result` (via `@ImportedCrossTypeImplicit`) during the initialization of `val Result` itself.
       |    */
       |  protected def makeCtorResultContainer: CtorResultContainer = {
       |    val ctor = Type.Ctor2.of[Either].setA[String](using Type.of[String])
       |    new CtorResultContainer(ctor.asInstanceOf[Type.Ctor1[Option]])
       |  }
       |""".stripMargin

  private def genMakeCtorNResultContainer(n: Int): String = {
    if (n == 22) {
      s"""  protected def makeCtor22ResultContainer: Ctor22ResultContainer = {
         |    val ctor = Type.Ctor22.of[Arity22]
         |    new Ctor22ResultContainer(ctor.asInstanceOf[Type.Ctor22[Arity22]])
         |  }
         |""".stripMargin
    } else {
      val nextN = n + 1
      val nextType = typeName(nextN)
      val castType = typeName(n)
      s"""  protected def makeCtor${n}ResultContainer: Ctor${n}ResultContainer = {
         |    val ctor = Type.Ctor$nextN.of[$nextType].setA[String](using Type.of[String])
         |    new Ctor${n}ResultContainer(ctor.asInstanceOf[Type.Ctor$n[$castType]])
         |  }
         |""".stripMargin
    }
  }

  // ---------------------------------------------------------------------------
  // 6. CtorResultContainer / Ctor{N}ResultContainer classes
  // ---------------------------------------------------------------------------

  private def genCtorResultContainerClass(): String =
    s"""  /** Helper providing the CtorLikeOf-like pattern for tests.
       |    *
       |    * The `Result` Ctor1 is passed in via constructor to avoid circular initialization in Scala 3. The type parameter is
       |    * erased at runtime, so the `Option` in `Type.Ctor1[Option]` is just a placeholder.
       |    */
       |  protected class CtorResultContainer(ctor: Type.Ctor1[Option]) {
       |    type Result[X] = Either[String, X]
       |    @hearth.typed.ImportedCrossTypeImplicit
       |    implicit val Result: Type.Ctor1[Result] = ctor.asInstanceOf[Type.Ctor1[Result]]
       |  }
       |""".stripMargin

  private def genCtorNResultContainerClass(n: Int): String = {
    val ctorType = typeName(n)
    if (n == 2) {
      s"""  protected class Ctor2ResultContainer(ctor: Type.Ctor2[Either]) {
         |    type Result[X, Y] = Arity3[String, X, Y]
         |    @hearth.typed.ImportedCrossTypeImplicit
         |    implicit val Result: Type.Ctor2[Result] = ctor.asInstanceOf[Type.Ctor2[Result]]
         |  }
         |""".stripMargin
    } else if (n == 22) {
      val params = ArityGen.paramNameList(22)
      s"""  protected class Ctor22ResultContainer(ctor: Type.Ctor22[Arity22]) {
         |    type Result[$params] =
         |      Arity22[$params]
         |    @hearth.typed.ImportedCrossTypeImplicit
         |    implicit val Result: Type.Ctor22[Result] = ctor.asInstanceOf[Type.Ctor22[Result]]
         |  }
         |""".stripMargin
    } else {
      // N >= 3 and N <= 21
      // type params: N params starting at B (index 1) through paramName(n)
      val tpList = (1 to n).map(i => ArityGen.paramName(i))
      val typeParams = tpList.mkString(", ") // B, C, D, ... (N params)
      val resultParams = "String, " + tpList.mkString(", ")
      val nextType = typeName(n + 1)
      // Check if the type alias fits on one line
      val typeAliasRhs = s"$nextType[$resultParams]"
      val typeParamsStr = typeParams
      if (typeAliasRhs.length + typeParamsStr.length + "    type Result[] = ".length <= 120) {
        s"""  protected class Ctor${n}ResultContainer(ctor: Type.Ctor$n[$ctorType]) {
           |    type Result[$typeParams] = $nextType[$resultParams]
           |    @hearth.typed.ImportedCrossTypeImplicit
           |    implicit val Result: Type.Ctor$n[Result] = ctor.asInstanceOf[Type.Ctor$n[Result]]
           |  }
           |""".stripMargin
      } else {
        s"""  protected class Ctor${n}ResultContainer(ctor: Type.Ctor$n[$ctorType]) {
           |    type Result[$typeParams] =
           |      $nextType[$resultParams]
           |    @hearth.typed.ImportedCrossTypeImplicit
           |    implicit val Result: Type.Ctor$n[Result] = ctor.asInstanceOf[Type.Ctor$n[Result]]
           |  }
           |""".stripMargin
      }
    }
  }

  // ---------------------------------------------------------------------------
  // 7. testCtorFromUntyped
  // ---------------------------------------------------------------------------

  private def genTestCtorFromUntyped(maxArity: Int): String = {
    val sb = new StringBuilder
    sb ++= "  /** Test: Type.CtorN.fromUntyped roundtrip - of -> asUntyped -> fromUntyped -> verify. */\n"
    sb ++= "  def testCtorFromUntyped: Expr[Data] = {\n"
    // Local val declarations
    for (n <- 1 to maxArity) {
      val prefix = fromVar(n)
      val t = typeName(n)
      sb ++= s"    val ${prefix}Of = Type.Ctor$n.of[$t]\n"
      sb ++= s"    val ${prefix}Untyped = ${prefix}Of.asUntyped\n"
      sb ++= s"    val ${prefix}From = Type.Ctor$n.fromUntyped[$t](${prefix}Untyped)\n"
    }
    sb ++= "\n"
    sb ++= "    Expr(\n"
    sb ++= "      Data.map(\n"

    val allEntries = new scala.collection.mutable.ArrayBuffer[String]

    for (n <- 1 to maxArity) {
      val prefix = fromVar(n)
      val k = keyPrefix(n)
      val t = typeName(n)

      // apply line
      val applyTypes = n match {
        case 1 => "Int"
        case 2 => "String, Int"
        case _ => ints(n)
      }
      val applyUsing = n match {
        case 1 => "Type.of[Int]"
        case 2 => "Type.of[String], Type.of[Int]"
        case _ => typeOfInts(n)
      }
      allEntries += s"""        "${k}Apply" -> Data(${prefix}From.apply[$applyTypes](using $applyUsing).plainPrint)"""

      // unapplyMatch line
      val matchTypes = n match {
        case 1 => "String"   // Option[String]
        case 2 => "String, Int" // Either[String, Int]
        case _ => ints(n)
      }
      allEntries += s"""        "${k}UnapplyMatch" -> Data(${prefix}From.unapply(Type.of[$t[$matchTypes]]).isDefined.toString)"""

      // unapplyNoMatch line
      val noMatchType = if (n == 1) "List[String]" else "Option[String]"
      allEntries += s"""        "${k}UnapplyNoMatch" -> Data(${prefix}From.unapply(Type.of[$noMatchType]).isDefined.toString)"""

      // asUntyped line
      allEntries += s"""        "${k}AsUntyped" -> Data(UntypedType.plainPrint(${prefix}From.asUntyped))"""
    }

    for ((entry, idx) <- allEntries.zipWithIndex) {
      val comma = if (idx < allEntries.size - 1) "," else ""
      sb ++= s"$entry$comma\n"
    }

    sb ++= "      )\n"
    sb ++= "    )\n"
    sb ++= "  }\n"
    sb.toString
  }

  // ---------------------------------------------------------------------------
  // 8. verifyExtractedCtorN
  // ---------------------------------------------------------------------------

  private def genVerifyExtractedCtorN(n: Int): String = {
    val t = typeName(n)
    val methodName = s"verifyExtractedCtor$n$t"

    val applyTypes = n match {
      case 1 => "Int"
      case 2 => "String, Int"
      case 3 => "Int, String, Boolean"
      case _ => ints(n)
    }
    val applyUsing = n match {
      case 1 => "Type.of[Int]"
      case 2 => "Type.of[String], Type.of[Int]"
      case 3 => "Type.of[Int], Type.of[String], Type.of[Boolean]"
      case _ => typeOfInts(n)
    }
    val unapplyTypes = n match {
      case 1 => "String" // Option[String]
      case 2 => "String, Int" // Either[String, Int]
      case 3 => "Int, String, Boolean"
      case _ => ints(n)
    }
    val noMatchType = if (n == 1) "List[String]" else "Option[String]"

    val sb = new StringBuilder
    sb ++= s"  /** Verify a Ctor$n[$t] that was constructed via fromUntyped using a platform-extracted UntypedType. */\n"
    sb ++= s"  def $methodName(ctor: Type.Ctor$n[$t]): Expr[Data] =\n"
    sb ++= s"    Expr(\n"
    sb ++= s"      Data.map(\n"
    sb ++= s"""        "apply" -> Data(ctor.apply[$applyTypes](using $applyUsing).plainPrint),\n"""
    sb ++= s"""        "unapplyMatch" -> Data(ctor.unapply(Type.of[$t[$unapplyTypes]]).isDefined.toString),\n"""
    sb ++= s"""        "unapplyNoMatch" -> Data(ctor.unapply(Type.of[$noMatchType]).isDefined.toString)\n"""
    sb ++= s"      )\n"
    sb ++= s"    )\n"
    sb.toString
  }

  // ---------------------------------------------------------------------------
  // 9. testCtorSetApplyUnapply range methods
  // ---------------------------------------------------------------------------

  private def genTestCtorSetApplyUnapplyRange(from: Int, to: Int): String = {
    val sb = new StringBuilder
    sb ++= s"  /** Test: Type.CtorN.setA/setLast.apply/unapply for arities $from-$to. */\n"
    sb ++= s"  def testCtorSetApplyUnapply${from}to$to: Expr[Data] = {\n"
    for (n <- from to to) {
      val v = ctorVarName(n)
      val t = typeName(n)
      sb ++= s"    val $v: Type.Ctor$n[$t] = Type.Ctor$n.of[$t]\n"
    }
    sb ++= "\n"
    sb ++= "    Expr(\n"
    sb ++= "      Data.map(\n"

    val allEntries = new scala.collection.mutable.ArrayBuffer[String]

    for (n <- from to to) {
      val k = keyPrefix(n)
      val v = ctorVarName(n)
      val t = typeName(n)
      val lastLetter = ArityGen.paramName(n - 1)
      val setApplyCount = n - 1 // after setting one param, apply takes N-1 params

      // setA entries
      allEntries += s"""        "${k}SetAApply" -> Data($v.setA[Int](using Type.of[Int]).apply[${ints(setApplyCount)}](using ${typeOfInts(setApplyCount)}).plainPrint)"""
      allEntries += s"""        "${k}SetAUnapplyMatch" -> Data($v.setA[Int](using Type.of[Int]).unapply(Type.of[$t[${ints(n)}]]).isDefined.toString)"""
      allEntries += s"""        "${k}SetAUnapplyNoMatch" -> Data($v.setA[Int](using Type.of[Int]).unapply(Type.of[Option[String]]).isDefined.toString)"""

      // setLast entries
      allEntries += s"""        "${k}Set${lastLetter}Apply" -> Data($v.set$lastLetter[Int](using Type.of[Int]).apply[${ints(setApplyCount)}](using ${typeOfInts(setApplyCount)}).plainPrint)"""
      allEntries += s"""        "${k}Set${lastLetter}UnapplyMatch" -> Data($v.set$lastLetter[Int](using Type.of[Int]).unapply(Type.of[$t[${ints(n)}]]).isDefined.toString)"""
      allEntries += s"""        "${k}Set${lastLetter}UnapplyNoMatch" -> Data($v.set$lastLetter[Int](using Type.of[Int]).unapply(Type.of[Option[String]]).isDefined.toString)"""
    }

    for ((entry, idx) <- allEntries.zipWithIndex) {
      val comma = if (idx < allEntries.size - 1) "," else ""
      sb ++= s"$entry$comma\n"
    }

    sb ++= "      )\n"
    sb ++= "    )\n"
    sb ++= "  }\n"
    sb.toString
  }

  // ---------------------------------------------------------------------------
  // 10. testCtorSetMiddleAsUntyped range methods
  // ---------------------------------------------------------------------------

  private def genTestCtorSetMiddleAsUntypedRange(from: Int, to: Int): String = {
    val sb = new StringBuilder
    sb ++= s"  /** Test: Type.CtorN.setMiddle.asUntyped for arities $from-$to. */\n"
    sb ++= s"  def testCtorSetMiddleAsUntyped${from}to$to: Expr[Data] = {\n"
    for (n <- from to to) {
      val v = ctorVarName(n)
      val t = typeName(n)
      sb ++= s"    val $v: Type.Ctor$n[$t] = Type.Ctor$n.of[$t]\n"
    }
    sb ++= "\n"
    sb ++= "    Expr(\n"
    sb ++= "      Data.map(\n"

    val allEntries = new scala.collection.mutable.ArrayBuffer[String]

    for (n <- from to to) {
      val k = keyPrefix(n)
      val v = ctorVarName(n)
      // Middle params: B (index 1) through second-to-last (index N-2)
      // For arity 3: middle = B only (indices 1..1)
      // For arity 4: middle = B, C (indices 1..2)
      for (i <- 1 to (n - 2)) {
        val letter = ArityGen.paramName(i)
        allEntries += s"""        "${k}Set${letter}Untyped" -> Data(UntypedType.plainPrint($v.set$letter[Int](using Type.of[Int]).asUntyped))"""
      }
    }

    for ((entry, idx) <- allEntries.zipWithIndex) {
      val comma = if (idx < allEntries.size - 1) "," else ""
      sb ++= s"$entry$comma\n"
    }

    sb ++= "      )\n"
    sb ++= "    )\n"
    sb ++= "  }\n"
    sb.toString
  }

  // ===========================================================================
  // Scala 2 bridge generator
  // ===========================================================================

  /** FQN used in Scala 2 for the type. */
  private def scala2Fqn(n: Int): String = n match {
    case 1 => "scala.Option"
    case 2 => "scala.Either"
    case _ => s"hearth.examples.kinds.Arity$n"
  }

  /** FQN used in Scala 3 for the type. */
  private def scala3Fqn(n: Int): String = n match {
    case 1 => "scala.Option"
    case 2 => "scala.util.Either"
    case _ => s"hearth.examples.kinds.Arity$n"
  }

  def generateScala2Bridge(maxArity: Int = ArityGen.maxArity): String = {
    val sb = new StringBuilder
    sb ++=
      s"""// AUTO-GENERATED — DO NOT EDIT
         |// format: off
         |package hearth
         |package crossquotes
         |
         |import hearth.data.Data
         |
         |import scala.language.experimental.macros
         |import scala.reflect.macros.blackbox
         |
         |final private class CrossCtorInjectionFixtures(val c: blackbox.Context)
         |    extends MacroCommonsScala2
         |    with CrossCtorInjectionFixturesImpl {
         |
         |  def testTypeOfWithImportedCtor1Impl[A: c.WeakTypeTag]: c.Expr[Data] = testTypeOfWithImportedCtor1[A]
         |
         |  def testCtorAsUntypedImpl: c.Expr[Data] = testCtorAsUntyped
         |
         |  def testCtorSetAsUntypedImpl: c.Expr[Data] = testCtorSetAsUntyped
         |
         |""".stripMargin

    // testTypeOfWithCtorNHigherImpl forwarders
    for (n <- 1 to maxArity) {
      sb ++= s"  def testTypeOfWithCtor${n}HigherImpl: c.Expr[Data] = testTypeOfWithCtor${n}Higher\n\n"
    }

    // testExprQuoteWithCtorNBodyImpl forwarders
    for (n <- 1 to maxArity) {
      sb ++= s"  def testExprQuoteWithCtor${n}BodyImpl: c.Expr[Data] = testExprQuoteWithCtor${n}Body\n\n"
    }

    // testCtorFromUntypedImpl forwarder
    sb ++= "  def testCtorFromUntypedImpl: c.Expr[Data] = testCtorFromUntyped\n\n"

    // testIdentityCtor1UntypedImpl forwarder
    sb ++= "  def testIdentityCtor1UntypedImpl: c.Expr[Data] = testIdentityCtor1Untyped\n\n"

    // testCtorExtractNImpl - substantive methods
    for (n <- 1 to maxArity) {
      sb ++= genScala2ExtractMethod(n)
      sb ++= "\n"
    }

    // testCtorSetApplyUnapply range forwarders
    for ((from, to) <- Seq((2, 8), (9, 15), (16, 22))) {
      sb ++= s"  def testCtorSetApplyUnapply${from}to${to}Impl: c.Expr[Data] = testCtorSetApplyUnapply${from}to$to\n\n"
    }

    // testCtorSetMiddleAsUntyped range forwarders
    for ((from, to) <- Seq((3, 7), (8, 12), (13, 17), (18, 22))) {
      sb ++= s"  def testCtorSetMiddleAsUntyped${from}to${to}Impl: c.Expr[Data] = testCtorSetMiddleAsUntyped${from}to$to\n\n"
    }

    sb ++= "}\n\n"

    // Companion object
    sb ++= "object CrossCtorInjectionFixtures {\n\n"

    sb ++= "  def testTypeOfWithImportedCtor1[A]: Data = macro CrossCtorInjectionFixtures.testTypeOfWithImportedCtor1Impl[A]\n\n"
    sb ++= "  def testCtorAsUntyped: Data = macro CrossCtorInjectionFixtures.testCtorAsUntypedImpl\n\n"
    sb ++= "  def testCtorSetAsUntyped: Data = macro CrossCtorInjectionFixtures.testCtorSetAsUntypedImpl\n\n"

    for (n <- 1 to maxArity) {
      sb ++= s"  def testTypeOfWithCtor${n}Higher: Data = macro CrossCtorInjectionFixtures.testTypeOfWithCtor${n}HigherImpl\n\n"
    }

    for (n <- 1 to maxArity) {
      sb ++= s"  def testExprQuoteWithCtor${n}Body: Data = macro CrossCtorInjectionFixtures.testExprQuoteWithCtor${n}BodyImpl\n\n"
    }

    sb ++= "  def testCtorFromUntyped: Data = macro CrossCtorInjectionFixtures.testCtorFromUntypedImpl\n\n"

    sb ++= "  def testIdentityCtor1Untyped: Data = macro CrossCtorInjectionFixtures.testIdentityCtor1UntypedImpl\n\n"

    for (n <- 1 to maxArity) {
      sb ++= s"  def testCtorExtract$n: Data = macro CrossCtorInjectionFixtures.testCtorExtract${n}Impl\n\n"
    }

    for ((from, to) <- Seq((2, 8), (9, 15), (16, 22))) {
      sb ++= s"  def testCtorSetApplyUnapply${from}to$to: Data = macro CrossCtorInjectionFixtures.testCtorSetApplyUnapply${from}to${to}Impl\n\n"
    }

    for ((from, to) <- Seq((3, 7), (8, 12), (13, 17), (18, 22))) {
      sb ++= s"  def testCtorSetMiddleAsUntyped${from}to$to: Data = macro CrossCtorInjectionFixtures.testCtorSetMiddleAsUntyped${from}to${to}Impl\n\n"
    }

    sb ++= "}\n"
    sb.toString
  }

  private def genScala2ExtractMethod(n: Int): String = {
    val sb = new StringBuilder
    val t = typeName(n)
    val prefix = fromVar(n)
    val typeArgs = n match {
      case 1 => "Int"
      case 2 => "String, Int"
      case 3 => "Int, String, Boolean"
      case _ => ints(n)
    }

    // Determine line wrapping based on length
    val weakTypeOfExpr = s"c.weakTypeOf[$t[$typeArgs]]"
    val fullLine = s"    val ctor: UntypedType = $weakTypeOfExpr.typeConstructor"

    sb ++= s"  def testCtorExtract${n}Impl: c.Expr[Data] = {\n"
    if (n >= 3) sb ++= s"    import hearth.examples.kinds.$t\n"

    if (fullLine.length <= 120) {
      sb ++= s"$fullLine\n"
    } else if (n < 16) {
      // Multi-line with newline after =
      sb ++= s"    val ctor: UntypedType =\n"
      sb ++= s"      $weakTypeOfExpr.typeConstructor\n"
    } else if (n < 19) {
      // c.\n  .weakTypeOf pattern
      sb ++= s"    val ctor: UntypedType = c\n"
      sb ++= s"      .weakTypeOf[$t[$typeArgs]]\n"
      sb ++= s"      .typeConstructor\n"
    } else if (n < 22) {
      // c.\n  .weakTypeOf[\n  ]\n  .typeConstructor pattern
      sb ++= s"    val ctor: UntypedType = c\n"
      sb ++= s"      .weakTypeOf[\n"
      sb ++= s"        $t[$typeArgs]\n"
      sb ++= s"      ]\n"
      sb ++= s"      .typeConstructor\n"
    } else {
      // arity 22: vertical Int list
      sb ++= s"    val ctor: UntypedType =\n"
      sb ++= s"      c.weakTypeOf[\n"
      sb ++= s"        $t[\n"
      sb ++= (1 to n).map(_ => "          Int").mkString(",\n")
      sb ++= "\n"
      sb ++= s"        ]\n"
      sb ++= s"      ].typeConstructor\n"
    }

    sb ++= s"    val ${prefix}Ctor = Type.Ctor$n.fromUntyped[$t](ctor)\n"
    sb ++= s"    verifyExtractedCtor$n$t(${prefix}Ctor)\n"
    sb ++= s"  }\n"
    sb.toString
  }

  // ===========================================================================
  // Scala 3 bridge generator
  // ===========================================================================

  def generateScala3Bridge(maxArity: Int = ArityGen.maxArity): String = {
    val sb = new StringBuilder
    sb ++=
      s"""// AUTO-GENERATED — DO NOT EDIT
         |// format: off
         |package hearth
         |package crossquotes
         |
         |import hearth.data.Data
         |
         |import scala.quoted.*
         |
         |final private class CrossCtorInjectionFixtures(q: Quotes)
         |    extends MacroCommonsScala3(using q),
         |      CrossCtorInjectionFixturesImpl {
         |
         |""".stripMargin

    // testCtorExtractN methods (class body)
    for (n <- 1 to maxArity) {
      sb ++= genScala3ExtractMethod(n)
      sb ++= "\n"
    }

    sb ++= "}\n\n"

    // Companion object
    sb ++= "object CrossCtorInjectionFixtures {\n\n"

    // testTypeOfWithImportedCtor1 - special: has type parameter
    sb ++= "  inline def testTypeOfWithImportedCtor1[A]: Data = ${ testTypeOfWithImportedCtor1Impl[A] }\n"
    sb ++= "  private def testTypeOfWithImportedCtor1Impl[A: Type](using q: Quotes): Expr[Data] =\n"
    sb ++= "    new CrossCtorInjectionFixtures(q).testTypeOfWithImportedCtor1[A]\n\n"

    // Simple forwarder methods
    for (name <- Seq("testCtorAsUntyped", "testCtorSetAsUntyped")) {
      sb ++= genScala3InlineSplice(name)
    }

    for (n <- 1 to maxArity) {
      sb ++= genScala3InlineSplice(s"testTypeOfWithCtor${n}Higher")
    }

    for (n <- 1 to maxArity) {
      sb ++= genScala3InlineSplice(s"testExprQuoteWithCtor${n}Body")
    }

    sb ++= genScala3InlineSplice("testCtorFromUntyped")
    sb ++= genScala3InlineSplice("testIdentityCtor1Untyped")

    for (n <- 1 to maxArity) {
      sb ++= genScala3InlineSplice(s"testCtorExtract$n")
    }

    for ((from, to) <- Seq((2, 8), (9, 15), (16, 22))) {
      sb ++= genScala3InlineSplice(s"testCtorSetApplyUnapply${from}to$to")
    }

    for ((from, to) <- Seq((3, 7), (8, 12), (13, 17), (18, 22))) {
      sb ++= genScala3InlineSplice(s"testCtorSetMiddleAsUntyped${from}to$to")
    }

    sb ++= "}\n"
    sb.toString
  }

  private def genScala3InlineSplice(name: String): String =
    s"""  inline def $name: Data = $${ ${name}Impl }
       |  private def ${name}Impl(using q: Quotes): Expr[Data] =
       |    new CrossCtorInjectionFixtures(q).$name
       |
       |""".stripMargin

  private def genScala3ExtractMethod(n: Int): String = {
    val sb = new StringBuilder
    val t = typeName(n)
    val prefix = fromVar(n)
    val typeArgs = n match {
      case 1 => "Int"
      case 2 => "String, Int"
      case 3 => "Int, String, Boolean"
      case _ => ints(n)
    }

    sb ++= s"  def testCtorExtract$n: Expr[Data] = {\n"
    sb ++= s"    import quotes.reflect.*\n"
    if (n >= 3) sb ++= s"    import hearth.examples.kinds.$t\n"

    // TypeRepr.of line
    val typeReprExpr = s"TypeRepr.of[$t[$typeArgs]]"
    val fullLine = s"    val repr = $typeReprExpr"

    if (fullLine.length <= 120) {
      sb ++= s"$fullLine\n"
    } else if (n < 19) {
      // Multi-line with newline after =
      sb ++= s"    val repr =\n"
      sb ++= s"      $typeReprExpr\n"
    } else if (n < 21) {
      // TypeRepr\n.of[...] pattern
      sb ++= s"    val repr = TypeRepr\n"
      sb ++= s"      .of[$t[$typeArgs]]\n"
    } else if (n == 21) {
      // TypeRepr.of[\n  ...\n] pattern
      sb ++= s"    val repr = TypeRepr.of[\n"
      sb ++= s"      $t[$typeArgs]\n"
      sb ++= s"    ]\n"
    } else {
      // arity 22: vertical Int list
      sb ++= s"    val repr =\n"
      sb ++= s"      TypeRepr.of[$t[\n"
      sb ++= (1 to n).map(_ => "        Int").mkString(",\n")
      sb ++= "\n"
      sb ++= s"      ]]\n"
    }

    val errorMsg = n match {
      case 1 => "Expected AppliedType for Option[Int]"
      case 2 => "Expected AppliedType for Either[String, Int]"
      case 3 => "Expected AppliedType for Arity3[Int, String, Boolean]"
      case _ => s"Expected AppliedType for $t[...]"
    }
    sb ++= s"    val ctor = repr match {\n"
    sb ++= s"""      case AppliedType(tycon, _) => tycon.asInstanceOf[UntypedType]\n"""
    sb ++= s"""      case _                     => throw new Exception("$errorMsg")\n"""
    sb ++= s"    }\n"
    sb ++= s"    val ${prefix}Ctor = Type.Ctor$n.fromUntyped[$t](ctor)\n"
    sb ++= s"    verifyExtractedCtor$n$t(${prefix}Ctor)\n"
    sb ++= s"  }\n"
    sb.toString
  }

  // ===========================================================================
  // Spec generator
  // ===========================================================================

  def generateSpec(maxArity: Int = ArityGen.maxArity): String = {
    val sb = new StringBuilder
    sb ++=
      s"""// AUTO-GENERATED — DO NOT EDIT
         |// format: off
         |package hearth
         |package crossquotes
         |
         |import hearth.data.Data
         |
         |/** Tests for Type.CtorN injection into cross-quotes.
         |  *
         |  * Macro implementation is in [[hearth.cq.CrossQuotesMacros]] (Scala 2) and [[hearth.cq.CrossQuotesPlugin]] (Scala 3).
         |  *
         |  * Fixtures are in [[CrossCtorInjectionFixturesImpl]].
         |  */
         |final class CrossCtorInjectionSpec extends MacroSuite {
         |
         |  group("Cross-Quotes CtorN injection") {
         |
         |""".stripMargin

    // Group 1: testTypeOfWithImportedCtor1
    sb ++= genSpecImportedCtor1()
    sb ++= "\n"

    // Group 2: testCtorAsUntyped
    sb ++= genSpecCtorAsUntyped(maxArity)
    sb ++= "\n"

    // Group 3: testCtorSetAsUntyped
    sb ++= genSpecCtorSetAsUntyped(maxArity)
    sb ++= "\n"

    // Group 4: testTypeOfWithCtorNHigher
    sb ++= genSpecTypeOfWithCtorNHigher(maxArity)
    sb ++= "\n"

    // Group 5: testExprQuoteWithCtorNBody
    sb ++= genSpecExprQuoteWithCtorNBody(maxArity)
    sb ++= "\n"

    // Group 6: testCtorFromUntyped
    sb ++= genSpecCtorFromUntyped(maxArity)
    sb ++= "\n"

    // Group 7: testCtorExtractN
    sb ++= genSpecCtorExtractN(maxArity)
    sb ++= "\n"

    // Group 8: testCtorSetApplyUnapply
    sb ++= genSpecCtorSetApplyUnapply(maxArity)
    sb ++= "\n"

    // Group 9: testCtorSetMiddleAsUntyped
    sb ++= genSpecCtorSetMiddleAsUntyped(maxArity)
    sb ++= "\n"

    sb ++= "  }\n\n"
    sb ++= "}\n"
    sb.toString
  }

  private def genSpecImportedCtor1(): String =
    s"""    group("for Type.of with imported Type.Ctor1 (Result pattern)") {
       |
       |      test("should resolve Result[X] when Result is imported from a CtorLikeOf-like container") {
       |        CrossCtorInjectionFixtures.testTypeOfWithImportedCtor1[Int] <==> Data.map(
       |          "resultOfInt" -> Data("scala.util.Either[java.lang.String, scala.Int]"),
       |          "resultOfString" -> Data("scala.util.Either[java.lang.String, java.lang.String]"),
       |          "resultOfA" -> Data("scala.util.Either[java.lang.String, scala.Int]")
       |        )
       |      }
       |
       |      test("should resolve Result[X] with String type parameter") {
       |        CrossCtorInjectionFixtures.testTypeOfWithImportedCtor1[String] <==> Data.map(
       |          "resultOfInt" -> Data("scala.util.Either[java.lang.String, scala.Int]"),
       |          "resultOfString" -> Data("scala.util.Either[java.lang.String, java.lang.String]"),
       |          "resultOfA" -> Data("scala.util.Either[java.lang.String, java.lang.String]")
       |        )
       |      }
       |    }
       |""".stripMargin

  /** Generates "A >: scala.Nothing <: scala.Any" repeated for N params. */
  private def scala3BoundParams(n: Int): String =
    ArityGen.params(n).map(p => s"$p >: scala.Nothing <: scala.Any").mkString(", ")

  /** Generates Scala 3 ctor untyped representation for arity N. */
  private def scala3CtorUntyped(n: Int): String = {
    val fqn = scala3Fqn(n)
    val params = scala3BoundParams(n)
    val args = ArityGen.paramNameList(n)
    s"$fqn[$params] => $fqn[$args]"
  }

  private def genSpecCtorAsUntyped(maxArity: Int): String = {
    val sb = new StringBuilder
    sb ++= "    group(\"for Type.CtorN.asUntyped\") {\n\n"
    sb ++= "      test(\"should return valid UntypedType for type constructors of all arities\") {\n"
    sb ++= "        val isScala3 = LanguageVersion.byHearth.isScala3\n"
    sb ++= "        CrossCtorInjectionFixtures.testCtorAsUntyped <==> Data.map(\n"

    for (n <- 1 to maxArity) {
      val k = keyPrefix(n)
      val s3 = scala3CtorUntyped(n)
      val s2 = scala2Fqn(n)
      val comma = if (n < maxArity) "," else ""
      sb ++= s"""          "${k}CtorUntyped" -> Data(\n"""
      sb ++= s"""            if (isScala3)\n"""
      sb ++= s"""              "$s3"\n"""
      sb ++= s"""            else "$s2"\n"""
      sb ++= s"""          )$comma\n"""
    }

    sb ++= "        )\n"
    sb ++= "      }\n"
    sb ++= "    }\n"
    sb.toString
  }

  /** Generates "_$1 >: scala.Nothing <: scala.Any, _$2 >: ..." for count params. */
  private def scala3SetBoundParams(count: Int): String =
    (1 to count).map(i => s"_$$$i >: scala.Nothing <: scala.Any").mkString(", ")

  /** Generates "_$1, _$2, ..." for count params. */
  private def scala3SetArgs(count: Int): String =
    (1 to count).map(i => s"_$$$i").mkString(", ")

  /** Generates Scala 3 setA untyped string: FQN[_$1 >: ..., _$2 >: ...] => FQN[java.lang.String, _$1, _$2] */
  private def scala3SetAUntyped(n: Int): String = {
    val fqn = scala3Fqn(n)
    val remaining = n - 1
    val params = scala3SetBoundParams(remaining)
    val resultArgs = "java.lang.String, " + scala3SetArgs(remaining)
    s"$fqn[$params] => $fqn[$resultArgs]"
  }

  /** Generates Scala 3 setLast untyped string. */
  private def scala3SetLastUntyped(n: Int): String = {
    val fqn = scala3Fqn(n)
    val remaining = n - 1
    val params = scala3SetBoundParams(remaining)
    val resultArgs = scala3SetArgs(remaining) + ", scala.Int"
    s"$fqn[$params] => $fqn[$resultArgs]"
  }

  /** Generates Scala 2 setA untyped string: "({type λ[remaining params] = FQN})#λ" */
  private def scala2SetAUntyped(n: Int): String = {
    val fqn = scala2Fqn(n)
    val remaining = ArityGen.params(n).drop(1).mkString(", ")
    s"({type λ[$remaining] = $fqn})#λ"
  }

  /** Generates Scala 2 setLast untyped string. */
  private def scala2SetLastUntyped(n: Int): String = {
    val fqn = scala2Fqn(n)
    val remaining = ArityGen.params(n).dropRight(1).mkString(", ")
    s"({type λ[$remaining] = $fqn})#λ"
  }

  private def genSpecCtorSetAsUntyped(maxArity: Int): String = {
    val sb = new StringBuilder
    sb ++= "    group(\"for Type.CtorN.setX.asUntyped\") {\n\n"
    sb ++= "      test(\"should return valid UntypedType for partially applied type constructors of all arities\") {\n"
    sb ++= "        val isScala3 = LanguageVersion.byHearth.isScala3\n"
    sb ++= "        CrossCtorInjectionFixtures.testCtorSetAsUntyped <==> Data.map(\n"

    val allEntries = new scala.collection.mutable.ArrayBuffer[String]

    for (n <- 2 to maxArity) {
      val k = keyPrefix(n)
      val lastLetter = ArityGen.paramName(n - 1)
      val s3a = scala3SetAUntyped(n)
      val s2a = scala2SetAUntyped(n)
      val s3l = scala3SetLastUntyped(n)
      val s2l = scala2SetLastUntyped(n)

      val entryA = new StringBuilder
      entryA ++= s"""          "${k}SetAUntyped" -> Data(\n"""
      entryA ++= s"""            if (isScala3)\n"""
      entryA ++= s"""              "$s3a"\n"""
      entryA ++= s"""            else "$s2a"\n"""
      entryA ++= s"""          )"""
      allEntries += entryA.toString

      val entryL = new StringBuilder
      entryL ++= s"""          "${k}Set${lastLetter}Untyped" -> Data(\n"""
      entryL ++= s"""            if (isScala3)\n"""
      entryL ++= s"""              "$s3l"\n"""
      entryL ++= s"""            else "$s2l"\n"""
      entryL ++= s"""          )"""
      allEntries += entryL.toString
    }

    for ((entry, idx) <- allEntries.zipWithIndex) {
      val comma = if (idx < allEntries.size - 1) "," else ""
      sb ++= s"$entry$comma\n"
    }

    sb ++= "        )\n"
    sb ++= "      }\n"
    sb ++= "    }\n"
    sb.toString
  }

  /** Container type parameter string for Scala 3 representation in higher-kinded test. */
  private def scala3HigherCtorParams(n: Int): String = {
    if (n == 1) {
      "X >: scala.Nothing <: scala.Any"
    } else if (n == 2) {
      // Ctor2: Arity3.setA[String] produces synthetic type lambda params X, Y in Scala 3
      "X >: scala.Nothing <: scala.Any, Y >: scala.Nothing <: scala.Any"
    } else if (n == 22) {
      // Ctor22: no setA, params are A..V (all original)
      ArityGen.params(n).map(p => s"$p >: scala.Nothing <: scala.Any").mkString(", ")
    } else {
      // N >= 3: Result is setA[String], so params start at B
      (1 to n).map(i => s"${ArityGen.paramName(i)} >: scala.Nothing <: scala.Any").mkString(", ")
    }
  }

  /** Result type string in Scala 3 higher-kinded test. */
  private def scala3HigherResultType(n: Int): String = {
    if (n == 1) {
      "scala.util.Either[java.lang.String, X]"
    } else if (n == 2) {
      // Ctor2: Arity3.setA[String] produces synthetic type lambda params X, Y in Scala 3
      "hearth.examples.kinds.Arity3[java.lang.String, X, Y]"
    } else if (n == 22) {
      val args = ArityGen.paramNameList(22)
      s"hearth.examples.kinds.Arity22[$args]"
    } else {
      val nextFqn = scala3Fqn(n + 1)
      val resultArgs = "java.lang.String, " + (1 to n).map(i => ArityGen.paramName(i)).mkString(", ")
      s"$nextFqn[$resultArgs]"
    }
  }

  private def genSpecTypeOfWithCtorNHigher(maxArity: Int): String = {
    val sb = new StringBuilder
    sb ++= "    group(\"for Type.of with higher-kinded Type.CtorN\") {\n\n"

    // Ctor1 test is special (has functorOfResult and containerOfResult)
    sb ++=
      s"""      test("should resolve Type.of[Trait[Result]] when Result comes from Type.Ctor1") {
         |        val isScala3 = LanguageVersion.byHearth.isScala3
         |        CrossCtorInjectionFixtures.testTypeOfWithCtor1Higher <==> Data.map(
         |          "functorOfResult" -> Data(
         |            if (isScala3)
         |              "hearth.fp.Functor[[X >: scala.Nothing <: scala.Any] => scala.util.Either[java.lang.String, X]]"
         |            else "hearth.fp.Functor[container.Result]"
         |          ),
         |          "containerOfResult" -> Data(
         |            if (isScala3)
         |              "hearth.crossquotes.Container[[X >: scala.Nothing <: scala.Any] => scala.util.Either[java.lang.String, X]]"
         |            else "hearth.crossquotes.Container[container.Result]"
         |          )
         |        )
         |      }
         |
         |""".stripMargin

    // Ctor2..Ctor22
    for (n <- 2 to maxArity) {
      val cn = containerName(n)
      val params = scala3HigherCtorParams(n)
      val resultType = scala3HigherResultType(n)

      sb ++= s"""      test("should resolve Type.of[${cn}[Result]] when Result comes from Type.Ctor$n") {\n"""
      sb ++= s"""        val isScala3 = LanguageVersion.byHearth.isScala3\n"""
      sb ++= s"""        CrossCtorInjectionFixtures.testTypeOfWithCtor${n}Higher <==> Data.map(\n"""
      sb ++= s"""          "container${n}OfResult" -> Data(\n"""
      sb ++= s"""            if (isScala3)\n"""
      sb ++= s"""              "hearth.crossquotes.${cn}[[$params] => $resultType]"\n"""
      sb ++= s"""            else "hearth.crossquotes.${cn}[container.Result]"\n"""
      sb ++= s"""          )\n"""
      sb ++= s"""        )\n"""
      sb ++= s"""      }\n\n"""
    }

    sb ++= "    }\n"
    sb.toString
  }

  private def genSpecExprQuoteWithCtorNBody(maxArity: Int): String = {
    val sb = new StringBuilder
    sb ++= "    group(\"for Expr.quote with Type.CtorN body\") {\n\n"

    // Ctor1 is special
    sb ++=
      s"""      test("should create anonymous class extending Container[Option] (Ctor1)") {
         |        CrossCtorInjectionFixtures.testExprQuoteWithCtor1Body <==> Data("Some(ctor injection works)")
         |      }
         |
         |""".stripMargin

    for (n <- 2 to maxArity) {
      val cn = containerName(n)
      val t = typeName(n)
      sb ++= s"""      test("should create anonymous class extending $cn[$t] (Ctor$n)") {\n"""
      sb ++= s"""        CrossCtorInjectionFixtures.testExprQuoteWithCtor${n}Body <==> Data("ctor$n works")\n"""
      sb ++= s"""      }\n\n"""
    }

    sb ++= "    }\n"
    sb.toString
  }

  /** Apply result type string for the testCtorFromUntyped test. */
  private def fromUntypedApplyResult(n: Int): String = {
    val fqn = scala3Fqn(n)
    val args = n match {
      case 1 => "scala.Int"
      case 2 => "java.lang.String, scala.Int"
      case _ => Seq.fill(n)("scala.Int").mkString(", ")
    }
    s"$fqn[$args]"
  }

  private def genSpecCtorFromUntyped(maxArity: Int): String = {
    val sb = new StringBuilder
    sb ++= "    group(\"for Type.CtorN.fromUntyped\") {\n\n"
    sb ++= "      test(\"should roundtrip of -> asUntyped -> fromUntyped for all arities\") {\n"
    sb ++= "        val isScala3 = LanguageVersion.byHearth.isScala3\n"
    sb ++= "        CrossCtorInjectionFixtures.testCtorFromUntyped <==> Data.map(\n"

    val allEntries = new scala.collection.mutable.ArrayBuffer[String]

    for (n <- 1 to maxArity) {
      val k = keyPrefix(n)
      val applyResult = fromUntypedApplyResult(n)
      val s3untyped = scala3CtorUntyped(n)
      val s2untyped = scala2Fqn(n)

      allEntries += s"""          "${k}Apply" -> Data("$applyResult")"""
      allEntries += s"""          "${k}UnapplyMatch" -> Data("true")"""
      allEntries += s"""          "${k}UnapplyNoMatch" -> Data("false")"""

      val untypedEntry = new StringBuilder
      untypedEntry ++= s"""          "${k}AsUntyped" -> Data(\n"""
      untypedEntry ++= s"""            if (isScala3)\n"""
      untypedEntry ++= s"""              "$s3untyped"\n"""
      untypedEntry ++= s"""            else "$s2untyped"\n"""
      untypedEntry ++= s"""          )"""
      allEntries += untypedEntry.toString
    }

    for ((entry, idx) <- allEntries.zipWithIndex) {
      val comma = if (idx < allEntries.size - 1) "," else ""
      sb ++= s"$entry$comma\n"
    }

    sb ++= "        )\n"
    sb ++= "      }\n\n"
    sb ++= "    }\n"
    sb.toString
  }

  private def genSpecCtorExtractN(maxArity: Int): String = {
    val sb = new StringBuilder
    sb ++= "    group(\"for Type.CtorN.fromUntyped extraction\") {\n\n"

    for (n <- 1 to maxArity) {
      val t = typeName(n)
      val sourceDesc = n match {
        case 1 => "Option[Int]"
        case 2 => "Either[String, Int]"
        case 3 => "Arity3[Int, String, Boolean]"
        case _ => s"$t[Int, ...]"
      }

      val applyResult = n match {
        case 1 => "scala.Option[scala.Int]"
        case 2 => "scala.util.Either[java.lang.String, scala.Int]"
        case 3 => "hearth.examples.kinds.Arity3[scala.Int, java.lang.String, scala.Boolean]"
        case _ => s"${scala3Fqn(n)}[${Seq.fill(n)("scala.Int").mkString(", ")}]"
      }

      sb ++= s"""      test("should work for Ctor$n extracted from $sourceDesc") {\n"""
      sb ++= s"""        CrossCtorInjectionFixtures.testCtorExtract$n <==> Data.map(\n"""
      sb ++= s"""          "apply" -> Data(\n"""
      sb ++= s"""            "$applyResult"\n"""
      sb ++= s"""          ),\n"""
      sb ++= s"""          "unapplyMatch" -> Data("true"),\n"""
      sb ++= s"""          "unapplyNoMatch" -> Data("false")\n"""
      sb ++= s"""        )\n"""
      sb ++= s"""      }\n\n"""
    }

    sb ++= "    }\n"
    sb.toString
  }

  private def genSpecCtorSetApplyUnapply(maxArity: Int): String = {
    val sb = new StringBuilder
    sb ++= "    group(\"for Type.CtorN.setX.apply/unapply\") {\n\n"

    for ((from, to) <- Seq((2, 8), (9, 15), (16, 22))) {
      sb ++= s"""      test("should verify apply/unapply for Ctor$from-Ctor$to setA/setLast") {\n"""
      sb ++= s"""        CrossCtorInjectionFixtures.testCtorSetApplyUnapply${from}to$to <==> Data.map(\n"""

      val allEntries = new scala.collection.mutable.ArrayBuffer[String]

      for (n <- from to to) {
        val k = keyPrefix(n)
        val lastLetter = ArityGen.paramName(n - 1)
        val fqn = scala3Fqn(n)
        val allInts = Seq.fill(n)("scala.Int").mkString(", ")
        val typeStr = s"$fqn[$allInts]"

        allEntries += s"""          "${k}SetAApply" -> Data(\n            "$typeStr"\n          )"""
        allEntries += s"""          "${k}SetAUnapplyMatch" -> Data("true")"""
        allEntries += s"""          "${k}SetAUnapplyNoMatch" -> Data("false")"""
        allEntries += s"""          "${k}Set${lastLetter}Apply" -> Data(\n            "$typeStr"\n          )"""
        allEntries += s"""          "${k}Set${lastLetter}UnapplyMatch" -> Data("true")"""
        allEntries += s"""          "${k}Set${lastLetter}UnapplyNoMatch" -> Data("false")"""
      }

      for ((entry, idx) <- allEntries.zipWithIndex) {
        val comma = if (idx < allEntries.size - 1) "," else ""
        sb ++= s"$entry$comma\n"
      }

      sb ++= "        )\n"
      sb ++= "      }\n\n"
    }

    sb ++= "    }\n"
    sb.toString
  }

  /** Generates Scala 3 middle setter untyped string for arity n, setting param at position setIdx (0-based). */
  private def scala3SetMiddleUntyped(n: Int, setIdx: Int): String = {
    val fqn = scala3Fqn(n)
    val remaining = n - 1
    val params = scala3SetBoundParams(remaining)
    // Build the result args: params are _$1.._$(remaining), with scala.Int inserted at setIdx
    val resultArgs = {
      var dollIdx = 0
      (0 until n).map { i =>
        if (i == setIdx) "scala.Int"
        else {
          dollIdx += 1
          s"_$$$dollIdx"
        }
      }.mkString(", ")
    }
    s"$fqn[$params] => $fqn[$resultArgs]"
  }

  /** Generates Scala 2 middle setter untyped string for arity n, setting param at position setIdx (0-based). */
  private def scala2SetMiddleUntyped(n: Int, setIdx: Int): String = {
    val fqn = scala2Fqn(n)
    val remaining = (0 until n).filter(_ != setIdx).map(i => ArityGen.paramName(i)).mkString(", ")
    s"({type λ[$remaining] = $fqn})#λ"
  }

  private def genSpecCtorSetMiddleAsUntyped(maxArity: Int): String = {
    val sb = new StringBuilder
    sb ++= "    group(\"for Type.CtorN.setMiddle.asUntyped\") {\n\n"

    for ((from, to) <- Seq((3, 7), (8, 12), (13, 17), (18, 22))) {
      sb ++= s"""      test("should verify middle setters asUntyped for Ctor$from-Ctor$to") {\n"""
      sb ++= s"""        val isScala3 = LanguageVersion.byHearth.isScala3\n"""
      sb ++= s"""        CrossCtorInjectionFixtures.testCtorSetMiddleAsUntyped${from}to$to <==> Data.map(\n"""

      val allEntries = new scala.collection.mutable.ArrayBuffer[String]

      for (n <- from to to) {
        val k = keyPrefix(n)
        // Middle params: B (index 1) through second-to-last (index N-2)
        for (i <- 1 to (n - 2)) {
          val letter = ArityGen.paramName(i)
          val s3 = scala3SetMiddleUntyped(n, i)
          val s2 = scala2SetMiddleUntyped(n, i)

          val entry = new StringBuilder
          entry ++= s"""          "${k}Set${letter}Untyped" -> Data(\n"""
          entry ++= s"""            if (isScala3)\n"""
          entry ++= s"""              "$s3"\n"""
          entry ++= s"""            else "$s2"\n"""
          entry ++= s"""          )"""
          allEntries += entry.toString
        }
      }

      for ((entry, idx) <- allEntries.zipWithIndex) {
        val comma = if (idx < allEntries.size - 1) "," else ""
        sb ++= s"$entry$comma\n"
      }

      sb ++= "        )\n"
      sb ++= "      }\n\n"
    }

    sb ++= "    }\n"
    sb.toString
  }

}
