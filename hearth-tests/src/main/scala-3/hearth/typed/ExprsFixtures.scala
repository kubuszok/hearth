package hearth
package typed

import hearth.data.Data

import scala.quoted.*

final private class ExprsFixtures(q: Quotes) extends MacroCommonsScala3(using q), ExprsFixturesImpl

object ExprsFixtures {

  // TODO: create macro annotation which would allow to do the following

  inline def testExprPrinters[A](inline expr: A): Data = ${ testExprPrintersImpl[A]('{ expr }) }
  private def testExprPrintersImpl[A: Type](expr: Expr[A])(using q: Quotes): Expr[Data] =
    new ExprsFixtures(q).testExprPrinters[A](expr)

  inline def testExprSummoning[A]: Data = ${ testExprSummoningImpl[A] }
  private def testExprSummoningImpl[A: Type](using q: Quotes): Expr[Data] = new ExprsFixtures(q).testExprSummoning[A]

  inline def testExprSummoningIgnoring[A, Companion](inline ignoredNames: String*): Data = ${
    testExprSummoningIgnoringImpl[A, Companion]('{ ignoredNames })
  }
  private def testExprSummoningIgnoringImpl[A: Type, Companion: Type](ignoredNames: Expr[Seq[String]])(using
      q: Quotes
  ): Expr[Data] =
    new ExprsFixtures(q).testExprSummoningIgnoring[A, Companion](ignoredNames)

  inline def testExprUpcasting[A, B](inline expr: A): Data = ${ testExprUpcastingImpl[A, B]('{ expr }) }
  private def testExprUpcastingImpl[A: Type, B: Type](expr: Expr[A])(using q: Quotes): Expr[Data] =
    new ExprsFixtures(q).testExprUpcasting[A, B](expr)

  inline def testSuppressUnused[A](inline expr: A): Unit = ${ testSuppressUnusedImpl[A]('{ expr }) }
  private def testSuppressUnusedImpl[A: Type](expr: Expr[A])(using q: Quotes): Expr[Unit] =
    new ExprsFixtures(q).testSuppressUnused[A](expr)

  inline def testMatchCaseTypeMatch[A](inline expr: A): Data = ${ testMatchCaseTypeMatchImpl[A]('{ expr }) }
  private def testMatchCaseTypeMatchImpl[A: Type](expr: Expr[A])(using q: Quotes): Expr[Data] =
    new ExprsFixtures(q).testMatchCaseTypeMatch[A](expr)

  inline def testMatchCasePartition[A, B](inline expr: A): B = ${ testMatchCasePartitionImpl[A, B]('{ expr }) }
  private def testMatchCasePartitionImpl[A: Type, B: Type](expr: Expr[A])(using q: Quotes): Expr[B] =
    new ExprsFixtures(q).testMatchCasePartition[A, B](expr)

  inline def testMatchCaseTraverse[A, B](inline expr: A): B = ${ testMatchCaseTraverseImpl[A, B]('{ expr }) }
  private def testMatchCaseTraverseImpl[A: Type, B: Type](expr: Expr[A])(using q: Quotes): Expr[B] =
    new ExprsFixtures(q).testMatchCaseTraverse[A, B](expr)

  inline def testValDefsCreateAndUse: Data = ${ testValDefsCreateAndUseImpl }
  private def testValDefsCreateAndUseImpl(using q: Quotes): Expr[Data] = new ExprsFixtures(q).testValDefsCreateAndUse

  inline def testValDefsPartitionAndClose[A, B](inline expr: A): B = ${
    testValDefsPartitionAndCloseImpl[A, B]('{ expr })
  }
  private def testValDefsPartitionAndCloseImpl[A: Type, B: Type](expr: Expr[A])(using q: Quotes): Expr[B] =
    new ExprsFixtures(q).testValDefsPartitionAndClose[A, B](expr)

  inline def testValDefsTraverseAndClose[A, B](inline expr: A): B = ${
    testValDefsTraverseAndCloseImpl[A, B]('{ expr })
  }
  private def testValDefsTraverseAndCloseImpl[A: Type, B: Type](expr: Expr[A])(using q: Quotes): Expr[B] =
    new ExprsFixtures(q).testValDefsTraverseAndClose[A, B](expr)

  inline def testValDefBuilderCreateAndUse: Data = ${ testValDefBuilderCreateAndUseImpl }
  private def testValDefBuilderCreateAndUseImpl(using q: Quotes): Expr[Data] =
    new ExprsFixtures(q).testValDefBuilderCreateAndUse

  inline def testValDefBuilderPartitionAndClose[A, B](inline expr: A): B = ${
    testValDefBuilderPartitionAndCloseImpl[A, B]('{ expr })
  }
  private def testValDefBuilderPartitionAndCloseImpl[A: Type, B: Type](expr: Expr[A])(using q: Quotes): Expr[B] =
    new ExprsFixtures(q).testValDefBuilderPartitionAndClose[A, B](expr)

  inline def testValDefBuilderTraverseAndClose[A, B](inline expr: A): B = ${
    testValDefBuilderTraverseAndCloseImpl[A, B]('{ expr })
  }
  private def testValDefBuilderTraverseAndCloseImpl[A: Type, B: Type](expr: Expr[A])(using q: Quotes): Expr[B] =
    new ExprsFixtures(q).testValDefBuilderTraverseAndClose[A, B](expr)

  inline def testLambdaBuilderOfNAndBuild: Data = ${ testLambdaBuilderOfNAndBuildImpl }
  private def testLambdaBuilderOfNAndBuildImpl(using q: Quotes): Expr[Data] =
    new ExprsFixtures(q).testLambdaBuilderOfNAndBuild

  inline def testLambdaBuilderBuildWith: Data = ${ testLambdaBuilderBuildWithImpl }
  private def testLambdaBuilderBuildWithImpl(using q: Quotes): Expr[Data] =
    new ExprsFixtures(q).testLambdaBuilderBuildWith

  inline def testLambdaBuilderPartition[A](inline expr: A): Data = ${ testLambdaBuilderPartitionImpl[A]('{ expr }) }
  private def testLambdaBuilderPartitionImpl[A: Type](expr: Expr[A])(using q: Quotes): Expr[Data] =
    new ExprsFixtures(q).testLambdaBuilderPartition[A](expr)

  inline def testLambdaBuilderTraverse[A](inline expr: A): Data = ${ testLambdaBuilderTraverseImpl[A]('{ expr }) }
  private def testLambdaBuilderTraverseImpl[A: Type](expr: Expr[A])(using q: Quotes): Expr[Data] =
    new ExprsFixtures(q).testLambdaBuilderTraverse[A](expr)

  inline def testBidirectionalCodecs: Data = ${ testBidirectionalCodecsImpl }
  private def testBidirectionalCodecsImpl(using q: Quotes): Expr[Data] =
    new ExprsFixtures(q).testBidirectionalCodecs

  inline def testOneWayCodecs: Data = ${ testOneWayCodecsImpl }
  private def testOneWayCodecsImpl(using q: Quotes): Expr[Data] =
    new ExprsFixtures(q).testOneWayCodecs
}
