package hearth
package typed

import hearth.data.Data
import hearth.fp.effect.MIO
import hearth.fp.instances.*
import hearth.fp.syntax.*

import scala.quoted.*

/** Scala 3-only fixtures for testing [[passQuotes]] and [[withQuotes]] utilities with native Scala 3 quoting syntax.
  *
  * Unlike [[ExprsFixtures]] which uses cross-quotes ([[Expr.quote]] and [[Expr.splice]]), these fixtures use native
  * Scala 3 `'{...}` and `${...}` syntax, which requires explicit [[passQuotes]] and [[withQuotes]] calls to manage the
  * [[Quotes]] context.
  *
  * These are used in [[ExprsScala3Spec]] to verify that [[passQuotes]]/[[withQuotes]] correctly handle the scope issue
  * when building expressions with [[LambdaBuilder]] and [[ValDefBuilder]] in a Scala-3-only codebase.
  */
// format: off
final private class ExprsScala3Fixtures(q: Quotes) extends MacroCommonsScala3(using q) {

  // Cannot use Type[Int] in any annotations (parameter types, local vals) in scala-3 source files because the
  // cross-quotes plugin transforms these into self-referencing code, causing infinite loop errors.
  // Instead, pass the type as Any and cast to the expected type within a helper that delegates to a method
  // accepting Type[Int] implicitly in the shared source.

  /** Provides an implicit Type[Int] within a thunk by obtaining it from Type.of[Int] inside a passQuotes block. */
  private inline def withIntType[A](thunk: Type[Int] ?=> A): A =
    thunk(using Type.of[Int])

  // LambdaBuilder scope issue tests using passQuotes/withQuotes

  def testLambdaBuilderOf1ScopeIssue: Expr[Data] = {
    def lambda: Expr[Int => Int] = withQuotes {
      '{
        val x: Int => Int = ${ passQuotes {
          withIntType {
            MIO
              .scoped { runSafe =>
                runSafe {
                  LambdaBuilder
                    .of1[Int]("a")
                    .traverse[MIO, Expr[Int]] { a =>
                      MIO.pure(Expr.quote(Expr.splice(a) + 1))
                    }
                    .map(_.build[Int])
                }
              }
              .unsafe.runSync._2.fold(es => throw es.head, identity)
          }
        } }
        x
      }
    }
    withQuotes{'{ Data(${ passQuotes{lambda} }(2)) }}
  }

  def testLambdaBuilderOf2ScopeIssue: Expr[Data] = {
    def lambda: Expr[(Int, Int) => Int] = withQuotes {
      '{
        val x: (Int, Int) => Int = ${ passQuotes {
          withIntType {
            MIO
              .scoped { runSafe =>
                runSafe {
                  LambdaBuilder
                    .of2[Int, Int]("a", "b")
                    .traverse[MIO, Expr[Int]] { case ((a, b)) =>
                      MIO.pure(Expr.quote(Expr.splice(a) * Expr.splice(b) + 1))
                    }
                    .map(_.build[Int])
                }
              }
              .unsafe.runSync._2.fold(es => throw es.head, identity)
          }
        } }
        x
      }
    }
    withQuotes{'{ Data(${ passQuotes{lambda} }(2, 3)) }}
  }

  def testLambdaBuilderOf3ScopeIssue: Expr[Data] = {
    def lambda: Expr[(Int, Int, Int) => Int] = withQuotes {
      '{
        val x: (Int, Int, Int) => Int = ${ passQuotes {
          withIntType {
            MIO
              .scoped { runSafe =>
                runSafe {
                  LambdaBuilder
                    .of3[Int, Int, Int]("a", "b", "c")
                    .traverse[MIO, Expr[Int]] { case ((a, b, c)) =>
                      MIO.pure(Expr.quote(Expr.splice(a) * Expr.splice(b) * Expr.splice(c) + 1))
                    }
                    .map(_.build[Int])
                }
              }
              .unsafe.runSync._2.fold(es => throw es.head, identity)
          }
        } }
        x
      }
    }
    withQuotes{'{ Data(${ passQuotes{lambda} }(2, 3, 5)) }}
  }

  // ValDefBuilder scope issue tests using passQuotes/withQuotes

  def testValDefBuilderOfValScopeIssue: Expr[Data] = {
    def result: Expr[Int] = withQuotes {
      '{
        val x: Int = ${ passQuotes {
          withIntType {
            MIO
              .scoped { runSafe =>
                runSafe {
                  ValDefBuilder
                    .ofVal[Int]("v")
                    .traverse[MIO, Expr[Int]] { _ =>
                      MIO.pure(Expr.quote(42))
                    }
                    .map(_.build.close)
                }
              }
              .unsafe.runSync._2.fold(es => throw es.head, identity)
          }
        } }
        x
      }
    }
    withQuotes{'{ Data(${ passQuotes{result} }) }}
  }

  def testValDefBuilderOfVarScopeIssue: Expr[Data] = {
    def result: Expr[Int] = withQuotes {
      '{
        val x: Int = ${ passQuotes {
          withIntType {
            MIO
              .scoped { runSafe =>
                runSafe {
                  ValDefBuilder
                    .ofVar[Int]("v")
                    .traverse[MIO, Expr[Int]] { _ =>
                      MIO.pure(Expr.quote(42))
                    }
                    .map(_.build.close)
                }
              }
              .unsafe.runSync._2.fold(es => throw es.head, identity)
          }
        } }
        x
      }
    }
    withQuotes{'{ Data(${ passQuotes{result} }) }}
  }

  def testValDefBuilderOfLazyScopeIssue: Expr[Data] = {
    def result: Expr[Int] = withQuotes {
      '{
        val x: Int = ${ passQuotes {
          withIntType {
            MIO
              .scoped { runSafe =>
                runSafe {
                  ValDefBuilder
                    .ofLazy[Int]("v")
                    .traverse[MIO, Expr[Int]] { _ =>
                      MIO.pure(Expr.quote(42))
                    }
                    .map(_.build.close)
                }
              }
              .unsafe.runSync._2.fold(es => throw es.head, identity)
          }
        } }
        x
      }
    }
    withQuotes{'{ Data(${ passQuotes{result} }) }}
  }

  def testValDefBuilderOfDef0ScopeIssue: Expr[Data] = {
    def result: Expr[Int] = withQuotes {
      '{
        val x: Int = ${ passQuotes {
          withIntType {
            MIO
              .scoped { runSafe =>
                runSafe {
                  ValDefBuilder
                    .ofDef0[Int]("d")
                    .traverse[MIO, Expr[Int]] { _ =>
                      MIO.pure(Expr.quote(42))
                    }
                    .map(_.build.close)
                }
              }
              .unsafe.runSync._2.fold(es => throw es.head, identity)
          }
        } }
        x
      }
    }
    withQuotes{'{ Data(${ passQuotes{result} }) }}
  }

  def testValDefBuilderOfDef1ScopeIssue: Expr[Data] = {
    def result: Expr[Int] = withQuotes {
      '{
        val x: Int = ${ passQuotes {
          withIntType {
            MIO
              .scoped { runSafe =>
                runSafe {
                  ValDefBuilder
                    .ofDef1[Int, Int]("d", "a")
                    .traverse[MIO, Expr[Int]] { case (_, a) =>
                      MIO.pure(Expr.quote(Expr.splice(a) + 1))
                    }
                    .map(_.build.use(_(Expr.quote(2))))
                }
              }
              .unsafe.runSync._2.fold(es => throw es.head, identity)
          }
        } }
        x
      }
    }
    withQuotes{'{ Data(${ passQuotes{result} }) }}
  }
}
// format: on

object ExprsScala3Fixtures {

  inline def testLambdaBuilderOf1ScopeIssue: Data = ${ testLambdaBuilderOf1ScopeIssueImpl }
  private def testLambdaBuilderOf1ScopeIssueImpl(using q: Quotes): Expr[Data] =
    new ExprsScala3Fixtures(q).testLambdaBuilderOf1ScopeIssue

  inline def testLambdaBuilderOf2ScopeIssue: Data = ${ testLambdaBuilderOf2ScopeIssueImpl }
  private def testLambdaBuilderOf2ScopeIssueImpl(using q: Quotes): Expr[Data] =
    new ExprsScala3Fixtures(q).testLambdaBuilderOf2ScopeIssue

  inline def testLambdaBuilderOf3ScopeIssue: Data = ${ testLambdaBuilderOf3ScopeIssueImpl }
  private def testLambdaBuilderOf3ScopeIssueImpl(using q: Quotes): Expr[Data] =
    new ExprsScala3Fixtures(q).testLambdaBuilderOf3ScopeIssue

  inline def testValDefBuilderOfValScopeIssue: Data = ${ testValDefBuilderOfValScopeIssueImpl }
  private def testValDefBuilderOfValScopeIssueImpl(using q: Quotes): Expr[Data] =
    new ExprsScala3Fixtures(q).testValDefBuilderOfValScopeIssue

  inline def testValDefBuilderOfVarScopeIssue: Data = ${ testValDefBuilderOfVarScopeIssueImpl }
  private def testValDefBuilderOfVarScopeIssueImpl(using q: Quotes): Expr[Data] =
    new ExprsScala3Fixtures(q).testValDefBuilderOfVarScopeIssue

  inline def testValDefBuilderOfLazyScopeIssue: Data = ${ testValDefBuilderOfLazyScopeIssueImpl }
  private def testValDefBuilderOfLazyScopeIssueImpl(using q: Quotes): Expr[Data] =
    new ExprsScala3Fixtures(q).testValDefBuilderOfLazyScopeIssue

  inline def testValDefBuilderOfDef0ScopeIssue: Data = ${ testValDefBuilderOfDef0ScopeIssueImpl }
  private def testValDefBuilderOfDef0ScopeIssueImpl(using q: Quotes): Expr[Data] =
    new ExprsScala3Fixtures(q).testValDefBuilderOfDef0ScopeIssue

  inline def testValDefBuilderOfDef1ScopeIssue: Data = ${ testValDefBuilderOfDef1ScopeIssueImpl }
  private def testValDefBuilderOfDef1ScopeIssueImpl(using q: Quotes): Expr[Data] =
    new ExprsScala3Fixtures(q).testValDefBuilderOfDef1ScopeIssue
}
