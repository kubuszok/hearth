// Here we do not use:
//   package hearth
//   package demo
// because we want to show all the imports normal users would have to do.
package hearth.demo

import hearth.*
import hearth.fp.effect.*
import hearth.fp.instances.*
import hearth.fp.syntax.*

private[demo] trait ShowMacrosImpl { this: MacroCommons =>

  // TODO: Scala 2 cross-quotes do not handle this yet
  /** Derives a `Show[A]` type class instance for a given type `A`. */
  def deriveTypeClass[A: Type]: Expr[Show[A]] = Expr.quote {
    new Show[A] {
      def show(value: A): String = Expr.splice {
        deriveOrFail[A](Expr.quote(value), s"Show[${Type.prettyPrint[A]}] type class")
      }
    }
  }

  /** Derives a `String` representation of a given value of type `A` (inlined `Show[A].show`). */
  def deriveShowString[A: Type](value: Expr[A]): Expr[String] =
    deriveOrFail[A](value, s"Show[${Type.prettyPrint[A]}].show(${value.prettyPrint}) result")

  /** Converts the [[MIO]] results into an [[Expr]] or error message. */
  private def deriveOrFail[A: Type](value: Expr[A], name: String): Expr[String] = Log
    .namedScope(s"Derivation for $name") {
      attemptAllRules[A](value)
    }
    .expandFinalResultOrFail(name) { (errorLogs, errors) =>
      val errorsStr = errors.toVector
        .map {
          case DerivationError.UnsupportedType(typeName)           => s"Derivation of $typeName is not supported"
          case DerivationError.UnsupportedMethod(typeName, method) =>
            s"Derivation of $typeName.$method is not supported"
          case DerivationError.AssertionFailed(message) => s"Assertion failed: $message"
          case e                                        => s"Unexpected error: ${e.getMessage}"
        }
        .mkString("\n")

      s"""Failed to derive $name:
         |$errorsStr
         |Error logs:
         |$errorLogs
         |""".stripMargin
    }

  // All methods below implement the rules for deriving a `Show[A]` type class instance.

  /** The idea here is that we are attempting one derivation rule after another, and if one fails, we try the next one.
    *
    *   - successful MIO with Some(expr) -> derivation succeeded according to the rule
    *   - successful MIO with None -> the rule did not apply, we should try the next one
    *   - failed MIO -> the rule failed, we should fail the whole derivation
    */
  private type Attempt[A] = MIO[Option[Expr[A]]]

  /** Attempts one derivation rule after another, if none of them apply, we fail the derivation. */
  private def attemptAllRules[A: Type](value: Expr[A]): MIO[Expr[String]] = MIO.async { await =>
    await {
      attemptUsingImplicit[A](value)
    } orElse await {
      attemptAsBuiltIn[A](value)
    } orElse await {
      attempAsCaseClass[A](value)
    } orElse await {
      attemptAsEnum[A](value)
    } getOrElse await {
      MIO.fail(DerivationError.UnsupportedType(Type.prettyPrint[A]))
    }
  }

  /** Attempts to show `A` value using an implicit `Show[A]` value. */
  private def attemptUsingImplicit[A: Type](value: Expr[A]): Attempt[String] =
    Log.info(s"Attempting summoning implicit Show[${Type.prettyPrint[A]}] to show value") >> MIO {
      implicit val showA: Type[Show[A]] = showType[A]
      Expr.summonImplicit[Show[A]].map { show =>
        Expr.quote {
          Expr.splice(show).show(Expr.splice(value))
        }
      }
    }

  /** Attempts to show `A` value using a built-in handlers for primitive types. */
  private def attemptAsBuiltIn[A: Type](value: Expr[A]): Attempt[String] =
    Log.info(s"Attempting to use build-in support to show value of type ${Type.prettyPrint[A]}") >> MIO {
      if (Type[A] <:< Type.of[Boolean]) Some(Expr.quote {
        Expr.splice(value).toString
      })
      else if (Type[A] <:< Type.of[Byte]) Some(Expr.quote {
        Expr.splice(value).toString + ".toByte"
      })
      else if (Type[A] <:< Type.of[Short]) Some(Expr.quote {
        Expr.splice(value).toString + ".toShort"
      })
      else if (Type[A] <:< Type.of[Int]) Some(Expr.quote {
        Expr.splice(value).toString
      })
      else if (Type[A] <:< Type.of[Long]) Some(Expr.quote {
        Expr.splice(value).toString + "L"
      })
      else if (Type[A] <:< Type.of[Float]) Some(Expr.quote {
        Expr.splice(value).toString + "f"
      })
      else if (Type[A] <:< Type.of[Double]) Some(Expr.quote {
        Expr.splice(value).toString
      })
      else if (Type[A] <:< Type.of[Char]) Some(Expr.quote {
        "'" + Expr.splice(value).toString + "'"
      })
      else if (Type[A] <:< Type.of[String]) Some(Expr.quote {
        "\"" + Expr.splice(value) + "\""
      })
      else None
    }

  /** Attempts to show `A` value using a case class support. */
  private def attempAsCaseClass[A: Type](value: Expr[A]): Attempt[String] =
    Log.info(s"Attempting to use case class support to show value of type ${Type.prettyPrint[A]}") >>
      CaseClass.parse[A].traverse { caseClass =>
        caseClass
          .caseFieldValuesAt(value)
          .toList
          .parTraverse { case (name, fieldValue) =>
            import fieldValue.{Underlying as FieldType, value as fieldExpr}
            Log.namedScope(s"Attempting field ${Type.prettyPrint[FieldType]} of ${Type.prettyPrint[A]}") {
              attemptAllRules[FieldType](fieldExpr).map { result =>
                Expr.quote {
                  Expr.splice(Expr(name)) + " = " + Expr.splice(result)
                }
              }
            }
          }
          .map { fieldResults =>
            val name = Type.shortName[A]
            val inner = fieldResults
              .reduceOption { (a, b) =>
                Expr.quote {
                  Expr.splice(a) + ", " + Expr.splice(b)
                }
              }
              .getOrElse(Expr(""))
            Expr.quote {
              Expr.splice(Expr(name)) + "(" + Expr.splice(inner) + ")"
            }
          }
      }

  /** Attempts to show `A` value using an enum support. */
  private def attemptAsEnum[A: Type](value: Expr[A]): Attempt[String] =
    Log.info(s"Attempting to use enum support to show value of type ${Type.prettyPrint[A]}") >>
      Enum
        .parse[A]
        .traverse { enumm =>
          implicit val string: Type[String] = stringType
          enumm.matchOn(value) { matchedSubtype =>
            import matchedSubtype.{Underlying as B, value as matchedExpr}
            Log.namedScope(s"Attempting subtype ${Type.prettyPrint[B]} <: ${Type.prettyPrint[A]}") {
              attemptAllRules[B](matchedExpr)
            }
          }
        }
        .map(_.flatten)

  private def stringType: Type[String] = Type.of[String]
  private def showType[A: Type]: Type[Show[A]] = Type.of[Show[A]]
}

sealed private[demo] trait DerivationError extends scala.util.control.NoStackTrace with Product with Serializable
private[demo] object DerivationError {
  final case class UnsupportedType(typeName: String) extends DerivationError
  final case class UnsupportedMethod(typeName: String, field: String) extends DerivationError
  final case class AssertionFailed(message: String) extends DerivationError
}
