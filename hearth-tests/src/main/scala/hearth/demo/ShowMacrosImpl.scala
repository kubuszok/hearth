// We've put things into a separate package and do not use:
//   package hearth
//   package demo
// here, because we want to show all the imports normal users would have to do.
package hearth.demo

import hearth.*
import hearth.fp.effect.*
import hearth.fp.instances.*
import hearth.fp.syntax.*

/** Implementation of the [[Show]] macro, tested in [[ShowSpec]]. */
private[demo] trait ShowMacrosImpl { this: MacroCommons =>

  /** Derives a `Show[A]` type class instance for a given type `A`. */
  def deriveTypeClass[A: Type]: Expr[Show[A]] = Expr.quote {
    new Show[A] {
      def show(value: A): String = Expr.splice {
        deriveOrFail[A](Expr.quote(value), s"${Types.Show[A].prettyPrint} type class")
      }
    }
  }

  /** Derives a `String` representation of a given value of type `A` (inlined `Show[A].show`). */
  def deriveShowString[A: Type](value: Expr[A]): Expr[String] =
    deriveOrFail[A](value, s"${Types.Show[A].prettyPrint}.show(${value.prettyPrint}) result")

  /** Converts the [[MIO]] results into an [[Expr]] or error message. */
  private def deriveOrFail[A: Type](value: Expr[A], name: String): Expr[String] = Log
    .namedScope(s"Derivation for $name") {
      Log.info(s"Macro expansion started at ${Environment.currentPosition.prettyPrint}") >>
        attemptAllRules[A](value)
    }
    .runToExprOrFail(
      name,
      infoRendering = if (shouldWeLogDerivation) RenderFrom(Log.Level.Info) else DontRender
    ) { (errorLogs, errors) =>
      // errorLogs: String - pretty-printed log
      // errors: NonEmptyVector[Throwable] - errors that happened during the derivation (if it succeeded, this wouldn't be called)

      val errorsStr = errors.toVector
        .map {
          case DerivationError.UnsupportedType(typeName)           => s"Derivation of $typeName is not supported"
          case DerivationError.UnsupportedMethod(typeName, method) =>
            s"Derivation of $typeName.$method is not supported"
          case DerivationError.AssertionFailed(message) => s"Assertion failed: $message"
          case e => s"Unexpected error: ${e.getMessage}:\n${e.getStackTrace.mkString("\n")}"
        }
        .mkString("\n")

      if (errorLogs.nonEmpty) {
        s"""Failed to derive $name:
           |$errorsStr
           |Error logs:
           |$errorLogs
           |""".stripMargin
      } else {
        s"""Failed to derive $name:
           |$errorsStr
           |""".stripMargin
      }
    }

  /** Enables logging if we either:
    *   - import [[hearth.demo.debug.logDerivation]] in the scope
    *   - have set scalac option `-Xmacro-settings:show.logDerivation=true`
    */
  private def shouldWeLogDerivation: Boolean = {
    implicit val LogDerivation: Type[Show.LogDerivation] = Types.LogDerivation
    def logDerivationImported = Expr.summonImplicit[Show.LogDerivation].isDefined

    def logDerivationSetGlobally = (for {
      data <- Environment.typedSettings.toOption
      show <- data.get("show")
      shouldLog <- show.get("logDerivation").flatMap(_.asBoolean)
    } yield shouldLog).getOrElse(false) // We don't want to fail the derivation if we can't parse the settings.

    logDerivationImported || logDerivationSetGlobally
  }

  // All methods below implement the rules for deriving a `Show[A]` type class instance.

  /** The idea here is that we are attempting one derivation rule after another, and if one fails, we try the next one.
    *
    *   - successful MIO with Some(expr) -> derivation succeeded according to the rule
    *   - successful MIO with None -> the rule does not apply, we should try the next one
    *   - failed MIO -> the rule does apply but it failed, we should fail the whole derivation
    *
    * If none of the rules matched, then we fail derivation as well.
    */
  private type Attempt[A] = MIO[Option[Expr[A]]]

  /** Attempts one derivation rule after another, if none of them apply, we fail the derivation. */
  private def attemptAllRules[A: Type](value: Expr[A]): MIO[Expr[String]] = MIO.scoped { runSafe =>
    runSafe {
      attemptUsingImplicit[A](value)
    } orElse runSafe {
      attemptAsBuiltIn[A](value)
    } orElse runSafe {
      attemptAsIterable[A](value)
    } orElse runSafe {
      attemptAsCaseClass[A](value)
    } orElse runSafe {
      attemptAsEnum[A](value)
    } getOrElse runSafe {
      MIO.fail(DerivationError.UnsupportedType(Type.prettyPrint[A]))
    }
  }

  /** Attempts to show `A` value using an implicit `Show[A]` value. */
  private def attemptUsingImplicit[A: Type](value: Expr[A]): Attempt[String] =
    Log.info(s"Attempting summoning implicit ${Types.Show[A].prettyPrint} to show value") >> MIO {
      implicit val showA: Type[Show[A]] = Types.Show[A]
      Expr.summonImplicit[Show[A]].map { show =>
        Expr.quote {
          Expr.splice(show).show(Expr.splice(value))
        }
      }
    }.flatTap {
      case Some(expr) => Log.info(s"Successfully summoned ${Types.Show[A].prettyPrint}:\n${expr.prettyPrint}")
      case None       => Log.info(s"Failed to summon ${Types.Show[A].prettyPrint}")
    }

  /** Attempts to show `A` value using a built-in handlers for primitive types. */
  private def attemptAsBuiltIn[A: Type](value: Expr[A]): Attempt[String] =
    Log.info(s"Attempting to use built-in support to show value of type ${Type.prettyPrint[A]}") >> MIO {
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
        val result = Expr.splice(value).toString
        // Workaround for https://www.scala-js.org/doc/semantics.html#tostring-of-float-double-and-unit
        val workaround = if (result.contains(".")) result else (result + ".0")
        workaround + "f"
      })
      else if (Type[A] <:< Type.of[Double]) Some(Expr.quote {
        val result = Expr.splice(value).toString
        // Workaround for https://www.scala-js.org/doc/semantics.html#tostring-of-float-double-and-unit
        val workaround = if (result.contains(".")) result else (result + ".0")
        workaround
      })
      else if (Type[A] <:< Type.of[Char]) Some(Expr.quote {
        "'" + Expr.splice(value).toString + "'"
      })
      else if (Type[A] <:< Type.of[String]) Some(Expr.quote {
        "\"" + Expr.splice(value) + "\""
      })
      else None
    }.flatTap {
      case Some(expr) =>
        Log.info(
          s"Successfully used built-in support to show value of type ${Type.prettyPrint[A]}:\n${expr.prettyPrint}"
        )
      case None => Log.info(s"Failed to use built-in support to show value of type ${Type.prettyPrint[A]}")
    }

  /** Attempts to show `A` value using a iterable support. */
  private def attemptAsIterable[A: Type](value: Expr[A]): Attempt[String] =
    Log.info(s"Attempting to use iterable support to show value of type ${Type.prettyPrint[A]}") >>
      Types.Iterable
        .unapply(Type[A])
        .traverse { innerType =>
          // It is currently required to have a separate method with `B: Type` for Scala 2 to find the implicit `Type[B]`.
          // (Scala 2 doesn't find implicit `Type[B]` if provided in another way, and we are PoC-quality now).
          def showIterable[B: Type](
              iterableExpr: Expr[Iterable[B]]
          )(f: Expr[B] => Expr[String]): Expr[String] =
            Expr.quote {
              Expr
                .splice {
                  iterableExpr
                }
                .map { item =>
                  Expr.splice {
                    f(Expr.quote(item))
                  }
                }
                .toString
            }

          MIO.scoped { runSafe =>
            import innerType.Underlying as B
            implicit val IterableB: Type[Iterable[B]] = Types.Iterable[B] // for .upcast[Iterable[B]]

            showIterable[B](value.upcast[Iterable[B]]) { item =>
              runSafe {
                Log.namedScope(s"Iterables inner type: ${Type.prettyPrint[B]}") {
                  attemptAllRules[B](item)
                }
              }
            }
          }
        }
        .flatTap {
          case Some(expr) =>
            Log.info(
              s"Successfully used iterable support to show value of type ${Type.prettyPrint[A]}:\n${expr.prettyPrint}"
            )
          case None => Log.info(s"Failed to use iterable support to show value of type ${Type.prettyPrint[A]}")
        }

  /** Attempts to show `A` value using a case class support. */
  private def attemptAsCaseClass[A: Type](value: Expr[A]): Attempt[String] =
    Log.info(s"Attempting to use case class support to show value of type ${Type.prettyPrint[A]}") >>
      CaseClass
        .parse[A]
        .traverse { caseClass =>
          val nameExpr = Expr(Type.shortName[A])

          if (Type[A].isCaseObject || Type[A].isCaseVal) {
            MIO.pure(nameExpr)
          } else {
            caseClass
              .caseFieldValuesAt(value)
              .toList
              .parTraverse { case (name, fieldValue) =>
                import fieldValue.{Underlying as FieldType, value as fieldExpr}
                Log.namedScope(s"Attempting field `$name`: ${Type.prettyPrint[FieldType]} of ${Type.prettyPrint[A]}") {
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
        }
        .flatTap {
          case Some(expr) =>
            Log.info(
              s"Successfully used case class support to show value of type ${Type.prettyPrint[A]}:\n${expr.prettyPrint}"
            )
          case None => Log.info(s"Failed to use case class support to show value of type ${Type.prettyPrint[A]}")
        }

  /** Attempts to show `A` value using an enum support. */
  private def attemptAsEnum[A: Type](value: Expr[A]): Attempt[String] =
    Log.info(s"Attempting to use enum support to show value of type ${Type.prettyPrint[A]}") >>
      Enum
        .parse[A]
        .traverse { enumm =>
          implicit val String: Type[String] = Types.String
          enumm.matchOn(value) { matchedSubtype =>
            import matchedSubtype.{Underlying as B, value as matchedExpr}
            Log.namedScope(s"Attempting subtype ${Type.prettyPrint[B]} <: ${Type.prettyPrint[A]}") {
              attemptAllRules[B](matchedExpr)
            }
          }
        }
        .map(_.flatten)
        .flatTap {
          case Some(expr) =>
            Log.info(
              s"Successfully used enum support to show value of type ${Type.prettyPrint[A]}:\n${expr.prettyPrint}"
            )
          case None => Log.info(s"Failed to use enum support to show value of type ${Type.prettyPrint[A]}")
        }

  /** We cannot make these implicits "global", or they would resolve to themselves. So we put them here and refer to
    * them inside methods when we need them.
    */
  private object Types {

    val LogDerivation: Type[demo.Show.LogDerivation] = Type.of[demo.Show.LogDerivation]
    val String: Type[String] = Type.of[String]
    val Show: Type.Ctor1[Show] = Type.Ctor1.of[Show]
    val Iterable: Type.Ctor1[Iterable] = Type.Ctor1.of[Iterable]
  }
}

/** We can define our own ADT for errors, they are better than bunch of strings when we want to build a single, nice
  * error message.
  */
sealed private[demo] trait DerivationError extends scala.util.control.NoStackTrace with Product with Serializable
private[demo] object DerivationError {
  final case class UnsupportedType(typeName: String) extends DerivationError
  final case class UnsupportedMethod(typeName: String, field: String) extends DerivationError
  final case class AssertionFailed(message: String) extends DerivationError
}
