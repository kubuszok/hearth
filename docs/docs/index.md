---
hide:
  - navigation
  - toc
---

<p style="text-align: center"><img src="assets/images/logo.svg" alt="Hearth logo" style="height: 250px" /></p>

<h1 style="margin-bottom:0">Hearth</h1>
<h2 style="margin-top:0">The first Scala macros' standard library.</h2>

This is the first library focused on helping you write robust, easy-to-maintain macros that you can make cross-compilable between the Scala 2 and Scala 3 macro systems.

Introduces among others:

 * reliable extension methods that make checking properties of types, expressions and methods easier
 * Magnolia-like utilities for creating and decomposing data types
 * improvements over built-in types and expression printing utilities
 * small FP library that lets you reuse your Cats experience in macros (including Macro IO/MIO monad)
 * direct style utilities for working with cases that are hard to handle with monads and combinators (or even impossible to handle with them)
 * macro-extension system that allows you to extend your macros just by adding a dependency to the class path - without any additional imports!
 * and finally, a macro API that has implementations for Scala 2 and Scala 3 macro systems - you can use it with either or both!

## How to use the library for cross-compilable macros?

As a showcase, we will build a cross-compilable `Show` derivation.

??? example "`build.sbt` (assumes [sbt-crossproject](https://github.com/portable-scala/sbt-crossproject) or [sbt-projectmatrix](https://github.com/sbt/sbt-projectmatrix)/sbt 2)"

    ```scala
    // Add the core library
    libraryDependencies += "com.kubuszok" %%% "hearth" % "{{ hearth_version() }}"

    // Add the cross-quotes compiler plugin (but only on Scala 3, Scala 2.13 uses macros)
    libraryDependencies ++= CrossVersion.partialVersion(scalaVersion.value) match {
      case Some((3, _)) => Seq(compilerPlugin("com.kubuszok" % "hearth-cross-quotes" % "{{ hearth_version() }}_3"))
      case _            => Seq()
    }

    // If you want to enable debugging of cross-quotes, pass the right option to the macro/compiler plugin
    scalacOptions ++= CrossVersion.partialVersion(scalaVersion.value) match {
      case Some((3, _)) => Seq(
        // set to true OR file1.scala,file2.scala,... if you want to debug cross-quotes generation on Scala 3
        "-P:hearth.cross-quotes:logging=false"
      )
      case Some((2, 13)) => Seq(
        // set to true OR file1.scala,file2.scala,... if you want to debug cross-quotes generation on Scala 2
        "-Xmacro-settings:hearth.cross-quotes.logging=false"
      )
    }
    ```

The majority of the macro code is shared by putting it into a mix-in trait.

!!! example "`src/main/scala/demo/Show.scala` - shared `Show` type class"

    ```scala
    // file: src/main/scala/demo_sanely_automatic/Show.scala - part of Show example
    //> using scala {{ scala.newest_2_13 }} {{ scala.newest_3 }}
    //> using dep com.kubuszok::hearth:{{ hearth_version() }}
    package demo_sanely_automatic

    /** toString as a type class - easy to understand what this type class want to do. */
    trait Show[A] {

      def show(value: A): String
    }

    /** Companion will contain the derivation adapted specific to Scala language version. */
    object Show extends ShowCompanionCompat {

      def apply[A](implicit show: Show[A]): Show[A] = show

      /** Special type - is its implicit is in scope then macros will log the derivation process.
        *
        * @see
        *   [[hearth.demo_sanely_automatic.debug.logDerivation]] for details
        */
      sealed trait LogDerivation
      object LogDerivation extends LogDerivation
    }
    ```

!!! example "`src/main/scala/demo_sanely_automatic/ShowMacrosImpl.scala` - shared macro logic using [cross-quotes](cross-quotes.md)"

    ```scala
    // file: src/main/scala/demo_sanely_automatic/ShowMacrosImpl.scala - part of Show example
    package demo_sanely_automatic

    import hearth.*
    import hearth.fp.effect.*
    import hearth.fp.instances.*
    import hearth.fp.syntax.*

    /** Implementation of the [[Show]] macro, tested in [[ShowSpec]]. */
    private[demo_sanely_automatic] trait ShowMacrosImpl { this: MacroCommons =>

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
      
      /** Show.derived will be ignored - so this macro will never summon itself! */
      private lazy val ignoredImplicits: Seq[UntypedMethod] =
        Type
          .of[Show.type]
          .asUntyped
          .methods
          .collect {
            case method if method.name == "derived" => method
          }
          .toSeq

      /** Attempts to show `A` value using an implicit `Show[A]` value. */
      private def attemptUsingImplicit[A: Type](value: Expr[A]): Attempt[String] =
        Log.info(s"Attempting summoning implicit ${Types.Show[A].prettyPrint} to show value") >> MIO {
          implicit val showA: Type[Show[A]] = Types.Show[A]
          Expr.summonImplicitIgnoring[Show[A]](ignoredImplicits*).toOption.map { show =>
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
            .parTraverse { caseClass =>
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
              enumm.parMatchOn(value) { matchedSubtype =>
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

        val LogDerivation: Type[demo_sanely_automatic.Show.LogDerivation] = Type.of[demo_sanely_automatic.Show.LogDerivation]
        val String: Type[String] = Type.of[String]
        val Show: Type.Ctor1[Show] = Type.Ctor1.of[Show]
        val Iterable: Type.Ctor1[Iterable] = Type.Ctor1.of[Iterable]
      }
    }

    /** We can define our own ADT for errors, they are better than bunch of strings when we want to build a single, nice
      * error message.
      */
    sealed private[demo_sanely_automatic] trait DerivationError extends scala.util.control.NoStackTrace with Product with Serializable
    private[demo_sanely_automatic] object DerivationError {
      final case class UnsupportedType(typeName: String) extends DerivationError
      final case class UnsupportedMethod(typeName: String, field: String) extends DerivationError
      final case class AssertionFailed(message: String) extends DerivationError
    }
    ```

Then in Scala 2 and Scala 3-specific code you would write only adapters.

??? example "`src/main/scala-2/demo_sanely_automatic/ShowCompanionCompat.scala` - adapter for Scala 2"

    ```scala
    // file: src/main/scala-2/demo_sanely_automatic/ShowCompanionCompat.scala - part of Show example
    //> using target.scala {{ scala.newest_2_13 }}
    //> using options -Xsource:3
    package demo_sanely_automatic

    import scala.language.experimental.macros
    import scala.reflect.macros.blackbox

    private[demo_sanely_automatic] trait ShowCompanionCompat { this: Show.type =>

      implicit def derived[A]: Show[A] = macro ShowMacros.deriveTypeClassImpl[A]

      def show[A](value: A): String = macro ShowMacros.deriveShowStringImpl[A]
    }

    // I hope that one day most of it could be automated, but for now we have to bear.
    private[demo_sanely_automatic] class ShowMacros(val c: blackbox.Context) extends hearth.MacroCommonsScala2 with ShowMacrosImpl {

      def deriveTypeClassImpl[A: c.WeakTypeTag]: c.Expr[Show[A]] = deriveTypeClass[A]

      def deriveShowStringImpl[A: c.WeakTypeTag](value: c.Expr[A]): c.Expr[String] = deriveShowString[A](value)
    }
    ```

??? example "`src/main/scala-3/demo_sanely_automatic/ShowCompanionCompat.scala` - adapter for Scala 3"

    ```scala
    // file: src/main/scala-3/demo_sanely_automatic/ShowCompanionCompat.scala - part of Show example
    //> using target.scala {{ scala.newest_3 }}
    //> using plugin com.kubuszok::hearth-cross-quotes::{{ hearth_version() }}
    package demo_sanely_automatic

    import scala.quoted.*

    private[demo_sanely_automatic] trait ShowCompanionCompat { this: Show.type =>

      inline given derived[A]: Show[A] = ${ ShowMacros.deriveTypeClass[A] }

      inline def show[A](value: A): String = ${ ShowMacros.deriveShowString[A]('{ value }) }
    }

    // I hope that one day most of it could be automated, but for now we have to bear.
    private[demo_sanely_automatic] class ShowMacros(q: Quotes) extends hearth.MacroCommonsScala3(using q), ShowMacrosImpl

    private[demo_sanely_automatic] object ShowMacros {

      def deriveTypeClass[A: Type](using q: Quotes): Expr[Show[A]] = new ShowMacros(q).deriveTypeClass[A]

      def deriveShowString[A: Type](value: Expr[A])(using q: Quotes): Expr[String] =
        new ShowMacros(q).deriveShowString[A](value)
    }
    ```

??? example "`src/test/scala/demo_sanely_automatic/ShowCompanionCompat.scala` - tests"

    ```scala
    // file: src/test/scala/demo_sanely_automatic/ShowSpec.scala - part of Show example
    //> using test.dep org.scalameta::munit::{{ libraries.munit }}
    package demo_sanely_automatic

    /** Macro implementation of [[Show]] is in [[ShowMacrosImpl]]. */
    final class ShowSpec extends munit.FunSuite {

      test("Show should be able to derive type class for values with built-in support") {
        // import hearth.demo_sanely_automatic.debug.logDerivation // Uncomment to see how the derivation is done.

        assertEquals(Show.derived[Boolean].show(true), "true")
        assertEquals(Show.derived[Byte].show(1.toByte), "1.toByte")
        assertEquals(Show.derived[Short].show(1.toShort), "1.toShort")
        assertEquals(Show.derived[Int].show(1), "1")
        assertEquals(Show.derived[Long].show(1L), "1L")
        assertEquals(Show.derived[Float].show(1.0f), "1.0f")
        assertEquals(Show.derived[Double].show(1.0), "1.0")
        assertEquals(Show.derived[Char].show('a'), "'a'")
        assertEquals(Show.derived[String].show("hello"), "\"hello\"")
      }

      test("Show should be able to derive type class for values with iterable support") {
        // import hearth.demo_sanely_automatic.debug.logDerivation // Uncomment to see how the derivation is done.

        assertEquals(Show.derived[Iterable[Int]].show(List(1, 2, 3)), "List(1, 2, 3)")
      }

      test("Show should be able to derive type class for values with case class support") {
        // import hearth.demo_sanely_automatic.debug.logDerivation // Uncomment to see how the derivation is done.

        case class Person(name: String, age: Int)
        assertEquals(Show.show(Person("John", 30)), "Person(name = \"John\", age = 30)")
      }

      test("Show should be able to derive type class for values with enum support") {
        // import hearth.demo_sanely_automatic.debug.logDerivation // Uncomment to see how the derivation is done.

        sealed trait Color
        case object Red extends Color
        case object Green extends Color
        case object Blue extends Color

        def impl(color: Color): String = Show.show(color)
        assertEquals(impl(Red), "Red")
        assertEquals(impl(Green), "Green")
        assertEquals(impl(Blue), "Blue")
      }

      test("Show should be able to inline showing for values with built-in support") {
        // import hearth.demo_sanely_automatic.debug.logDerivation // Uncomment to see how the derivation is done.

        assertEquals(Show.show(true), "true")
        assertEquals(Show.show(1.toByte), "1.toByte")
        assertEquals(Show.show(1.toShort), "1.toShort")
        assertEquals(Show.show(1), "1")
        assertEquals(Show.show(1L), "1L")
        assertEquals(Show.show(1.0f), "1.0f")
        assertEquals(Show.show(1.0), "1.0")
        assertEquals(Show.show('a'), "'a'")
        assertEquals(Show.show("hello"), "\"hello\"")
      }

      test("Show should be able to inline showing for values with iterable support") {
        // import hearth.demo_sanely_automatic.debug.logDerivation // Uncomment to see how the derivation is done.

        assertEquals(Show.show(List(1, 2, 3)), "List(1, 2, 3)")
      }

      test("Show should be able to inline showing for values with case class support") {
        // import hearth.demo_sanely_automatic.debug.logDerivation // Uncomment to see how the derivation is done.

        case class Person(name: String, age: Int)
        assertEquals(Show.derived[Person].show(Person("John", 30)), "Person(name = \"John\", age = 30)")
      }

      test("Show should be able to inline showing for values with enum support") {
        // import hearth.demo_sanely_automatic.debug.logDerivation // Uncomment to see how the derivation is done.

        sealed trait Color
        case object Red extends Color
        case object Green extends Color
        case object Blue extends Color

        assertEquals(Show.derived[Color].show(Red), "Red")
        assertEquals(Show.derived[Color].show(Green), "Green")
        assertEquals(Show.derived[Color].show(Blue), "Blue")
      }
    }
    ```
