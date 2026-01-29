# Standard Extensions

While the [Basic Utilities](basic-utilities.md) try to be reasonably unopinionated, sometimes we can consider
solutions that require us to follow some convention, but in turn give us a lot out of the box. To not mix
these unopinionated core utilities and opinionated solution, we put the latter under `std` package as opt-in
mixins.

## Rule-based derivation

`Rules` is a utility that helps you combine multiple derivation rules into a single rule application system. It tries rules in order until one matches, or collects failure reasons from all rules if none match. This is particularly useful for building flexible macro systems where you want to try multiple strategies for deriving code (e.g., JSON encoders/decoders, type mappers, serializers).

### Introduction to rules

Let's say we need to derive a JSON encoder. After drafting the logic it should follow, we would probably arrive more or less at something like that:

 * if there is an existing implicit in scope - use it
 * if the type we are deriving for is a collection, use recursion to figure out how to encode its items, and then handle the collection
 * if the type we are deriving for is an `Option`, use recursion to figure out how to encode its item, and handle as well the case when the value is `null`
 * if the type we are deriving for is a `case class`, use recursion to figuer out how to encode each field

(For simplicity, we're skipping handling of enums, maps, value types, etc).

We could try to implement it with if-elses:

```scala
val IterableType = Type.Ctor1.of[Iterable]
val OptionType = Type.Ctor1.of[Option]

lazy val asImplicit = Expr.summonImplicit[A]
lazy val asIterable = IterableType.unapply(A)
lazy val asOption = OptionType.unapply(A)
lazy val asCaseClass = CaseClass.parse(A).toOption

if (asImplicit.isDefined) { ... }
else if (asIterable.isDefined) { ... }
else if (asOption.isDefined) { ... }
else if (asCaseClass.isDefined) { ... }
else Environment.reportErrorAndAbort("...")
```

but it quickly becomes a giant method that is hard to read, and any adjustments to the methods requires shuffling a lot of code around.

But, we can use the fact that this code follows the patter of "if it matches, try derivation that might fail, or yield to the next rule".
For start, we can start with handling these as `Option`s:

```scala
def asImplicit[A: Type]: Option[...] = {
  Expr.summonImplicit[A].map { implicitExpr =>
    // ...
  }
}
def asOption[A: Type]: Option[...] = {
  val IterableType = Type.Ctor1.of[Iterable]
  A match {
    case IterableType(item) => // ...
    case _ => None
  }
}
def asIterable[A: Type]: Option[...] = {
  val IterableType = Type.Ctor1.of[Iterable]
  Type[A] match {
    case IterableType(item) => // ...
    case _ => None
  }
}
def asCaseClass[A: Type]: Option[...] = CaseClass.parse[A].map { caseClass =>
  // ...
}

def deriveRecursively[A: Type] =
  asImplicit[A]
    .orElse(asOption[A])
    .orElse(asIterable[A])
    .orElse(asCaseClass[A])
    .getOrElse(Environment.reportErrorAndAbort("..."))
```

That is much easier to maintain:

 - each "rule" that we described in our specification becomes a separate method
 - their order is easy to follow and adjust
 - it is easy to split some rule to its subcases, and combine them back again

But, let's say, we want to also add some [logging](micro-fp.md#logging) to the derivation, e.g using `MIO`. It is still possible:

```scala
def asImplicit[A: Type]: MIO[Option[...]] = ...
def asOption[A: Type]: MIO[Option[...]] = ...
def asIterable[A: Type]: MIO[Option[...]] = ...
def asCaseClass[A: Type]: MIO[Option[...]] = ...

def deriveRecursively[A: Type] = MIO.scoped { runSafe =>
  runSafe(asImplicit[A])
    .orElse(runSafe(asOption[A]))
    .orElse(runSafe(asIterable[A]))
    .orElse(runSafe(asCaseClass[A]))
    .getOrElse(runSafe(MIO.fail(...)))
}
```

However at this point `Option` is no longer as self-explanatory. If we also wanted to log a reason why each rule yielded
in `MIO.fail(...)`, we would have to replace it with some `Either`, and then somehow manually aggregate the reasons for each rule
(e.g. there is implicit but there is ambiguity; something is iterable, but its elements cannot be rendered, etc).

While it's not difficult task to write such aggregator, one might want for some existing solution to exist, one that would
standardize the appoach to such problem.

That's what `Rule` and `Rules` in `hearth.std` are providing.

### Core Concepts

- **`Rule`** - A trait that marks something as a rule. Each rule has a `name` for identification.
- **`Rule.Applicability[A]`** - The result of applying a rule:
  - `Matched(result)` - The rule matched and produced a result of type `A`
  - `Yielded(reasons)` - The rule didn't match, but provides reasons why (as `Vector[String]`)
- **`Rules[R]`** - A class that combines multiple rules of type `R` and tries them in order
- **`ApplicationResult[R, A]`** - The result type: `Either[ListMap[R, Vector[String]], A]`
  - `Right(result)` - One of the rules matched and produced a result
  - `Left(failureMap)` - All rules failed, with a map of each rule to its failure reasons

### Basic Usage

The simplest way to use `Rules` is with synchronous rule application:

!!! example "Synchronous rule application"

    ```scala
    // file: src/main/scala/example/RulesExample.scala - part of basic Rules example
    //> using scala {{ scala.2_13 }} {{ scala.3 }}
    //> using dep com.kubuszok::hearth::{{ hearth_version() }}
    package example

    import hearth.std.{Rule, Rules}

    abstract class OurRule extends Rule with Product with Serializable {
      def attempt: Rule.Applicability[Int]
    }

    // Define a simple rule that always matches
    final case class MatchingRule(name: String, result: Int) extends OurRule {
      override def attempt: Rule.Applicability[Int] = Rule.matched(result)
    }

    // Define a rule that always yields (doesn't match)
    final case class YieldingRule(name: String, reasons: Vector[String]) extends OurRule {
      override def attempt: Rule.Applicability[Int] = Rule.yielded((reasons.toSeq): _*)
    }

    object RulesExample {

      def applyRules(): Rules.ApplicationResult[OurRule, Int] = {
        val rule1 = YieldingRule("rule1", Vector("Type not supported"))
        val rule2 = MatchingRule("rule2", 42)
        val rule3 = MatchingRule("rule3", 99)

        Rules(rule1, rule2, rule3)(_.attempt)
      }
    }
    ```

    ```scala
    // file: src/test/scala/example/RulesExampleSpec.scala - part of basic Rules example
    //> using test.dep org.scalameta::munit::{{ libraries.munit }}
    package example

    final class RulesExampleSpec extends munit.FunSuite {

      test("Rules tries rules in order until one matches") {
        val result = RulesExample.applyRules()
        assert(result.isRight)
        assertEquals(result, Right(42)) // rule2 matches, rule3 is never tried
      }
    }
    ```

In this example, `Rules` tries `rule1` first, which yields (doesn't match). Then it tries `rule2`, which matches and returns `42`. `rule3` is never tried because `rule2` already matched.

### Effectful Rule Application

`Rules` also supports effectful rule application using `DirectStyle`, allowing you to work with effects like `Option`, `Either`, `Try`, or `MIO`:

!!! example "Effectful rule application"

    ```scala
    // file: src/main/scala/example/EffectfulRulesExample.scala - part of effectful Rules example
    //> using scala {{ scala.2_13 }} {{ scala.3 }}
    //> using dep com.kubuszok::hearth::{{ hearth_version() }}
    //> using dep com.kubuszok::hearth-micro-fp::{{ hearth_version() }}
    package example

    import hearth.std.{Rule, Rules}
    import hearth.fp.DirectStyle
    import hearth.fp.instances._
    import scala.util.{Failure, Success, Try}

    // Rule that might fail when attempting
    final case class EffectfulRule(name: String, shouldMatch: Boolean, result: Int) extends Rule {
      def attempt: Option[Rule.Applicability[Int]] = {
        if (shouldMatch) Some(Rule.Applicability.Matched(result))
        else Some(Rule.Applicability.Yielded(Vector("Condition not met")))
      }
    }

    object EffectfulRulesExample {

      def applyWithOption(): Option[Rules.ApplicationResult[EffectfulRule, Int]] = {
        val rule1 = EffectfulRule("rule1", shouldMatch = false, 1)
        val rule2 = EffectfulRule("rule2", shouldMatch = true, 42)

        Rules(rule1, rule2)[Option, Int](r => r.attempt)
      }

      def applyWithEither(): Either[String, Rules.ApplicationResult[EffectfulRule, Int]] = {
        val rule1 = EffectfulRule("rule1", shouldMatch = false, 1)
        val rule2 = EffectfulRule("rule2", shouldMatch = true, 42)

        type EitherString[A] = Either[String, A]
        Rules(rule1, rule2)[EitherString, Int](r => Right(r.attempt.get))
      }

      def applyWithTry(): Try[Rules.ApplicationResult[EffectfulRule, Int]] = {
        val rule1 = EffectfulRule("rule1", shouldMatch = false, 1)
        val rule2 = EffectfulRule("rule2", shouldMatch = true, 42)

        Rules(rule1, rule2)[Try, Int](r => Success(r.attempt.get))
      }
    }
    ```

    ```scala
    // file: src/test/scala/example/EffectfulRulesExampleSpec.scala - part of effectful Rules example
    //> using test.dep org.scalameta::munit::{{ libraries.munit }}
    package example

    import scala.util.Success

    final class EffectfulRulesExampleSpec extends munit.FunSuite {

      test("Rules works with Option effect") {
        val result = EffectfulRulesExample.applyWithOption()
        assertEquals(result, Some(Right(42)))
      }

      test("Rules works with Either effect") {
        val result = EffectfulRulesExample.applyWithEither()
        assertEquals(result, Right(Right(42)))
      }

      test("Rules works with Try effect") {
        val result = EffectfulRulesExample.applyWithTry()
        assertEquals(result, Success(Right(42)))
      }
    }
    ```

### Realistic Use Case: JSON Encoder Rules

The examples above show how the API works, but not how it could be used in a macro.

Here's a more realistic example showing how you might use `Rules` to build a JSON encoder system with multiple derivation strategies:

!!! example "JSON encoder with multiple derivation rules"

    ```scala
    import hearth.*
    import hearth.fp.effect.*
    import hearth.fp.instances.*
    import hearth.fp.syntax.*

    // Mostly pseudo-code, at least for now

    trait EncodingExample { this: MacroCommons =>

      // The API called by macro adapters

      def deriveInlineExpr[A: Type](expr: Expr[A]): Expr[Json] =
        deriveExprRecursively(EncodingContext(expr, Type[A]))
          .runToExprOrFail("deriveInline")(renderFailure)

      def deriveEncoderExpr[A: Type]: Expr[Encoder[A]] = MIO.scoped { runSafe =>
        Expr.quote {
          new Encoder[A] {
            def encode(value: A): Json = Expr.splice(runSafe(deriveExprRecursivel(EncodingContext(Expr.quote(value), Type[A]))))
          }
        }
      }
        .runToExprOrFail("deriveEncoder")(renderFailure)

      // The actual macro implementation: we'll start with defining types that we'll use to express our logic.

      /** To pass around: types, expressions, flags, etc */
      case class EncodingContext[A](
        encodedExpr: Expr[A],
        encodedType: Type[A]
      )

      /** To represent failed derivation with some context */
      case class EncodingFailure(
        ruleName: String,
        reason: String
      ) extends Throwable

      /** Abstract rule for JSON encoding */
      abstract class EncoderRule(override val name: String) extends Rule {
        def attempt[A](ctx: EncodingContext[A]): MIO[Rule.Applicability[Expr[Json]]]
      }

      // Then, we will use these types to define our rule-based derivation.

      def deriveExprRecursively[A](ctx: EncodingContext[A]): MIO[Expr[JSON]] = Rules(
        AttemptUsingImplicit,
        AttemptAsOption,
        AttemptAsIterable,
        AttemptAsCaseClass
      ) { encoderRule =>
        encoderRule.attempt(ctx)
      }.flatMap {
        case Left(reasons) =>
          MIO.fail(reasons.toNonEmptyVector.map { case (rule, reason) =>
            RuleFailure(rule, reason.mkString("\n"))
          })
        case Right(result) =>
          MIO.pure(result)
      }

      /** Rule 1: Try to encode using implicit */
      object AttemptUsingImplicit extends EncoderRule("attempt using implicit") {
        lazy val EncoderType = Type.Ctor1.of[Encoder]

        def attempt[A](ctx: EncoderContext[A]): MIO[Rule.Applicability[Expr[Json]]] =
          Expr.summonImplicit(using EncoderType(using ctx.encodedType)).map { encoderExpr =>
            Expr.quote {
              Expr.splice(encoderExpr).encode(Expr.splice(ctx.encodedExpr))
            }
          }
      }

      /** Rule 2: Try to encode as Option */
      object AttemptAsOption extends EncoderRule("attempt as Option") {

        def attempt[A](ctx: EncoderContext[A]): MIO[Rule.Applicability[Expr[Json]]] = ctx.encoder match {
          lazy val OptionType = Type.Ctor1.of[Option]

          // There are better ways to implement it, see IsOption section!
          case OptionType(item) =>
            import item.Underlying as Item
            LambdaBuilder.of[Item]("value")
              .traverse { itemExpr =>
                deriveExprRecursively(EncodingContext(itemExpr, Item))
              }
              .map(_.build)
              .map { lambda =>
                // There are better ways to implement it, see IsOption section!
                val option = ctx.encodedExpr.asInstanceOf[Expr[Option[Item]]]
                Expr.quote {
                  Expr.splice(option).fold(Json.empty)(Expr.splice(lambda))
                }
              }
          case _ => MIO.pure(Rule.yielded(s"${ctx.encodedType.prettyPrint} is not an Option"))
        }
      }

      /** Rule 3: Try to encode as Iterable */
      object AttemptAsIterable extends EncoderRule("attempt as Iterable") {
        lazy val IterableType = Type.Ctor1.of[Iterable]

        def attempt[A](ctx: EncoderContext[A]): MIO[Rule.Applicability[Expr[Json]]] = ctx.encoder match {
          // There are better ways to implement it, see IsCollection section!
          case IterableType(item) =>
            import item.Underlying as Item
            LambdaBuilder.of[Item]("value")
              .traverse { itemExpr =>
                deriveExprRecursively(EncodingContext(itemExpr, Item))
              }
              .map(_.build)
              .map { lambda =>
                // There are better ways to implement it, see IsCollection section!
                val iterable = ctx.encodedExpr.asInstanceOf[Expr[Iterable[Item]]]
                Expr.quote {
                  Json.arr(Expr.splice(iterable).map(Expr.splice(lambda)))
                }
              }
          case _ => MIO.pure(Rule.yielded(s"${ctx.encodedType.prettyPrint} is not an Iterable"))
        }
      }

      /** Rule 4: Try to encode as case class */
      object AttemptAsOption extends EncoderRule("attempt as case class") {

        def attempt[A](ctx: EncoderContext[A]): MIO[Rule.Applicability[Expr[Json]]] = CaseClass.parse[A] match {
          case Some(caseClass) =>
            caseClass.fieldValuesAt(ctx.encodedExpr).toList.parTraverse { case (fieldName, fieldValue) =>
              fieldName.{Underlying as Field, value}
              deriveExprRecursively(EncodingContext(value, Field)).map { jsonExpr =>
                val keyExpr = Expr(fieldName)
                Expr.quote {
                  Expr.splice(keyExpr) -> Expr.splice(jsonExpr)
                }
              }
            }.map { exprList =>
              Expr.quote {
                Json.obj(Expr.splice(VarArgs.from(exprList))*)
              }
            }
          case None => MIO.pure(Rule.yielded(s"${ctx.encodedType.prettyPrint} is not a case class"))
        }
      }

      lazy val renderFailure: (String, fp.data.NonEmptyVector[Throwable]) => String = (_, errors) => {
        errors.map {
          case EncodingFailure(ruleName, reasons) =>
            if (reasons.isEmpty)
              s"""$ruleName failed"""
            else
              s"""$ruleName failed because:
                 |${reasons.split("\n").map("  " + _).mkString("\n")}""".stripMargin
          case throwable =>
            s"Unexpected error: ${throwable.getMessage}"
        }.mkString("\n\n")
      }
    }
    ```

### Key Points

- **Order matters**: Rules are tried in the order you provide them. Once a rule matches, subsequent rules are not tried.
- **Failure reasons**: When a rule doesn't match, it can provide reasons why. This is useful for debugging and error messages.
- **Flexible signatures**: Rules can have different method signatures (different type parameters, different context parameters). You tell `Rules` how to call each rule via the `attempt` function.
- **Effect support**: `Rules` supports both synchronous and effectful rule application through `DirectStyle`, making it work seamlessly with `Option`, `Either`, `Try`, `MIO`, and other effects.
- **Type safety**: The result type `ApplicationResult[R, A]` ensures you handle both success and failure cases.

This design allows you to build flexible, extensible macro systems where you can easily add new derivation strategies by implementing new rules and adding them to your `Rules` instance.

## Standard Macro Extensions

Core library allows us to define [macro extensions](basic-utilities.md#macro-extensions):

 - we define some trait, tell it how much it knows about our quoting context
 - then we load it in a macro, so that it would execute some code, e.g. injecting new capabilities into our codebase

But there are no built-in macro extensions in the bare `MacroCommons`, since every such extensio is a design decision.

But we can predefine a set of such extensions as a proposal of usable API that Hearth users can opt-in into using, because they could be immensly useful.

### Requirements

 1. we need to mix-in `hearth.std.StdExtensions` trait
 2. we need to load extensions via `Environment.loadStandardExtensions()` before using any of the following interfaces

### `IsCollection` macro extension

`IsCollection` allows you to check if a provided `Type[A]` can be considered a collection, that is:

 - it can be iterated over
 - it can be constructed with a `Factory`

It is not only more convenient that just using `Type.Ctor1[Iterable]` to `unapply` on type, then upcasting expression to `Expr[Iterable[Item]]`,
but also out-of-the-box supports types what are not Scala collections, but we want to treat them as such:

 - `Array`s
 - `IArray`s (immutable arrays from Scala 3)
 - Java collections

with a possibility to support even more types, with the same API, just by adding a macro extension implementation to the class-path!

How could we use this API?

!!! example "Cross-compilable `IsCollection`"

    We can write cross-compilable macros that use `IsCollection` by sharing the core logic:

    ```scala
    // file: src/main/scala/example/CollectionUtilsLogic.scala - part of IsCollection example
    //> using scala {{ scala.2_13 }} {{ scala.3 }}
    //> using dep com.kubuszok::hearth::{{ hearth_version() }}
    package example

    import hearth.MacroCommons
    import hearth.std.StdExtensions

    // Shared macro logic
    trait CollectionUtilsLogic { this: MacroCommons & StdExtensions =>

      // Load standard extensions to enable IsCollection support:
      // this is necessary to teach the macro what implementations it can use!
      Environment.loadStandardExtensions() match {
        case ExtensionLoadingResult.LoaderFailed(error) =>
          Environment.reportErrorAndAbort("Failed to resolve extensions: " + error.toString)
        case ExtensionLoadingResult.SomeFailed(extensions, errors) =>
          Environment.reportErrorAndAbort(
            "Failed to load standard extensions: " + errors.toNonEmptyVector.map(_._2).mkString("\n")
          )
        case _ =>
      }

      private val StringType = Type.of[String]

      def processCollection[A: Type](collection: Expr[A]): Expr[String] = Type[A] match {
        case IsCollection(isCollection) =>
          // This import let us refer to the collection's Item and puts implicit Type[Item] in the scope.
          import isCollection.Underlying as Item
          // This import decribes the Type of result of isCollection.value.factory
          import isCollection.value.CtorResult
          
          implicit val String: Type[String] = StringType

          // Iterate over the collection:
          val iterationExample = Expr.quote {
            val iterable = Expr.splice(isCollection.value.asIterable(collection))
            iterable.map { item =>
              "\"" + item + "\""
            }.mkString("Iteration: ", ", ", "")
          }

          // Build a new collection from a single Item:
          val buildingExample = if (Item <:< String) {
            isCollection.value.build match {
              case CtorLikeOf.PlainValue(ctor, _) =>
                Expr.quote {
                  val builder = Expr.splice(isCollection.value.factory).newBuilder
                  val item = Expr.splice(Expr("newItem").upcast[Item])
                  builder.addOne(item)
                  val result = Expr.splice(ctor(Expr.quote(builder)))
                  "Built: " + result.toString
                }
              case _ =>
                Expr("<cannot build - smart constructor not handled in this example>")
            }
          }
          else Expr("<cannot build - not a collection of String>")

          Expr.quote {
            Expr.splice(iterationExample) + ", " + Expr.splice(buildingExample)
          }
        case _ =>
          Expr(s"Not a collection: ${Type[A].plainPrint}")
      }
    }
    ```

    Then we create platform-specific adapters:

    ```scala
    // file: src/main/scala-2/example/CollectionUtils.scala - part of IsCollection example
    //> using target.scala {{ scala.2_13 }}
    //> using options -Xsource:3
    package example

    import scala.language.experimental.macros
    import scala.reflect.macros.blackbox

    import hearth.MacroCommonsScala2

    // Scala 2 adapter
    class CollectionUtils(val c: blackbox.Context) extends MacroCommonsScala2 with CollectionUtilsLogic {

      def processCollectionImpl[A: c.WeakTypeTag](collection: c.Expr[A]): c.Expr[String] =
        processCollection(collection)
    }

    object CollectionUtils {
      def processCollection[A](collection: A): String = macro CollectionUtils.processCollectionImpl[A]
    }
    ```

    ```scala
    // file: src/main/scala-3/example/CollectionUtils.scala - part of IsCollection example
    //> using target.scala {{ scala.3 }}
    //> using plugin com.kubuszok::hearth-cross-quotes::{{ hearth_version() }}
    package example

    import scala.quoted.*

    import hearth.MacroCommonsScala3

    // Scala 3 adapter
    class CollectionUtils(q: Quotes) extends MacroCommonsScala3(using q) with CollectionUtilsLogic

    object CollectionUtils {

      inline def processCollection[A](inline collection: A): String = ${ processCollectionImpl[A]('{ collection }) }
      private def processCollectionImpl[A: Type](collection: Expr[A])(using q: Quotes): Expr[String] =
        new CollectionUtils(q).processCollection(collection)
    }
    ```

    And finally we can expand the macro in our tests.

    ```scala
    // file: src/test/scala/example/CollectionUtilsSpec.scala - part of IsCollection example
    //> using test.dep org.scalameta::munit::{{ libraries.munit }}
    package example

    final class CollectionUtilsSpec extends munit.FunSuite {

      test("processCollection works with List") {
        val result = CollectionUtils.processCollection(List("one", "two", "three"))
        assert(result.contains("""Iteration: "one", "two", "three""""))
        assert(result.contains("Built: List(newItem)"))
      }

      test("processCollection works with Array") {
        val result = CollectionUtils.processCollection(Array("a", "b", "c"))
        assert(result.contains("""Iteration: "a", "b", "c""""))
      }

      test("processCollection works with Set") {
        val result = CollectionUtils.processCollection(Set("x", "y"))
        assert(result.contains("Iteration"))
        assert(result.contains("Built"))
      }

      test("processCollection handles non-collections") {
        val result = CollectionUtils.processCollection("not a collection")
        assert(result.startsWith("Not a collection:"))
      }
    }
    ```

In these examples:

 1. **Loading extensions**: We call `Environment.loadStandardExtensions()` to load all standard macro extensions, which registers providers for `IsCollection` support.
 2. **Pattern matching**: We use `IsCollection.unapply` to check if a type is a collection. If it matches, we get an `IsCollection[A]` instance.
 3. **Accessing item type**: We import `isCollection.Underlying as Item` to get the existential item type, and `isCollection.value.CtorResult` to get the result type that can be built.
 4. **Iteration**: We use `isCollection.value.asIterable(collection)` to convert the collection to an `Iterable[Item]` that we can iterate over.
 5. **Building**: We use `isCollection.value.factory` to get a `Factory[Item, CtorResult]`, create a builder, add items, and use `isCollection.value.build` to construct the final collection.

This API works seamlessly with Scala collections, `Array`s, `IArray`s (Scala 3), and Java collections, all through the same interface!

### `IsOption` macro extension

`IsOption` allows you to check if a provided `Type[A]` can be considered an option type, that is:

 - it can represent an empty value
 - it can represent a value of some item type
 - it can be folded over (handling both empty and non-empty cases)

It is not only more convenient than manually pattern matching on `Option`, but also out-of-the-box supports types that are not Scala options, but we want to treat them as such:

 - `scala.Option`
 - `java.util.Optional` (on JVM)

with a possibility to support even more types, with the same API, just by adding a macro extension implementation to the class-path!

How could we use this API?

!!! example "Cross-compilable `IsOption`"

    We can write cross-compilable macros that use `IsOption` by sharing the core logic:

    ```scala
    // file: src/main/scala/example/OptionUtilsLogic.scala - part of IsOption example
    //> using scala {{ scala.2_13 }} {{ scala.3 }}
    //> using dep com.kubuszok::hearth::{{ hearth_version() }}
    package example

    import hearth.MacroCommons
    import hearth.std.StdExtensions

    // Shared macro logic
    trait OptionUtilsLogic { this: MacroCommons & StdExtensions =>

      // Load standard extensions to enable IsOption support:
      // this is necessary to teach the macro what implementations it can use!
      Environment.loadStandardExtensions() match {
        case ExtensionLoadingResult.LoaderFailed(error) =>
          Environment.reportErrorAndAbort("Failed to resolve extensions: " + error.toString)
        case ExtensionLoadingResult.SomeFailed(extensions, errors) =>
          Environment.reportErrorAndAbort(
            "Failed to load standard extensions: " + errors.toNonEmptyVector.map(_._2).mkString("\n")
          )
        case _ =>
      }

      private val StringType = Type.of[String]

      def processOption[A: Type](option: Expr[A]): Expr[String] = Type[A] match {
        case IsOption(isOption) =>
          // This import let us refer to the option's Item and puts implicit Type[Item] in the scope.
          import isOption.Underlying as Item
          
          implicit val String: Type[String] = StringType

          // Fold over the option (handling both empty and non-empty cases):
          val foldingExample = if (Item <:< StringType) {
            isOption.value.fold(option)(
              onEmpty = Expr("empty"),
              onSome = (item: Expr[Item]) =>
                Expr.quote {
                  val i = Expr.splice(item)
                  "some: " + i.toString
                }
            )
          }
          else Expr("<not an option of string>")

          // Get value or default:
          val getOrElseExample = if (Item <:< StringType) {
            Expr.quote {
              val opt = Expr.splice(option)
              Expr.splice(
                isOption.value.getOrElse(Expr.quote(opt))(
                  Expr("default").upcast[Item]
                )
              ).toString
            }
          }
          else Expr("<not an option of string>")

          // Build new options:
          val buildingExample = if (Item <:< StringType) {
            Expr.quote {
              val some = Expr.splice(isOption.value.of(Expr("test").upcast[Item]))
              val empty = Expr.splice(isOption.value.empty)
              "Built some: " + some.toString + ", empty: " + empty.toString
            }
          }
          else Expr("<not an option of string>")

          Expr.quote {
            Expr.splice(foldingExample) + ", " + Expr.splice(getOrElseExample) + ", " + Expr.splice(buildingExample)
          }
        case _ =>
          Expr(s"Not an option: ${Type[A].plainPrint}")
      }
    }
    ```

    Then we create platform-specific adapters:

    ```scala
    // file: src/main/scala-2/example/OptionUtils.scala - part of IsOption example
    //> using target.scala {{ scala.2_13 }}
    //> using options -Xsource:3
    package example

    import scala.language.experimental.macros
    import scala.reflect.macros.blackbox

    import hearth.MacroCommonsScala2

    // Scala 2 adapter
    class OptionUtils(val c: blackbox.Context) extends MacroCommonsScala2 with OptionUtilsLogic {

      def processOptionImpl[A: c.WeakTypeTag](option: c.Expr[A]): c.Expr[String] =
        processOption(option)
    }

    object OptionUtils {
      def processOption[A](option: A): String = macro OptionUtils.processOptionImpl[A]
    }
    ```

    ```scala
    // file: src/main/scala-3/example/OptionUtils.scala - part of IsOption example
    //> using target.scala {{ scala.3 }}
    //> using plugin com.kubuszok::hearth-cross-quotes::{{ hearth_version() }}
    package example

    import scala.quoted.*

    import hearth.MacroCommonsScala3

    // Scala 3 adapter
    class OptionUtils(q: Quotes) extends MacroCommonsScala3(using q) with OptionUtilsLogic

    object OptionUtils {

      inline def processOption[A](inline option: A): String = ${ processOptionImpl[A]('{ option }) }
      private def processOptionImpl[A: Type](option: Expr[A])(using q: Quotes): Expr[String] =
        new OptionUtils(q).processOption(option)
    }
    ```

    And finally we can expand the macro in our tests.

    ```scala
    // file: src/test/scala/example/OptionUtilsSpec.scala - part of IsOption example
    //> using test.dep org.scalameta::munit::{{ libraries.munit }}
    package example

    final class OptionUtilsSpec extends munit.FunSuite {

      test("processOption works with Some") {
        val result = OptionUtils.processOption(Option("value"))
        assert(result.contains("some: value"))
        assert(result.contains("value"))
        assert(result.contains("Built some:"))
      }

      test("processOption works with None") {
        val result = OptionUtils.processOption(Option.empty[String])
        assert(result.contains("empty"))
        assert(result.contains("default"))
        assert(result.contains("Built"))
      }

      test("processOption handles non-options") {
        val result = OptionUtils.processOption("not an option")
        assert(result.startsWith("Not an option:"))
      }
    }
    ```

In these examples:

 1. **Loading extensions**: We call `Environment.loadStandardExtensions()` to load all standard macro extensions, which registers providers for `IsOption` support.
 2. **Pattern matching**: We use `IsOption.unapply` to check if a type is an option. If it matches, we get an `IsOption[A]` instance.
 3. **Accessing item type**: We import `isOption.Underlying as Item` to get the existential item type.
 4. **Folding**: We use `isOption.value.fold(option)(onEmpty, onSome)` to handle both empty and non-empty cases.
 5. **Getting with default**: We use `isOption.value.getOrElse(option)(default)` to get the value or a default.
 6. **Building**: We use `isOption.value.of(item)` to create a non-empty option and `isOption.value.empty` to create an empty option.

This API works seamlessly with Scala `Option` and Java `Optional` (on JVM), all through the same interface!

### `IsEither` macro extension

`IsEither` allows you to check if a provided `Type[A]` can be considered an either type, that is:

 - it can represent a left value of some type
 - it can represent a right value of some type
 - it can be folded over (handling both left and right cases)

It is not only more convenient than manually pattern matching on `Either`, but also out-of-the-box supports types that are not Scala eithers, but we want to treat them as such:

 - `scala.Either`
 - `scala.util.Try` (treated as `Either[Throwable, A]`)

with a possibility to support even more types, with the same API, just by adding a macro extension implementation to the class-path!

How could we use this API?

!!! example "Cross-compilable `IsEither`"

    We can write cross-compilable macros that use `IsEither` by sharing the core logic:

    ```scala
    // file: src/main/scala/example/EitherUtilsLogic.scala - part of IsEither example
    //> using scala {{ scala.2_13 }} {{ scala.3 }}
    //> using dep com.kubuszok::hearth::{{ hearth_version() }}
    package example

    import hearth.MacroCommons
    import hearth.std.StdExtensions

    // Shared macro logic
    trait EitherUtilsLogic { this: MacroCommons & StdExtensions =>

      // Load standard extensions to enable IsEither support:
      // this is necessary to teach the macro what implementations it can use!
      Environment.loadStandardExtensions() match {
        case ExtensionLoadingResult.LoaderFailed(error) =>
          Environment.reportErrorAndAbort("Failed to resolve extensions: " + error.toString)
        case ExtensionLoadingResult.SomeFailed(extensions, errors) =>
          Environment.reportErrorAndAbort(
            "Failed to load standard extensions: " + errors.toNonEmptyVector.map(_._2).mkString("\n")
          )
        case _ =>
      }

      private val StringType = Type.of[String]
      private val IntType = Type.of[Int]

      def processEither[A: Type](either: Expr[A]): Expr[String] = Type[A] match {
        case IsEither(isEither) =>
          // This import let us refer to the either's LeftValue and RightValue types.
          import isEither.{LeftValue, RightValue}
          
          implicit val String: Type[String] = StringType
          implicit val Int: Type[Int] = IntType

          // Fold over the either (handling both left and right cases):
          val foldingExample = Expr.quote {
            val e = Expr.splice(either)
            Expr.splice(
              isEither.value.fold[String](Expr.quote(e))(
                onLeft = (left: Expr[LeftValue]) =>
                  Expr.quote {
                    val l = Expr.splice(left)
                    "left: " + l.toString
                  },
                onRight = (right: Expr[RightValue]) =>
                  Expr.quote {
                    val r = Expr.splice(right)
                    "right: " + r.toString
                  }
              )
            )
          }

          // Get right value or default:
          val getOrElseExample = if (RightValue <:< StringType) {
            Expr.quote {
              val e = Expr.splice(either)
              Expr.splice(
                isEither.value.getOrElse(Expr.quote(e))(
                  Expr("default").upcast[RightValue]
                )
              ).toString
            }
          }
          else Expr("<not an either with string right>")

          // Build new eithers:
          val buildingExample = if (LeftValue <:< StringType && RightValue <:< IntType) {
            Expr.quote {
              val left = Expr.splice(isEither.value.left(Expr("error").upcast[LeftValue]))
              val right = Expr.splice(isEither.value.right(Expr(42).upcast[RightValue]))
              "Built left: " + left.toString + ", right: " + right.toString
            }
          }
          else Expr("<not an either of string and int>")

          Expr.quote {
            Expr.splice(foldingExample) + ", " + Expr.splice(getOrElseExample) + ", " + Expr.splice(buildingExample)
          }
        case _ =>
          Expr(s"Not an either: ${Type[A].plainPrint}")
      }
    }
    ```

    Then we create platform-specific adapters:

    ```scala
    // file: src/main/scala-2/example/EitherUtils.scala - part of IsEither example
    //> using target.scala {{ scala.2_13 }}
    //> using options -Xsource:3
    package example

    import scala.language.experimental.macros
    import scala.reflect.macros.blackbox

    import hearth.MacroCommonsScala2

    // Scala 2 adapter
    class EitherUtils(val c: blackbox.Context) extends MacroCommonsScala2 with EitherUtilsLogic {

      def processEitherImpl[A: c.WeakTypeTag](either: c.Expr[A]): c.Expr[String] =
        processEither(either)
    }

    object EitherUtils {
      def processEither[A](either: A): String = macro EitherUtils.processEitherImpl[A]
    }
    ```

    ```scala
    // file: src/main/scala-3/example/EitherUtils.scala - part of IsEither example
    //> using target.scala {{ scala.3 }}
    //> using plugin com.kubuszok::hearth-cross-quotes::{{ hearth_version() }}
    package example

    import scala.quoted.*

    import hearth.MacroCommonsScala3

    // Scala 3 adapter
    class EitherUtils(q: Quotes) extends MacroCommonsScala3(using q) with EitherUtilsLogic

    object EitherUtils {

      inline def processEither[A](inline either: A): String = ${ processEitherImpl[A]('{ either }) }
      private def processEitherImpl[A: Type](either: Expr[A])(using q: Quotes): Expr[String] =
        new EitherUtils(q).processEither(either)
    }
    ```

    And finally we can expand the macro in our tests.

    ```scala
    // file: src/test/scala/example/EitherUtilsSpec.scala - part of IsEither example
    //> using test.dep org.scalameta::munit::{{ libraries.munit }}
    package example

    final class EitherUtilsSpec extends munit.FunSuite {

      test("processEither works with Left") {
        val result = EitherUtils.processEither(Left("error"): Either[String, Int])
        assert(result.contains("left: error"))
        assert(result.contains("Built"))
      }

      test("processEither works with Right") {
        val result = EitherUtils.processEither(Right(42): Either[String, Int])
        assert(result.contains("right: 42"))
        assert(result.contains("Built"))
      }

      test("processEither works with Try Success") {
        val result = EitherUtils.processEither(scala.util.Success("value"): scala.util.Try[String])
        assert(result.contains("right: value"))
      }

      test("processEither works with Try Failure") {
        val result = EitherUtils.processEither(scala.util.Failure(new Exception("error")): scala.util.Try[String])
        assert(result.contains("left:"))
        assert(result.contains("Exception"))
      }

      test("processEither handles non-eithers") {
        val result = EitherUtils.processEither("not an either")
        assert(result.startsWith("Not an either:"))
      }
    }
    ```

In these examples:

 1. **Loading extensions**: We call `Environment.loadStandardExtensions()` to load all standard macro extensions, which registers providers for `IsEither` support.
 2. **Pattern matching**: We use `IsEither.unapply` to check if a type is an either. If it matches, we get an `IsEither[A]` instance.
 3. **Accessing left and right types**: We import `isEither.{LeftValue, RightValue}` to get the existential left and right types.
 4. **Folding**: We use `isEither.value.fold(either)(onLeft, onRight)` to handle both left and right cases.
 5. **Getting with default**: We use `isEither.value.getOrElse(either)(default)` to get the right value or a default.
 6. **Building**: We use `isEither.value.left(leftValue)` to create a left value and `isEither.value.right(rightValue)` to create a right value.

This API works seamlessly with Scala `Either` and `Try`, all through the same interface!

### `IsValueType` macro extension

`IsValueType` allows you to check if a provided `Type[A]` can be considered a value type, that is:

 - it wraps an inner type
 - it can be unwrapped to its inner type
 - it can be wrapped from its inner type

It is not only more convenient than manually handling value types, but also out-of-the-box supports types that are value types:

 - `AnyVal` types (with a single constructor argument)
 - Java boxed types (on JVM): `java.lang.Integer`, `java.lang.Boolean`, `java.lang.Byte`, `java.lang.Character`, `java.lang.Short`, `java.lang.Long`, `java.lang.Float`, `java.lang.Double`

with a possibility to support even more types, with the same API, just by adding a macro extension implementation to the class-path!

How could we use this API?

!!! example "Cross-compilable `IsValueType`"

    We can write cross-compilable macros that use `IsValueType` by sharing the core logic:

    ```scala
    // file: src/main/scala/example/ValueTypeUtilsLogic.scala - part of IsValueType example
    //> using scala {{ scala.2_13 }} {{ scala.3 }}
    //> using dep com.kubuszok::hearth::{{ hearth_version() }}
    package example

    import hearth.MacroCommons
    import hearth.std.StdExtensions

    // Shared macro logic
    trait ValueTypeUtilsLogic { this: MacroCommons & StdExtensions =>

      // Load standard extensions to enable IsValueType support:
      // this is necessary to teach the macro what implementations it can use!
      Environment.loadStandardExtensions() match {
        case ExtensionLoadingResult.LoaderFailed(error) =>
          Environment.reportErrorAndAbort("Failed to resolve extensions: " + error.toString)
        case ExtensionLoadingResult.SomeFailed(extensions, errors) =>
          Environment.reportErrorAndAbort(
            "Failed to load standard extensions: " + errors.toNonEmptyVector.map(_._2).mkString("\n")
          )
        case _ =>
      }

      private val StringType = Type.of[String]

      def processValueType[A: Type](value: Expr[A]): Expr[String] = Type[A] match {
        case IsValueType(isValueType) =>
          // This import let us refer to the value type's Inner and puts implicit Type[Inner] in the scope.
          import isValueType.Underlying as Inner
          
          implicit val String: Type[String] = StringType

          // Unwrap the value type:
          val unwrappingExample = Expr.quote {
            val outer = Expr.splice(value)
            val inner = Expr.splice(isValueType.value.unwrap(Expr.quote(outer)))
            "Unwrapped: " + inner.toString
          }

          // Wrap the inner type:
          val wrappingExample = if (Inner <:< StringType) {
            isValueType.value.wrap match {
              case CtorLikeOf.PlainValue(ctor, _) =>
                Expr.quote {
                  val inner = Expr.splice(Expr("test").upcast[Inner])
                  val outer = Expr.splice(ctor(Expr.quote(inner)))
                  "Wrapped: " + outer.toString
                }
              case _ =>
                Expr("<cannot wrap - smart constructor not handled in this example>")
            }
          }
          else Expr("<not a value type of string>")

          Expr.quote {
            Expr.splice(unwrappingExample) + ", " + Expr.splice(wrappingExample)
          }
        case _ =>
          Expr(s"Not a value type: ${Type[A].plainPrint}")
      }
    }
    ```

    Then we create platform-specific adapters:

    ```scala
    // file: src/main/scala-2/example/ValueTypeUtils.scala - part of IsValueType example
    //> using target.scala {{ scala.2_13 }}
    //> using options -Xsource:3
    package example

    import scala.language.experimental.macros
    import scala.reflect.macros.blackbox

    import hearth.MacroCommonsScala2

    // Scala 2 adapter
    class ValueTypeUtils(val c: blackbox.Context) extends MacroCommonsScala2 with ValueTypeUtilsLogic {

      def processValueTypeImpl[A: c.WeakTypeTag](value: c.Expr[A]): c.Expr[String] =
        processValueType(value)
    }

    object ValueTypeUtils {
      def processValueType[A](value: A): String = macro ValueTypeUtils.processValueTypeImpl[A]
    }
    ```

    ```scala
    // file: src/main/scala-3/example/ValueTypeUtils.scala - part of IsValueType example
    //> using target.scala {{ scala.3 }}
    //> using plugin com.kubuszok::hearth-cross-quotes::{{ hearth_version() }}
    package example

    import scala.quoted.*

    import hearth.MacroCommonsScala3

    // Scala 3 adapter
    class ValueTypeUtils(q: Quotes) extends MacroCommonsScala3(using q) with ValueTypeUtilsLogic

    object ValueTypeUtils {

      inline def processValueType[A](inline value: A): String = ${ processValueTypeImpl[A]('{ value }) }
      private def processValueTypeImpl[A: Type](value: Expr[A])(using q: Quotes): Expr[String] =
        new ValueTypeUtils(q).processValueType(value)
    }
    ```

    And finally we can expand the macro in our tests.

    ```scala
    // file: src/test/scala/example/ValueTypeUtilsSpec.scala - part of IsValueType example
    //> using test.dep org.scalameta::munit::{{ libraries.munit }}
    package example

    final case class ExampleValueClass(a: String) extends AnyVal

    final class ValueTypeUtilsSpec extends munit.FunSuite {

      test("processValueType works with AnyVal") {
        val result = ValueTypeUtils.processValueType(ExampleValueClass("wrapped value"))
        assert(result.contains("Unwrapped: wrapped value"))
        assert(result.contains("Wrapped: ExampleValueClass(test)"))
      }

      test("processValueType handles non-value-types") {
        val result = ValueTypeUtils.processValueType("not a value type")
        assert(result.startsWith("Not a value type:"))
      }
    }
    ```

In these examples:

 1. **Loading extensions**: We call `Environment.loadStandardExtensions()` to load all standard macro extensions, which registers providers for `IsValueType` support.
 2. **Pattern matching**: We use `IsValueType.unapply` to check if a type is a value type. If it matches, we get an `IsValueType[A]` instance.
 3. **Accessing inner type**: We import `isValueType.Underlying as Inner` to get the existential inner type.
 4. **Unwrapping**: We use `isValueType.value.unwrap(outer)` to convert the outer value type to its inner type.
 5. **Wrapping**: We use `isValueType.value.wrap` to get a smart constructor that can wrap an inner value into the outer value type.

This API works seamlessly with `AnyVal` types, `opaque type`s and Java boxed types (on JVM), all through the same interface!

### Smart Constructors

When building values in macros (like constructing collections from builders or wrapping value types), sometimes the construction can fail. For example, when building a validated type, the constructor might return `Either[String, A]` instead of just `A`.

`CtorLikeOf[Input, Output]` is a type that abstracts over different constructor patterns, allowing macros to handle both direct construction and validation/error cases uniformly.

#### Variants

`CtorLikeOf` comes with several predefined variants:

1. **`PlainValue`** - Direct construction that always succeeds:
   ```scala
   Input => Output
   ```
   This is the most common case, used for building collections from builders or wrapping value types.

2. **`EitherStringOrValue`** - Construction that can fail with a single string error:
   ```scala
   Input => Either[String, Output]
   ```
   Useful for validation that returns a single error message.

3. **`EitherIterableStringOrValue`** - Construction that can fail with multiple string errors:
   ```scala
   Input => Either[Iterable[String], Output]
   ```
   Useful for validation that can accumulate multiple error messages.

4. **`EitherThrowableOrValue`** - Construction that can fail with a throwable:
   ```scala
   Input => Either[Throwable, Output]
   ```
   Useful for construction that might throw exceptions.

5. **`EitherIterableThrowableOrValue`** - Construction that can fail with multiple throwables:
   ```scala
   Input => Either[Iterable[Throwable], Output]
   ```
   Useful for construction that can accumulate multiple exceptions.

#### Usage Pattern

When you receive a `CtorLikeOf[Input, Output]`, you pattern match on it to handle each case:

```scala
val build: CtorLikeOf[Builder[Item, Coll], Coll] = isCollection.value.build

build match {
  case CtorLikeOf.PlainValue(ctor, _) =>
    // Direct construction: ctor(builder) returns Expr[Coll]
    val result = Expr.splice(ctor(Expr.quote(builder)))
    // Use result directly
    
  case CtorLikeOf.EitherStringOrValue(ctor, _) =>
    // Validation: ctor(builder) returns Expr[Either[String, Coll]]
    Expr.quote {
      Expr.splice(ctor(Expr.quote(builder))) match {
        case Left(error) => // handle error
        case Right(value) => // use value
      }
    }
    
  // Handle other variants similarly...
  case _ =>
    Expr("<unhandled smart constructor>")
}
```

#### Why This Design?

This design allows standard extensions to work with both:
- **Simple types** (like collections) that can always be built directly
- **Validated types** (like from validation libraries) that might fail during construction

All while providing a single, uniform interface. This makes it possible to extend support for custom types (like `Validated[E, A]` from Cats) by providing new `CtorLikeOf` implementations, without changing the core API.
