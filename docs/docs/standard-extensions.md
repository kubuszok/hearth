# Standard Extensions

While the [Basic Utilities](basic-utilities.md) try to be reasonably unopinionated, sometimes we can consider
solutions that require us to follow some convention, but in turn give us a lot out of the box. To not mix
these unopinionated core utilities and opinionated solution, we put the latter under `std` package as opt-in
mixins.

## Standard Macro Extensions

Core library allows us to define [macro extensions](basic-utilities.md#macro-extensions):

 - we define some trait, tell it how much it knows about our quoting context
 - then we load it in a macro, so that it would execute some code, e.g. injecting new capabilities into our codebase

But there are no built-in macro extensions in the bare `MacroCommons`, since every such extensio is a design decision.

But we can predefine a set of such extensions as a proposal of usable API that Hearth users can opt-in into using, because they could be immensly useful.

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
          import isCollection.value.PossibleSmartResult
          
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
              case PossibleSmartCtor.PlainValue(ctor) =>
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
        println(result + " is the result")
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
 3. **Accessing item type**: We import `isCollection.Underlying as Item` to get the existential item type, and `isCollection.value.PossibleSmartResult` to get the result type that can be built.
 4. **Iteration**: We use `isCollection.value.asIterable(collection)` to convert the collection to an `Iterable[Item]` that we can iterate over.
 5. **Building**: We use `isCollection.value.factory` to get a `Factory[Item, PossibleSmartResult]`, create a builder, add items, and use `isCollection.value.build` to construct the final collection.

This API works seamlessly with Scala collections, `Array`s, `IArray`s (Scala 3), and Java collections, all through the same interface!

### Smart Constructors

### Requirements

 1. we need to mix-in `hearth.std.StdExtensions` trait
 2. we need to load extensions vis `Environment.loadStandardExtensions()`