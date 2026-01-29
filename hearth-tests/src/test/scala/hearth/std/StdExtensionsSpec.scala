package hearth
package std

import hearth.data.Data

/** Macro implementation is in [[StdExtensionsFixturesImpl]] */
final class StdExtensionsSpec extends MacroSuite {

  group("trait std.StdExtensions") {

    group("class: IsCollection[A], returns preprocessed collection") {
      import StdExtensionsFixtures.testIsCollection

      test("for Scala Iterable") {
        testIsCollection(Iterable("one", "two", "three")) <==> Data.map(
          "iteration" -> Data.list(
            Data("one"),
            Data("two"),
            Data("three")
          ),
          "building" -> Data("List(one)")
        )
      }

      test("for Scala Seq-like collections") {
        testIsCollection(Seq("one", "two", "three")) <==> Data.map(
          "iteration" -> Data.list(
            Data("one"),
            Data("two"),
            Data("three")
          ),
          "building" -> Data("List(one)")
        )
        testIsCollection(IndexedSeq("one", "two", "three")) <==> Data.map(
          "iteration" -> Data.list(
            Data("one"),
            Data("two"),
            Data("three")
          ),
          "building" -> Data("Vector(one)")
        )
        testIsCollection(scala.collection.LinearSeq("one", "two", "three")) <==> Data.map(
          "iteration" -> Data.list(
            Data("one"),
            Data("two"),
            Data("three")
          ),
          "building" -> Data("List(one)")
        )
        testIsCollection(List("one", "two", "three")) <==> Data.map(
          "iteration" -> Data.list(
            Data("one"),
            Data("two"),
            Data("three")
          ),
          "building" -> Data("List(one)")
        )
        testIsCollection(LazyList("one", "two", "three")) <==> Data.map(
          "iteration" -> Data.list(
            Data("one"),
            Data("two"),
            Data("three")
          ),
          "building" -> Data("LazyList(<not computed>)")
        )
        testIsCollection(Vector("one", "two", "three")) <==> Data.map(
          "iteration" -> Data.list(
            Data("one"),
            Data("two"),
            Data("three")
          ),
          "building" -> Data("Vector(one)")
        )
        testIsCollection(scala.collection.immutable.ArraySeq("one", "two", "three")) <==> Data.map(
          "iteration" -> Data.list(
            Data("one"),
            Data("two"),
            Data("three")
          ),
          "building" -> Data("ArraySeq(one)")
        )
        testIsCollection(scala.collection.mutable.ArrayBuffer("one", "two", "three")) <==> Data.map(
          "iteration" -> Data.list(
            Data("one"),
            Data("two"),
            Data("three")
          ),
          "building" -> Data("ArrayBuffer(one)")
        )
        testIsCollection(scala.collection.mutable.ListBuffer("one", "two", "three")) <==> Data.map(
          "iteration" -> Data.list(
            Data("one"),
            Data("two"),
            Data("three")
          ),
          "building" -> Data("ListBuffer(one)")
        )
      }

      test("for Scala Sets") {
        testIsCollection(Set("one", "two", "three")) <==> Data.map(
          "iteration" -> Data.list(
            Data("one"),
            Data("two"),
            Data("three")
          ),
          "building" -> Data("Set(one)")
        )
        testIsCollection(scala.collection.immutable.SortedSet("one", "two", "three")) <==> Data.map(
          "iteration" -> Data.list(
            Data("one"),
            Data("three"),
            Data("two")
          ),
          "building" -> Data("TreeSet(one)")
        )
        testIsCollection(scala.collection.immutable.ListSet("one", "two", "three")) <==> Data.map(
          "iteration" -> Data.list(
            Data("one"),
            Data("two"),
            Data("three")
          ),
          "building" -> Data("ListSet(one)")
        )
        testIsCollection(scala.collection.immutable.HashSet("one", "two", "three")) <==> Data.map(
          "iteration" -> Data.list(
            Data("three"),
            Data("two"),
            Data("one")
          ),
          "building" -> Data("HashSet(one)")
        )
        testIsCollection(scala.collection.immutable.TreeSet("three", "one", "two")) <==> Data.map(
          "iteration" -> Data.list(
            Data("one"),
            Data("three"),
            Data("two")
          ),
          "building" -> Data("TreeSet(one)")
        )
        testIsCollection(scala.collection.mutable.Set("one", "two", "three")) <==> Data.map(
          "iteration" -> Data.list(
            Data("one"),
            Data("two"),
            Data("three")
          ),
          "building" -> Data("HashSet(one)")
        )
        testIsCollection(scala.collection.mutable.HashSet("one", "two", "three")) <==> Data.map(
          "iteration" -> Data.list(
            Data("one"),
            Data("two"),
            Data("three")
          ),
          "building" -> Data("HashSet(one)")
        )
      }

      test("for Scala BitSet") {
        testIsCollection(scala.collection.immutable.BitSet(1, 2, 3)) <==> Data.map(
          "iteration" -> Data.list(
            Data("1"),
            Data("2"),
            Data("3")
          ),
          "building" -> Data("<not a collection of string>")
        )
      }

      test("for Scala Buffers") {
        testIsCollection(scala.collection.mutable.Buffer("one", "two", "three")) <==> Data.map(
          "iteration" -> Data.list(
            Data("one"),
            Data("two"),
            Data("three")
          ),
          "building" -> Data(if (Platform.byHearth.isJs) "WrappedArray(one)" else "ArrayBuffer(one)")
        )
        testIsCollection(scala.collection.mutable.IndexedBuffer("one", "two", "three")) <==> Data.map(
          "iteration" -> Data.list(
            Data("one"),
            Data("two"),
            Data("three")
          ),
          "building" -> Data(if (Platform.byHearth.isJs) "WrappedArray(one)" else "ArrayBuffer(one)")
        )
        testIsCollection(scala.collection.mutable.ArrayBuffer("one", "two", "three")) <==> Data.map(
          "iteration" -> Data.list(
            Data("one"),
            Data("two"),
            Data("three")
          ),
          "building" -> Data("ArrayBuffer(one)")
        )
        testIsCollection(scala.collection.mutable.ListBuffer("one", "two", "three")) <==> Data.map(
          "iteration" -> Data.list(
            Data("one"),
            Data("two"),
            Data("three")
          ),
          "building" -> Data("ListBuffer(one)")
        )
      }

      test("for Scala Queues") {
        testIsCollection(scala.collection.immutable.Queue("one", "two", "three")) <==> Data.map(
          "iteration" -> Data.list(
            Data("one"),
            Data("two"),
            Data("three")
          ),
          "building" -> Data("Queue(one)")
        )
        testIsCollection(scala.collection.mutable.Queue("one", "two", "three")) <==> Data.map(
          "iteration" -> Data.list(
            Data("one"),
            Data("two"),
            Data("three")
          ),
          "building" -> Data("Queue(one)")
        )
      }

      test("for Scala ArrayDeque") {
        testIsCollection(scala.collection.mutable.ArrayDeque("one", "two", "three")) <==> Data.map(
          "iteration" -> Data.list(
            Data("one"),
            Data("two"),
            Data("three")
          ),
          "building" -> Data("ArrayDeque(one)")
        )
      }

      test("for Scala Stack") {
        testIsCollection(scala.collection.mutable.Stack("one", "two", "three")) <==> Data.map(
          "iteration" -> Data.list(
            Data("one"),
            Data("two"),
            Data("three")
          ),
          "building" -> Data("Stack(one)")
        )
      }

      test("for Scala Maps") {
        testIsCollection(Map(1 -> "one", 2 -> "two")) <==> Data.map(
          "iteration" -> Data.list(
            Data.map(
              "key" -> Data("1"),
              "value" -> Data("one")
            ),
            Data.map(
              "key" -> Data("2"),
              "value" -> Data("two")
            )
          ),
          "building" -> Data("Map(1 -> one)")
        )
        testIsCollection(scala.collection.immutable.SortedMap(1 -> "one", 2 -> "two")) <==> Data.map(
          "iteration" -> Data.list(
            Data.map(
              "key" -> Data("1"),
              "value" -> Data("one")
            ),
            Data.map(
              "key" -> Data("2"),
              "value" -> Data("two")
            )
          ),
          "building" -> Data("TreeMap(1 -> one)")
        )
        testIsCollection(scala.collection.immutable.ListMap(1 -> "one", 2 -> "two")) <==> Data.map(
          "iteration" -> Data.list(
            Data.map(
              "key" -> Data("1"),
              "value" -> Data("one")
            ),
            Data.map(
              "key" -> Data("2"),
              "value" -> Data("two")
            )
          ),
          "building" -> Data("ListMap(1 -> one)")
        )
        testIsCollection(scala.collection.immutable.VectorMap(1 -> "one", 2 -> "two")) <==> Data.map(
          "iteration" -> Data.list(
            Data.map(
              "key" -> Data("1"),
              "value" -> Data("one")
            ),
            Data.map(
              "key" -> Data("2"),
              "value" -> Data("two")
            )
          ),
          "building" -> Data("VectorMap(1 -> one)")
        )
        testIsCollection(scala.collection.immutable.HashMap(1 -> "one", 2 -> "two")) <==> Data.map(
          "iteration" -> Data.list(
            Data.map(
              "key" -> Data("1"),
              "value" -> Data("one")
            ),
            Data.map(
              "key" -> Data("2"),
              "value" -> Data("two")
            )
          ),
          "building" -> Data("HashMap(1 -> one)")
        )
        testIsCollection(scala.collection.immutable.TreeMap(2 -> "two", 1 -> "one")) <==> Data.map(
          "iteration" -> Data.list(
            Data.map(
              "key" -> Data("1"),
              "value" -> Data("one")
            ),
            Data.map(
              "key" -> Data("2"),
              "value" -> Data("two")
            )
          ),
          "building" -> Data("TreeMap(1 -> one)")
        )
        testIsCollection(scala.collection.mutable.Map(1 -> "one", 2 -> "two")) <==> Data.map(
          "iteration" -> Data.list(
            Data.map(
              "key" -> Data("1"),
              "value" -> Data("one")
            ),
            Data.map(
              "key" -> Data("2"),
              "value" -> Data("two")
            )
          ),
          "building" -> Data("HashMap(1 -> one)")
        )
        testIsCollection(scala.collection.mutable.HashMap(1 -> "one", 2 -> "two")) <==> Data.map(
          "iteration" -> Data.list(
            Data.map(
              "key" -> Data("1"),
              "value" -> Data("one")
            ),
            Data.map(
              "key" -> Data("2"),
              "value" -> Data("two")
            )
          ),
          "building" -> Data("HashMap(1 -> one)")
        )
        testIsCollection(scala.collection.mutable.LinkedHashMap(1 -> "one", 2 -> "two")) <==> Data.map(
          "iteration" -> Data.list(
            Data.map(
              "key" -> Data("1"),
              "value" -> Data("one")
            ),
            Data.map(
              "key" -> Data("2"),
              "value" -> Data("two")
            )
          ),
          "building" -> Data("LinkedHashMap(1 -> one)")
        )
      }

      test("for Arrays") {
        testIsCollection(Array[Short](1, 2, 3)) <==> Data.map(
          "building" -> Data("<not a collection of string>"),
          "iteration" -> Data.list(
            Data("1"),
            Data("2"),
            Data("3")
          )
        )
        testIsCollection(Array[Int](1, 2, 3)) <==> Data.map(
          "building" -> Data("<not a collection of string>"),
          "iteration" -> Data.list(
            Data("1"),
            Data("2"),
            Data("3")
          )
        )
        testIsCollection(Array[Long](1, 2, 3)) <==> Data.map(
          "building" -> Data("<not a collection of string>"),
          "iteration" -> Data.list(
            Data("1"),
            Data("2"),
            Data("3")
          )
        )
        testIsCollection(Array[Float](1, 2, 3)) <==> Data.map(
          "building" -> Data("<not a collection of string>"),
          "iteration" -> Data.list(
            Data(if (Platform.byHearth.isJs) "1" else "1.0"),
            Data(if (Platform.byHearth.isJs) "2" else "2.0"),
            Data(if (Platform.byHearth.isJs) "3" else "3.0")
          )
        )
        testIsCollection(Array[Double](1, 2, 3)) <==> Data.map(
          "building" -> Data("<not a collection of string>"),
          "iteration" -> Data.list(
            Data(if (Platform.byHearth.isJs) "1" else "1.0"),
            Data(if (Platform.byHearth.isJs) "2" else "2.0"),
            Data(if (Platform.byHearth.isJs) "3" else "3.0")
          )
        )
        testIsCollection(Array[Char]('1', '2', '3')) <==> Data.map(
          "building" -> Data("<not a collection of string>"),
          "iteration" -> Data.list(
            Data("1"),
            Data("2"),
            Data("3")
          )
        )
        testIsCollection(Array[Boolean](true, false, true)) <==> Data.map(
          "building" -> Data("<not a collection of string>"),
          "iteration" -> Data.list(
            Data("true"),
            Data("false"),
            Data("true")
          )
        )
        testIsCollection(Array[Byte](1, 2, 3)) <==> Data.map(
          "building" -> Data("<not a collection of string>"),
          "iteration" -> Data.list(
            Data("1"),
            Data("2"),
            Data("3")
          )
        )
        testIsCollection(Array[Unit]((), (), ())) <==> Data.map(
          "building" -> Data("<not a collection of string>"),
          "iteration" -> Data.list(
            Data(if (Platform.byHearth.isJs) "undefined" else "()"),
            Data(if (Platform.byHearth.isJs) "undefined" else "()"),
            Data(if (Platform.byHearth.isJs) "undefined" else "()")
          )
        )
        testIsCollection(Array[AnyRef]("one", "two", "three")) <==> Data.map(
          "building" -> Data("<not a collection of string>"),
          "iteration" -> Data.list(
            Data("one"),
            Data("two"),
            Data("three")
          )
        )
        testIsCollection(Array("one", "two", "three")) <==> Data.map(
          "building" -> Data(
            if (Platform.byHearth.isNative) "scala.scalanative.runtime.ObjectArray" else "[Ljava.lang.String"
          ),
          "iteration" -> Data.list(
            Data("one"),
            Data("two"),
            Data("three")
          )
        )
      }
    }

    group("class: IsOption[A], returns preprocessed option") {
      import StdExtensionsFixtures.testIsOption

      test("for Scala Option") {
        testIsOption(Option("value")) <==> Data.map(
          "folding" -> Data("some: value"),
          "getOrElse" -> Data("value"),
          "building" -> Data.map(
            "some" -> Data("Some(test)"),
            "empty" -> Data("None")
          )
        )
        testIsOption(Option.empty[String]) <==> Data.map(
          "folding" -> Data("empty"),
          "getOrElse" -> Data("default"),
          "building" -> Data.map(
            "some" -> Data("Some(test)"),
            "empty" -> Data("None")
          )
        )
        // Do not have both Some and None, without upcasting they are missing something.
        testIsOption(Some("value")) <==> Data("<no option>")
        testIsOption(None) <==> Data("<no option>")
      }
    }

    group("class: IsEither[A], returns preprocessed either") {
      import StdExtensionsFixtures.testIsEither

      test("for Scala Either") {
        testIsEither(Left("error"): Either[String, Int]) <==> Data.map(
          "folding" -> Data("left: error"),
          "getOrElse" -> Data("<not an either with string right>"),
          "building" -> Data.map(
            "left" -> Data("Left(error)"),
            "right" -> Data("Right(42)")
          )
        )
        testIsEither(Right(42): Either[String, Int]) <==> Data.map(
          "folding" -> Data("right: 42"),
          "getOrElse" -> Data("<not an either with string right>"),
          "building" -> Data.map(
            "left" -> Data("Left(error)"),
            "right" -> Data("Right(42)")
          )
        )

        testIsEither(Left("error"): Either[String, String]) <==> Data.map(
          "folding" -> Data("left: error"),
          "getOrElse" -> Data("default"),
          "building" -> Data.map(
            "left" -> Data("<not an either of string and int>"),
            "right" -> Data("<not an either of string and int>")
          )
        )
        testIsEither(Right("value"): Either[String, String]) <==> Data.map(
          "folding" -> Data("right: value"),
          "getOrElse" -> Data("value"),
          "building" -> Data.map(
            "left" -> Data("<not an either of string and int>"),
            "right" -> Data("<not an either of string and int>")
          )
        )
      }

      test("for Scala Try") {
        testIsEither(scala.util.Success("value"): scala.util.Try[String]) <==> Data.map(
          "folding" -> Data("right: value"),
          "getOrElse" -> Data("value"),
          "building" -> Data.map(
            "left" -> Data("<not an either of string and int>"),
            "right" -> Data("<not an either of string and int>")
          )
        )
        testIsEither(scala.util.Failure(new Exception("error")): scala.util.Try[String]) <==> Data.map(
          "folding" -> Data("left: java.lang.Exception: error"),
          "getOrElse" -> Data("default"),
          "building" -> Data.map(
            "left" -> Data("<not an either of string and int>"),
            "right" -> Data("<not an either of string and int>")
          )
        )
      }
    }

    group("class: IsValueType[A], returns preprocessed value type") {
      import StdExtensionsFixtures.testIsValueType
      import hearth.examples.ExampleValueClass

      test("for ExampleValueClass") {
        testIsValueType(ExampleValueClass(42)) <==> Data.map(
          "unwrapping" -> Data("unwrapped: 42"),
          "wrapping" -> Data("<not a value type of string>")
        )
      }
    }

    group("class: CtorLikes[A], smart constructors discovery") {
      import StdExtensionsFixtures.testCtorLikes
      import hearth.examples.*

      // Helper to check if a Data contains a ctor with the given type and method name
      def containsCtor(data: Data, ctorType: String, methodName: String): Boolean =
        data.get("ctors").flatMap(_.asList).exists { ctors =>
          ctors.exists { ctor =>
            ctor.get("type").contains(Data(ctorType)) &&
            ctor.get("method").contains(Data(methodName))
          }
        }

      test("ValidatedInt (AnyVal with EitherStringOrValue smart constructor)") {
        val result = testCtorLikes[ValidatedInt]
        assert(
          containsCtor(result, "EitherStringOrValue", "parse"),
          s"Expected EitherStringOrValue ctor with 'parse', got: ${result.render}"
        )
      }

      test("PositiveInt (class with EitherStringOrValue smart constructor)") {
        val result = testCtorLikes[PositiveInt]
        assert(
          containsCtor(result, "EitherStringOrValue", "apply"),
          s"Expected EitherStringOrValue ctor with 'apply', got: ${result.render}"
        )
      }

      test("Email (class with multiple smart constructors)") {
        val result = testCtorLikes[Email]
        assert(
          containsCtor(result, "EitherStringOrValue", "fromString"),
          s"Expected EitherStringOrValue ctor with 'fromString', got: ${result.render}"
        )
        assert(
          containsCtor(result, "PlainValue", "unsafeFromString"),
          s"Expected PlainValue ctor with 'unsafeFromString', got: ${result.render}"
        )
      }

      test("Username (case class with EitherIterableStringOrValue smart constructor)") {
        val result = testCtorLikes[Username]
        assert(
          containsCtor(result, "EitherIterableStringOrValue", "validate"),
          s"Expected EitherIterableStringOrValue ctor with 'validate', got: ${result.render}"
        )
      }
    }
  }
}
