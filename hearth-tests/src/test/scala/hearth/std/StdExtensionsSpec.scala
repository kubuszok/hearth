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

      test("for String") {
        testIsCollection("abc") <==> Data.map(
          "iteration" -> Data.list(
            Data("a"),
            Data("b"),
            Data("c")
          ),
          "building" -> Data("<not a collection of string>")
        )
        testIsCollection("") <==> Data.map(
          "iteration" -> Data.list(),
          "building" -> Data("<not a collection of string>")
        )
      }

      test("for Scala Option") {
        testIsCollection(Option("value")) <==> Data.map(
          "iteration" -> Data.list(
            Data("value")
          ),
          "building" -> Data("Some(one)")
        )
        testIsCollection(Option.empty[String]) <==> Data.map(
          "iteration" -> Data.list(),
          "building" -> Data("Some(one)")
        )
      }

      test("for Scala Iterator") {
        testIsCollection(Iterator("one", "two", "three")) <==> Data.map(
          "iteration" -> Data.list(
            Data("one"),
            Data("two"),
            Data("three")
          ),
          "building" -> Data("<iterator>")
        )
        testIsCollection(Iterator.empty[String]) <==> Data.map(
          "iteration" -> Data.list(),
          "building" -> Data("<iterator>")
        )
      }
    }

    group("class: IsCollectionOf[A, Item].foreach, iterates via foreach") {
      import StdExtensionsFixtures.testIsCollectionForeach

      test("for Scala List") {
        testIsCollectionForeach(List("one", "two", "three")) <==> Data.list(
          Data("one"),
          Data("two"),
          Data("three")
        )
      }

      test("for Scala Vector") {
        testIsCollectionForeach(Vector("one", "two", "three")) <==> Data.list(
          Data("one"),
          Data("two"),
          Data("three")
        )
      }

      test("for Scala Set") {
        testIsCollectionForeach(scala.collection.immutable.ListSet("one", "two", "three")) <==> Data.list(
          Data("one"),
          Data("two"),
          Data("three")
        )
      }

      test("for Scala Map") {
        testIsCollectionForeach(scala.collection.immutable.ListMap(1 -> "one", 2 -> "two")) <==> Data.list(
          Data.map("key" -> Data("1"), "value" -> Data("one")),
          Data.map("key" -> Data("2"), "value" -> Data("two"))
        )
      }

      test("for Array") {
        testIsCollectionForeach(Array("one", "two", "three")) <==> Data.list(
          Data("one"),
          Data("two"),
          Data("three")
        )
      }

      test("for Array[Int]") {
        testIsCollectionForeach(Array(1, 2, 3)) <==> Data.list(
          Data("1"),
          Data("2"),
          Data("3")
        )
      }

      test("for String") {
        testIsCollectionForeach("abc") <==> Data.list(
          Data("a"),
          Data("b"),
          Data("c")
        )
      }

      test("for Scala Option") {
        testIsCollectionForeach(Option("value")) <==> Data.list(Data("value"))
        testIsCollectionForeach(Option.empty[String]) <==> Data.list()
      }

      test("for Scala Iterator") {
        testIsCollectionForeach(Iterator("one", "two", "three")) <==> Data.list(
          Data("one"),
          Data("two"),
          Data("three")
        )
        testIsCollectionForeach(Iterator.empty[String]) <==> Data.list()
      }

      test("for empty List") {
        testIsCollectionForeach(List.empty[String]) <==> Data.list()
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

    group("ProviderResult via parse") {
      import StdExtensionsFixtures.testParse

      test("for List[String], IsCollection is matched") {
        val result = testParse[List[String]]
        assert(result.get("IsCollection").contains(Data("matched")))
      }

      test("for Option[String], IsOption is matched") {
        val result = testParse[Option[String]]
        assert(result.get("IsOption").contains(Data("matched")))
      }

      test("for Either[String, Int], IsEither is matched") {
        val result = testParse[Either[String, Int]]
        assert(result.get("IsEither").contains(Data("matched")))
      }

      test("for Int, all extractors return skipped") {
        val result = testParse[Int]
        assert(
          !result.get("IsCollection").contains(Data("matched")),
          s"IsCollection should be skipped for Int: ${result.render}"
        )
        assert(
          !result.get("IsOption").contains(Data("matched")),
          s"IsOption should be skipped for Int: ${result.render}"
        )
        assert(
          !result.get("IsEither").contains(Data("matched")),
          s"IsEither should be skipped for Int: ${result.render}"
        )
      }

      test("skipped results contain provider count") {
        val result = testParse[Int]
        val isCollection = result.get("IsCollection")
        assert(isCollection.flatMap(_.get("status")).contains(Data("skipped")))
        assert(
          isCollection.flatMap(_.get("count")).exists(c => c != Data(0)),
          s"Expected non-zero provider count in skipped result: ${result.render}"
        )
      }
    }

    group("lastUnapplyFailure tracking") {
      import StdExtensionsFixtures.testLastUnapplyFailure

      test("is set after failed unapply") {
        val result = testLastUnapplyFailure[Int]
        assert(result.get("status").contains(Data("skipped")))
        assert(result.get("hasFailure").contains(Data(true)))
      }

      test("is cleared after successful unapply") {
        val result = testLastUnapplyFailure[List[String]]
        assert(result.get("status").contains(Data("matched")))
        assert(result.get("hasFailure").contains(Data(false)))
      }
    }
    group("class: IsCollection[A], recognised types and their item/key/value types") {
      import StdExtensionsFixtures.testCollectionTypes
      import hearth.examples.stdedge.*

      val stringList = Data.map(
        "item" -> Data("java.lang.String"),
        "ctorResult" -> Data("scala.collection.immutable.List[java.lang.String]"),
        "build" -> Data("PlainValue"),
        "asMap" -> Data("<not a map>"),
        "IsMap" -> Data("<not a map>")
      )
      val intToStringMap = Data.map(
        "item" -> Data("scala.Tuple2[scala.Int, java.lang.String]"),
        "ctorResult" -> Data("scala.collection.immutable.Map[scala.Int, java.lang.String]"),
        "build" -> Data("PlainValue"),
        "asMap" -> Data.map("key" -> Data("scala.Int"), "value" -> Data("java.lang.String")),
        "IsMap" -> Data.map("key" -> Data("scala.Int"), "value" -> Data("java.lang.String"))
      )

      test("for List") {
        testCollectionTypes[List[String]] <==> stringList
      }

      test("for an alias of List") {
        testCollectionTypes[StringList] <==> stringList
      }

      test("for Map and aliases of Map, including an alias with reordered type parameters") {
        testCollectionTypes[Map[Int, String]] <==> intToStringMap
        testCollectionTypes[IntKeyed[String]] <==> intToStringMap
        testCollectionTypes[FlippedMap[String, Int]] <==> intToStringMap
      }

      test("for Option and an alias of Option, built with a validating smart constructor") {
        val expected = Data.map(
          "item" -> Data("java.lang.String"),
          "ctorResult" -> Data("scala.collection.immutable.List[java.lang.String]"),
          "build" -> Data("EitherStringOrValue"),
          "asMap" -> Data("<not a map>"),
          "IsMap" -> Data("<not a map>")
        )
        testCollectionTypes[Option[String]] <==> expected
        testCollectionTypes[Maybe[String]] <==> expected
      }

      test("for String as a collection of Char") {
        testCollectionTypes[String] <==> Data.map(
          "item" -> Data("scala.Char"),
          "ctorResult" -> Data("java.lang.String"),
          "build" -> Data("PlainValue"),
          "asMap" -> Data("<not a map>"),
          "IsMap" -> Data("<not a map>")
        )
      }

      test("for Array") {
        testCollectionTypes[Array[Int]] <==> Data.map(
          "item" -> Data("scala.Int"),
          "ctorResult" -> Data("scala.Array[scala.Int]"),
          "build" -> Data("PlainValue"),
          "asMap" -> Data("<not a map>"),
          "IsMap" -> Data("<not a map>")
        )
      }

      test("for Iterator") {
        testCollectionTypes[Iterator[String]] <==> Data.map(
          "item" -> Data("java.lang.String"),
          "ctorResult" -> Data("scala.collection.Iterator[java.lang.String]"),
          "build" -> Data("PlainValue"),
          "asMap" -> Data("<not a map>"),
          "IsMap" -> Data("<not a map>")
        )
      }

      test("for Scala BitSet") {
        testCollectionTypes[scala.collection.immutable.BitSet] <==> Data.map(
          "item" -> Data("scala.Int"),
          "ctorResult" -> Data("scala.collection.immutable.BitSet"),
          "build" -> Data("PlainValue"),
          "asMap" -> Data("<not a map>"),
          "IsMap" -> Data("<not a map>")
        )
      }

      test("not for Range, which has no Factory building a Range") {
        testCollectionTypes[Range] <==> Data("<no collection>")
      }

      test("not for Some, None, primitives and case classes") {
        testCollectionTypes[Some[String]] <==> Data("<no collection>")
        testCollectionTypes[None.type] <==> Data("<no collection>")
        testCollectionTypes[Int] <==> Data("<no collection>")
        testCollectionTypes[NotAValueClass] <==> Data("<no collection>")
      }
    }

    group("class: IsCollectionOf[A, Item].factory and .build, builds a collection which can be iterated again") {
      import StdExtensionsFixtures.testBuildAndIterate
      import hearth.examples.stdedge.*

      def built(one: Data, two: Data): Data = Data.map("one" -> one, "two" -> two)

      test("for Scala collections") {
        testBuildAndIterate[List[Int]] <==> built(Data.list(Data("1")), Data.list(Data("1"), Data("2")))
        testBuildAndIterate[StringList] <==> built(Data.list(Data("a")), Data.list(Data("a"), Data("b")))
        testBuildAndIterate[Vector[Int]] <==> built(Data.list(Data("1")), Data.list(Data("1"), Data("2")))
        testBuildAndIterate[scala.collection.immutable.SortedSet[Int]] <==> built(
          Data.list(Data("1")),
          Data.list(Data("1"), Data("2"))
        )
        testBuildAndIterate[scala.collection.immutable.BitSet] <==> built(
          Data.list(Data("1")),
          Data.list(Data("1"), Data("2"))
        )
        testBuildAndIterate[scala.collection.mutable.ArrayBuffer[Int]] <==> built(
          Data.list(Data("1")),
          Data.list(Data("1"), Data("2"))
        )
      }

      test("for Scala maps") {
        testBuildAndIterate[Map[Int, String]] <==> built(
          Data.list(Data("(1,a)")),
          Data.list(Data("(1,a)"), Data("(2,b)"))
        )
        testBuildAndIterate[FlippedMap[String, Int]] <==> built(
          Data.list(Data("(1,a)")),
          Data.list(Data("(1,a)"), Data("(2,b)"))
        )
        testBuildAndIterate[scala.collection.immutable.SortedMap[Int, String]] <==> built(
          Data.list(Data("(1,a)")),
          Data.list(Data("(1,a)"), Data("(2,b)"))
        )
        testBuildAndIterate[scala.collection.mutable.Map[Int, String]] <==> built(
          Data.list(Data("(1,a)")),
          Data.list(Data("(1,a)"), Data("(2,b)"))
        )
      }

      test("for Array") {
        testBuildAndIterate[Array[Int]] <==> built(Data.list(Data("1")), Data.list(Data("1"), Data("2")))
        testBuildAndIterate[Array[String]] <==> built(Data.list(Data("a")), Data.list(Data("a"), Data("b")))
      }

      test("for String") {
        testBuildAndIterate[String] <==> built(Data.list(Data("a")), Data.list(Data("a"), Data("b")))
      }

      test("for Iterator") {
        testBuildAndIterate[Iterator[Int]] <==> built(Data.list(Data("1")), Data.list(Data("1"), Data("2")))
      }

      test("for Option, rejecting more than 1 item") {
        testBuildAndIterate[Option[Int]] <==> built(
          Data.list(Data("1")),
          Data("error: Expected at most 1 element for Option, got 2")
        )
        testBuildAndIterate[Maybe[String]] <==> built(
          Data.list(Data("a")),
          Data("error: Expected at most 1 element for Option, got 2")
        )
      }
    }

    group("class: IsCollectionOf[A, Item].sizeHintForBuilder") {
      import StdExtensionsFixtures.testSizeHint

      test("computes the size of strict collections") {
        testSizeHint(Vector("a", "b", "c")) <==> Data(3)
        testSizeHint(Array(1, 2, 3)) <==> Data(3)
        testSizeHint("abc") <==> Data(3)
        testSizeHint(Set("a", "b", "c")) <==> Data(3)
        testSizeHint(Map(1 -> "a", 2 -> "b")) <==> Data(2)
      }

      test("may compute a negative value when the size is unknown without traversal") {
        testSizeHint(List("a", "b", "c")) <==> Data(-1)
      }

      test("is None for single-pass Iterator") {
        testSizeHint(Iterator("a", "b", "c")) <==> Data("<no size hint>")
      }
    }

    group("class: IsOption[A], types and orElse") {
      import StdExtensionsFixtures.{testIsOptionOrElse, testIsOptionTypes}
      import hearth.examples.stdedge.*

      test("for Scala Option and its alias") {
        testIsOptionTypes[Option[Int]] <==> Data.map("item" -> Data("scala.Int"))
        testIsOptionTypes[Maybe[String]] <==> Data.map("item" -> Data("java.lang.String"))
      }

      test("not for Some, None, Either and collections") {
        testIsOptionTypes[Some[Int]] <==> Data("<no option>")
        testIsOptionTypes[None.type] <==> Data("<no option>")
        testIsOptionTypes[Either[String, Int]] <==> Data("<no option>")
        testIsOptionTypes[List[Int]] <==> Data("<no option>")
      }

      test("orElse keeps a non-empty option and falls back for an empty one") {
        testIsOptionOrElse(Option("value"), Option("default")) <==> Data("Some(value)")
        testIsOptionOrElse(Option.empty[String], Option("default")) <==> Data("Some(default)")
        testIsOptionOrElse(Option.empty[String], Option.empty[String]) <==> Data("None")
      }
    }

    group("class: IsEither[A], types, building and orElse") {
      import StdExtensionsFixtures.{testIsEitherOrElse, testIsEitherTypes}
      import hearth.examples.stdedge.*

      test("for Either and its alias with reordered type parameters") {
        val expected = Data.map(
          "types" -> Data.map("left" -> Data("java.lang.String"), "right" -> Data("scala.Int")),
          "building" -> Data("<not an either of throwable and int>")
        )
        testIsEitherTypes[Either[String, Int]] <==> expected
        testIsEitherTypes[FlippedEither[Int, String]] <==> expected
      }

      test("for Try, treated as Either[Throwable, A]") {
        testIsEitherTypes[scala.util.Try[Int]] <==> Data.map(
          "types" -> Data.map("left" -> Data("java.lang.Throwable"), "right" -> Data("scala.Int")),
          "building" -> Data.map(
            "left" -> Data("Failure(java.lang.IllegalStateException: boom)"),
            "right" -> Data("Success(42)")
          )
        )
        testIsEitherTypes[Either[Throwable, Int]] <==> Data.map(
          "types" -> Data.map("left" -> Data("java.lang.Throwable"), "right" -> Data("scala.Int")),
          "building" -> Data.map(
            "left" -> Data("Left(java.lang.IllegalStateException: boom)"),
            "right" -> Data("Right(42)")
          )
        )
      }

      test("not for Option and collections") {
        testIsEitherTypes[Option[Int]] <==> Data("<no either>")
        testIsEitherTypes[List[Int]] <==> Data("<no either>")
      }

      test("orElse keeps a right and falls back for a left") {
        testIsEitherOrElse(Right(1): Either[String, Int], Right(2): Either[String, Int]) <==> Data("Right(1)")
        testIsEitherOrElse(Left("error"): Either[String, Int], Right(2): Either[String, Int]) <==> Data("Right(2)")
        testIsEitherOrElse(
          scala.util.Failure(new Exception("error")): scala.util.Try[Int],
          scala.util.Success(2): scala.util.Try[Int]
        ) <==> Data("Success(2)")
        testIsEitherOrElse(
          scala.util.Success(1): scala.util.Try[Int],
          scala.util.Success(2): scala.util.Try[Int]
        ) <==> Data("Success(1)")
      }
    }

    group("class: IsValueType[A], recognised AnyVals") {
      import StdExtensionsFixtures.{testIsValueType, testIsValueTypeRewrapType, testValueTypeInfo}
      import hearth.examples.stdedge.*

      val stringWrapperInfo = Data.map(
        "inner" -> Data("java.lang.String"),
        "wrap" -> Data("PlainValue"),
        "ctors" -> Data.list(Data("PlainValue"), Data("PlainValue")),
        "provenance" -> Data.map(
          "provider" -> Data("hearth.std.extensions.IsValueTypeProviderForAnyVal"),
          "isBuiltIn" -> Data(true)
        )
      )

      test("for an AnyVal") {
        testValueTypeInfo[StringWrapper] <==> stringWrapperInfo
      }

      test("for an alias of an AnyVal") {
        testValueTypeInfo[StringWrapperAlias] <==> stringWrapperInfo
      }

      test("for a generic AnyVal") {
        testValueTypeInfo[GenericWrapper[String]] <==> Data.map(
          "inner" -> Data("java.lang.String"),
          "wrap" -> Data("PlainValue"),
          "ctors" -> Data.list(Data("PlainValue")),
          "provenance" -> Data.map(
            "provider" -> Data("hearth.std.extensions.IsValueTypeProviderForAnyVal"),
            "isBuiltIn" -> Data(true)
          )
        )
      }

      test("wraps and unwraps an AnyVal") {
        testIsValueType(StringWrapper("value")) <==> Data.map(
          "unwrapping" -> Data("unwrapped: value"),
          "wrapping" -> Data("StringWrapper(test)")
        )
        testIsValueType(GenericWrapper("value")) <==> Data.map(
          "unwrapping" -> Data("unwrapped: value"),
          "wrapping" -> Data("GenericWrapper(test)")
        )
      }

      test("re-wrapping an AnyVal yields the AnyVal, not its inner type") {
        val expected = Data.map(
          "rewrappedIsValueType" -> Data(true),
          "rewrappedIsInner" -> Data(false)
        )
        testIsValueTypeRewrapType(StringWrapper("value")) <==> expected
        testIsValueTypeRewrapType(hearth.examples.ExampleValueClass(1)) <==> expected
      }

      test("not for an AnyVal with a private constructor, a non-AnyVal case class, primitives or String") {
        testValueTypeInfo[PrivateCtorWrapper] <==> Data("<no value type>")
        testValueTypeInfo[NotAValueClass] <==> Data("<no value type>")
        testValueTypeInfo[Int] <==> Data("<no value type>")
        testValueTypeInfo[String] <==> Data("<no value type>")
      }
    }

    group("class: CtorLikes[A], other smart constructor shapes") {
      import StdExtensionsFixtures.testCtorLikes
      import hearth.examples.stdedge.*

      // The private constructor cannot be called from the macro call site, so it must not be offered.
      test("for a class with Either[Throwable, A] smart constructor and a private constructor") {
        testCtorLikes[ThrowingCtor] <==> Data.map(
          "count" -> Data("1"),
          "ctors" -> Data.list(
            Data.map(
              "type" -> Data("EitherThrowableOrValue"),
              "input" -> Data("scala.Int"),
              "method" -> Data("attempt")
            )
          )
        )
      }

      test("for a class with Either[Iterable[Throwable], A] smart constructor and a private constructor") {
        testCtorLikes[AccumulatingThrowingCtor] <==> Data.map(
          "count" -> Data("1"),
          "ctors" -> Data.list(
            Data.map(
              "type" -> Data("EitherIterableThrowableOrValue"),
              "input" -> Data("scala.Int"),
              "method" -> Data("validateAll")
            )
          )
        )
      }

      test("for a class with a public single-argument constructor") {
        testCtorLikes[PublicSingleArgCtor] <==> Data.map(
          "count" -> Data("1"),
          "ctors" -> Data.list(
            Data.map("type" -> Data("PlainValue"), "input" -> Data("java.lang.String"), "method" -> Data("<init>"))
          )
        )
      }

      test("not for a class whose only unary constructor is private") {
        testCtorLikes[PrivateCtorWrapper] <==> Data("<no ctors>")
      }

      test("not for a class without unary constructors") {
        testCtorLikes[TwoArgCtor] <==> Data("<no ctors>")
      }
    }

    group("lastMatchProvenance") {
      import StdExtensionsFixtures.{testIsMapProvenance, testProvenance}

      def builtIn(provider: String): Data =
        Data.map("provider" -> Data(s"hearth.std.extensions.$provider"), "isBuiltIn" -> Data(true))

      test("names the built-in provider which matched, and is empty for the companions which did not match") {
        testProvenance[List[String]] <==> Data.map(
          "IsCollection" -> builtIn("IsCollectionProviderForScalaCollection"),
          "IsOption" -> Data("<none>"),
          "IsEither" -> Data("<none>"),
          "IsValueType" -> Data("<none>")
        )
        testProvenance[Option[String]] <==> Data.map(
          "IsCollection" -> builtIn("IsCollectionProviderForScalaOption"),
          "IsOption" -> builtIn("IsOptionProviderForScalaOption"),
          "IsEither" -> Data("<none>"),
          "IsValueType" -> Data("<none>")
        )
        testProvenance[scala.util.Try[String]] <==> Data.map(
          "IsCollection" -> Data("<none>"),
          "IsOption" -> Data("<none>"),
          "IsEither" -> builtIn("IsEitherProviderForScalaTry"),
          "IsValueType" -> Data("<none>")
        )
        testProvenance[hearth.examples.ExampleValueClass] <==> Data.map(
          "IsCollection" -> Data("<none>"),
          "IsOption" -> Data("<none>"),
          "IsEither" -> Data("<none>"),
          "IsValueType" -> builtIn("IsValueTypeProviderForAnyVal")
        )
        testProvenance[Int] <==> Data.map(
          "IsCollection" -> Data("<none>"),
          "IsOption" -> Data("<none>"),
          "IsEither" -> Data("<none>"),
          "IsValueType" -> Data("<none>")
        )
      }

      test("for IsMap mirrors the collection provider") {
        testIsMapProvenance[Map[Int, String]] <==> Data.map(
          "result" -> Data("matched"),
          "provenance" -> builtIn("IsCollectionProviderForScalaCollection")
        )
        testIsMapProvenance[Int] <==> Data.map(
          "result" -> Data("skipped"),
          "provenance" -> Data("<none>")
        )
      }
    }

    group("ProviderResult.Skipped reasons via parse") {
      import StdExtensionsFixtures.testSkipReasons

      val bottomReason = "bottom types (Nothing/Null) conform to every type but no provider can build a value of them"

      test("for Nothing and Null, every companion skips without consulting providers") {
        val expected = Data.map(
          "IsCollection" -> Data.map("IsCollection" -> Data(bottomReason)),
          "IsMap" -> Data.map("IsCollection" -> Data(bottomReason)),
          "IsOption" -> Data.map("IsOption" -> Data(bottomReason)),
          "IsEither" -> Data.map("IsEither" -> Data(bottomReason)),
          "IsValueType" -> Data.map("IsValueType" -> Data(bottomReason)),
          "CtorLikes" -> Data.map("CtorLikes" -> Data(bottomReason))
        )
        testSkipReasons[Nothing] <==> expected
        testSkipReasons[Null] <==> expected
      }

      test("for a collection which is not a map, IsMap explains why it skipped") {
        testSkipReasons[List[String]] <==> Data.map(
          "IsCollection" -> Data("matched"),
          "IsMap" -> Data.map(
            "IsMap" -> Data("scala.collection.immutable.List[java.lang.String] is a collection but not a Map")
          ),
          "IsOption" -> Data("<skipped by multiple providers>"),
          "IsEither" -> Data("<skipped by multiple providers>"),
          "IsValueType" -> Data("<skipped by multiple providers>"),
          "CtorLikes" -> Data("<skipped by multiple providers>")
        )
      }
    }

    group("lastUnapplyFailure tracking on every companion") {
      import StdExtensionsFixtures.testUnapplyFailures

      test("is set after a failed unapply and cleared after a successful one") {
        testUnapplyFailures[Map[Int, String]] <==> Data.map(
          "IsMap" -> Data.map("matched" -> Data(true), "hasFailure" -> Data(false)),
          "IsOption" -> Data.map("matched" -> Data(false), "hasFailure" -> Data(true)),
          "IsEither" -> Data.map("matched" -> Data(false), "hasFailure" -> Data(true)),
          "IsValueType" -> Data.map("matched" -> Data(false), "hasFailure" -> Data(true)),
          "CtorLikes" -> Data.map("matched" -> Data(false), "hasFailure" -> Data(true))
        )
        testUnapplyFailures[Either[String, Int]] <==> Data.map(
          "IsMap" -> Data.map("matched" -> Data(false), "hasFailure" -> Data(true)),
          "IsOption" -> Data.map("matched" -> Data(false), "hasFailure" -> Data(true)),
          "IsEither" -> Data.map("matched" -> Data(true), "hasFailure" -> Data(false)),
          "IsValueType" -> Data.map("matched" -> Data(false), "hasFailure" -> Data(true)),
          "CtorLikes" -> Data.map("matched" -> Data(false), "hasFailure" -> Data(true))
        )
        testUnapplyFailures[hearth.examples.ExampleValueClass] <==> Data.map(
          "IsMap" -> Data.map("matched" -> Data(false), "hasFailure" -> Data(true)),
          "IsOption" -> Data.map("matched" -> Data(false), "hasFailure" -> Data(true)),
          "IsEither" -> Data.map("matched" -> Data(false), "hasFailure" -> Data(true)),
          "IsValueType" -> Data.map("matched" -> Data(true), "hasFailure" -> Data(false)),
          "CtorLikes" -> Data.map("matched" -> Data(true), "hasFailure" -> Data(false))
        )
      }
    }
  }
}
