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
          "building" -> Data(if (Platform.byHearth.isNative) "scala.scalanative.runtime.ObjectArray" else "[Ljava.lang.String"),
          "iteration" -> Data.list(
            Data("one"),
            Data("two"),
            Data("three")
          )
        )
      }
    }

    // TODO: IsOption
    // TODO: IsEither
    // TODO: IsValueType
  }
}
