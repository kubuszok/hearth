package hearth
package std

import hearth.data.Data

/** Macro implementation is in [[StdExtensionsFixturesImpl]] */
final class StdExtensionsJvmSpec extends MacroSuite {

  // Helper to generate expected iteration values for collections with unpredictable order
  private def expectedIterationForCollection[A](coll: java.lang.Iterable[A]): Data = {
    import scala.jdk.CollectionConverters.*
    Data.list(coll.asScala.map(item => Data(item.toString)).toSeq*)
  }

  // Helper to generate expected iteration values for maps with unpredictable order
  private def expectedIterationForMap[K, V](map: java.util.Map[K, V]): Data = {
    import scala.jdk.CollectionConverters.*
    Data.list(
      map.asScala.map { case (k, v) =>
        Data.map("key" -> Data(k.toString), "value" -> Data(v.toString))
      }.toSeq*
    )
  }

  // Helper for Dictionary types (legacy API)
  private def expectedIterationForDictionary[K, V](dict: java.util.Dictionary[K, V]): Data = {
    import scala.jdk.CollectionConverters.*
    Data.list(
      dict.asScala.map { case (k, v) =>
        Data.map("key" -> Data(k.toString), "value" -> Data(v.toString))
      }.toSeq*
    )
  }

  group("trait std.StdExtensions") {

    group("class: IsCollection[A], returns preprocessed collection") {
      import StdExtensionsFixtures.testIsCollection

      test("for Scala WeakHashMap") {
        val weakHashMap = scala.collection.mutable.WeakHashMap(1 -> "one", 2 -> "two")
        testIsCollection(weakHashMap) <==> Data.map(
          "iteration" -> Data.list(
            weakHashMap.map { case (k, v) =>
              Data.map("key" -> Data(k.toString), "value" -> Data(v.toString))
            }.toSeq*
          ),
          "building" -> Data("WeakHashMap(1 -> one)")
        )
      }

      test("for java.util.Iterator") {
        val iter: java.util.Iterator[String] = java.util.Arrays.asList("one", "two", "three").iterator()
        testIsCollection(iter) <==> Data.map(
          "iteration" -> Data.list(
            Data("one"),
            Data("two"),
            Data("three")
          ),
          "building" -> Data("java.util.ArrayList")
        )
        testIsCollection(java.util.Arrays.asList("one", "two", "three").iterator()) <==> Data.map(
          "iteration" -> Data.list(
            Data("one"),
            Data("two"),
            Data("three")
          ),
          "building" -> Data("java.util.ArrayList")
        )
      }

      test("for java.util.Enumeration") {
        val enumm: java.util.Enumeration[String] =
          java.util.Collections.enumeration(java.util.Arrays.asList("one", "two", "three"))
        testIsCollection(enumm) <==> Data.map(
          "iteration" -> Data.list(
            Data("one"),
            Data("two"),
            Data("three")
          ),
          "building" -> Data("java.util.Vector")
        )
        testIsCollection(java.util.Collections.enumeration(java.util.Arrays.asList("one", "two", "three"))) <==> Data
          .map(
            "iteration" -> Data.list(
              Data("one"),
              Data("two"),
              Data("three")
            ),
            "building" -> Data("java.util.Vector")
          )
      }

      test("for java.util.Collection") {
        val coll: java.util.Collection[String] = new java.util.ArrayList[String]() {
          add("one")
          add("two")
          add("three")
        }
        testIsCollection(coll) <==> Data.map(
          "iteration" -> Data.list(
            Data("one"),
            Data("two"),
            Data("three")
          ),
          "building" -> Data("[one]")
        )
        testIsCollection[java.util.ArrayList[String]](new java.util.ArrayList[String]() {
          add("one")
          add("two")
          add("three")
        }) <==> Data.map(
          "iteration" -> Data.list(
            Data("one"),
            Data("two"),
            Data("three")
          ),
          "building" -> Data("[one]")
        )
        testIsCollection[java.util.LinkedList[String]](new java.util.LinkedList[String]() {
          add("one")
          add("two")
          add("three")
        }) <==> Data.map(
          "iteration" -> Data.list(
            Data("one"),
            Data("two"),
            Data("three")
          ),
          "building" -> Data("[one]")
        )
        testIsCollection[java.util.Vector[String]](new java.util.Vector[String]() {
          add("one")
          add("two")
          add("three")
        }) <==> Data.map(
          "iteration" -> Data.list(
            Data("one"),
            Data("two"),
            Data("three")
          ),
          "building" -> Data("[one]")
        )
        testIsCollection[java.util.Stack[String]](new java.util.Stack[String]() {
          add("one")
          add("two")
          add("three")
        }) <==> Data.map(
          "iteration" -> Data.list(
            Data("one"),
            Data("two"),
            Data("three")
          ),
          "building" -> Data("[one]")
        )
        testIsCollection[java.util.ArrayDeque[String]](new java.util.ArrayDeque[String]() {
          add("one")
          add("two")
          add("three")
        }) <==> Data.map(
          "iteration" -> Data.list(
            Data("one"),
            Data("two"),
            Data("three")
          ),
          "building" -> Data("[one]")
        )
        val pq1 = new java.util.PriorityQueue[String]()
        pq1.add("one")
        pq1.add("two")
        pq1.add("three")
        testIsCollection[java.util.PriorityQueue[String]](pq1) <==> Data.map(
          "iteration" -> expectedIterationForCollection(pq1),
          "building" -> Data("[one]")
        )
        val hs1 = new java.util.HashSet[String]()
        hs1.add("one")
        hs1.add("two")
        hs1.add("three")
        testIsCollection[java.util.HashSet[String]](hs1) <==> Data.map(
          "iteration" -> expectedIterationForCollection(hs1),
          "building" -> Data("[one]")
        )
        testIsCollection[java.util.LinkedHashSet[String]](new java.util.LinkedHashSet[String]() {
          add("one")
          add("two")
          add("three")
        }) <==> Data.map(
          "iteration" -> Data.list(
            Data("one"),
            Data("two"),
            Data("three")
          ),
          "building" -> Data("[one]")
        )
        testIsCollection[java.util.TreeSet[String]](new java.util.TreeSet[String]() {
          add("three")
          add("one")
          add("two")
        }) <==> Data.map(
          "iteration" -> Data.list(
            Data("one"),
            Data("three"),
            Data("two")
          ),
          "building" -> Data("[one]")
        )
      }

      test("for java.util.List") {
        val list: java.util.List[String] = new java.util.ArrayList[String]() {
          add("one")
          add("two")
          add("three")
        }
        testIsCollection(list) <==> Data.map(
          "iteration" -> Data.list(
            Data("one"),
            Data("two"),
            Data("three")
          ),
          "building" -> Data("[one]")
        )
        testIsCollection[java.util.ArrayList[String]](new java.util.ArrayList[String]() {
          add("one")
          add("two")
          add("three")
        }) <==> Data.map(
          "iteration" -> Data.list(
            Data("one"),
            Data("two"),
            Data("three")
          ),
          "building" -> Data("[one]")
        )
        testIsCollection[java.util.LinkedList[String]](new java.util.LinkedList[String]() {
          add("one")
          add("two")
          add("three")
        }) <==> Data.map(
          "iteration" -> Data.list(
            Data("one"),
            Data("two"),
            Data("three")
          ),
          "building" -> Data("[one]")
        )
        testIsCollection[java.util.Vector[String]](new java.util.Vector[String]() {
          add("one")
          add("two")
          add("three")
        }) <==> Data.map(
          "iteration" -> Data.list(
            Data("one"),
            Data("two"),
            Data("three")
          ),
          "building" -> Data("[one]")
        )
        testIsCollection[java.util.Stack[String]](new java.util.Stack[String]() {
          add("one")
          add("two")
          add("three")
        }) <==> Data.map(
          "iteration" -> Data.list(
            Data("one"),
            Data("two"),
            Data("three")
          ),
          "building" -> Data("[one]")
        )
      }

      test("for java.util.Deque") {
        val deque: java.util.Deque[String] = new java.util.ArrayDeque[String]() {
          add("one")
          add("two")
          add("three")
        }
        testIsCollection(deque) <==> Data.map(
          "iteration" -> Data.list(
            Data("one"),
            Data("two"),
            Data("three")
          ),
          "building" -> Data("[one]")
        )
        testIsCollection[java.util.ArrayDeque[String]](new java.util.ArrayDeque[String]() {
          add("one")
          add("two")
          add("three")
        }) <==> Data.map(
          "iteration" -> Data.list(
            Data("one"),
            Data("two"),
            Data("three")
          ),
          "building" -> Data("[one]")
        )
        testIsCollection[java.util.LinkedList[String]](new java.util.LinkedList[String]() {
          add("one")
          add("two")
          add("three")
        }) <==> Data.map(
          "iteration" -> Data.list(
            Data("one"),
            Data("two"),
            Data("three")
          ),
          "building" -> Data("[one]")
        )
      }

      test("for java.util.Queue") {
        val queue: java.util.Queue[String] = new java.util.PriorityQueue[String]() {
          add("one")
          add("two")
          add("three")
        }
        testIsCollection(queue) <==> Data.map(
          "iteration" -> expectedIterationForCollection(queue),
          "building" -> Data("[one]")
        )
        val pq2 = new java.util.PriorityQueue[String]()
        pq2.add("one")
        pq2.add("two")
        pq2.add("three")
        testIsCollection[java.util.PriorityQueue[String]](pq2) <==> Data.map(
          "iteration" -> expectedIterationForCollection(pq2),
          "building" -> Data("[one]")
        )
        testIsCollection[java.util.ArrayDeque[String]](new java.util.ArrayDeque[String]() {
          add("one")
          add("two")
          add("three")
        }) <==> Data.map(
          "iteration" -> Data.list(
            Data("one"),
            Data("two"),
            Data("three")
          ),
          "building" -> Data("[one]")
        )
      }

      test("for java.util.Set") {
        val set: java.util.Set[String] = new java.util.HashSet[String]() {
          add("one")
          add("two")
          add("three")
        }
        testIsCollection(set) <==> Data.map(
          "iteration" -> expectedIterationForCollection(set),
          "building" -> Data("[one]")
        )
        val hs2 = new java.util.HashSet[String]()
        hs2.add("one")
        hs2.add("two")
        hs2.add("three")
        testIsCollection[java.util.HashSet[String]](hs2) <==> Data.map(
          "iteration" -> expectedIterationForCollection(hs2),
          "building" -> Data("[one]")
        )
        testIsCollection[java.util.LinkedHashSet[String]](new java.util.LinkedHashSet[String]() {
          add("one")
          add("two")
          add("three")
        }) <==> Data.map(
          "iteration" -> Data.list(
            Data("one"),
            Data("two"),
            Data("three")
          ),
          "building" -> Data("[one]")
        )
        testIsCollection[java.util.TreeSet[String]](new java.util.TreeSet[String]() {
          add("three")
          add("one")
          add("two")
        }) <==> Data.map(
          "iteration" -> Data.list(
            Data("one"),
            Data("three"),
            Data("two")
          ),
          "building" -> Data("[one]")
        )
      }

      test("for java.util.BitSet") {
        testIsCollection {
          val bs = new java.util.BitSet()
          bs.set(1)
          bs.set(2)
          bs.set(3)
          bs
        } <==> Data.map(
          "iteration" -> Data.list(
            Data("1"),
            Data("2"),
            Data("3")
          ),
          "building" -> Data("<not a collection of string>")
        )
      }

      test("for java.util.stream.BaseStream") {
        val stream: java.util.stream.Stream[String] = java.util.stream.Stream.of("one", "two", "three")
        testIsCollection(stream) <==> Data.map(
          "iteration" -> Data.list(
            Data("one"),
            Data("two"),
            Data("three")
          ),
          "building" -> Data("java.util.stream.ReferencePipeline")
        )
        testIsCollection(java.util.stream.Stream.of("one", "two", "three")) <==> Data.map(
          "iteration" -> Data.list(
            Data("one"),
            Data("two"),
            Data("three")
          ),
          "building" -> Data("java.util.stream.ReferencePipeline")
        )
        testIsCollection(java.util.stream.IntStream.of(1, 2, 3)) <==> Data.map(
          "iteration" -> Data.list(
            Data("1"),
            Data("2"),
            Data("3")
          ),
          "building" -> Data("<not a collection of string>")
        )
        testIsCollection(java.util.stream.LongStream.of(1L, 2L, 3L)) <==> Data.map(
          "iteration" -> Data.list(
            Data("1"),
            Data("2"),
            Data("3")
          ),
          "building" -> Data("<not a collection of string>")
        )
        testIsCollection(java.util.stream.DoubleStream.of(1.0, 2.0, 3.0)) <==> Data.map(
          "iteration" -> Data.list(
            Data("1.0"),
            Data("2.0"),
            Data("3.0")
          ),
          "building" -> Data("<not a collection of string>")
        )
      }

      test("for java.util.Dictionary") {
        @scala.annotation.nowarn
        val dict: java.util.Dictionary[Int, String] = new java.util.Hashtable[Int, String]() {
          put(1, "one")
          put(2, "two")
        }
        testIsCollection(dict) <==> Data.map(
          "iteration" -> expectedIterationForDictionary(dict),
          "building" -> Data("{1=one}")
        )
        val ht = new java.util.Hashtable[Int, String]()
        ht.put(1, "one")
        ht.put(2, "two")
        testIsCollection[java.util.Hashtable[Int, String]](ht) <==> Data.map(
          "iteration" -> expectedIterationForDictionary(ht),
          "building" -> Data("{1=one}")
        )
        val props = new java.util.Properties()
        props.put("one", "1")
        props.put("two", "2")
        testIsCollection(props) <==> Data.map(
          "iteration" -> expectedIterationForDictionary(props),
          "building" -> Data("<not a map of int and string>")
        )
      }

      test("for java.util.Map") {
        @scala.annotation.nowarn
        val map: java.util.Map[Int, String] = new java.util.HashMap[Int, String]() {
          put(1, "one")
          put(2, "two")
        }
        testIsCollection(map) <==> Data.map(
          "iteration" -> expectedIterationForMap(map),
          "building" -> Data("{1=one}")
        )
        val hm = new java.util.HashMap[Int, String]()
        hm.put(1, "one")
        hm.put(2, "two")
        testIsCollection[java.util.HashMap[Int, String]](hm) <==> Data.map(
          "iteration" -> expectedIterationForMap(hm),
          "building" -> Data("{1=one}")
        )
        testIsCollection[java.util.LinkedHashMap[Int, String]](new java.util.LinkedHashMap[Int, String]() {
          put(1, "one")
          put(2, "two")
        }) <==> Data.map(
          "iteration" -> Data.list(
            Data.map("key" -> Data("1"), "value" -> Data("one")),
            Data.map("key" -> Data("2"), "value" -> Data("two"))
          ),
          "building" -> Data("{1=one}")
        )
        testIsCollection[java.util.TreeMap[Int, String]](new java.util.TreeMap[Int, String]() {
          put(2, "two")
          put(1, "one")
        }) <==> Data.map(
          "iteration" -> Data.list(
            Data.map("key" -> Data("1"), "value" -> Data("one")),
            Data.map("key" -> Data("2"), "value" -> Data("two"))
          ),
          "building" -> Data("{1=one}")
        )
        val whm = new java.util.WeakHashMap[Int, String]()
        whm.put(1, "one")
        whm.put(2, "two")
        testIsCollection[java.util.WeakHashMap[Int, String]](whm) <==> Data.map(
          "iteration" -> expectedIterationForMap(whm),
          "building" -> Data("{1=one}")
        )
        val ihm = new java.util.IdentityHashMap[Int, String]()
        ihm.put(1, "one")
        ihm.put(2, "two")
        testIsCollection[java.util.IdentityHashMap[Int, String]](ihm) <==> Data.map(
          "iteration" -> expectedIterationForMap(ihm),
          "building" -> Data("{1=one}")
        )
      }
    }

    group("class: IsOption[A], returns preprocessed option") {
      import StdExtensionsFixtures.testIsOption

      test("for Java Optional") {
        testIsOption(java.util.Optional.of("value")) <==> Data.map(
          "folding" -> Data("some: value"),
          "getOrElse" -> Data("value"),
          "building" -> Data.map(
            "some" -> Data("Optional[test]"),
            "empty" -> Data("Optional.empty")
          )
        )
        testIsOption(java.util.Optional.empty[String]) <==> Data.map(
          "folding" -> Data("empty"),
          "getOrElse" -> Data("default"),
          "building" -> Data.map(
            "some" -> Data("Optional[test]"),
            "empty" -> Data("Optional.empty")
          )
        )
      }

      test("for Java OptionalInt") {
        testIsOption(java.util.OptionalInt.of(42)) <==> Data.map(
          "folding" -> Data("some: 42"),
          "getOrElse" -> Data("<not an option of string>"),
          "building" -> Data("<not an option of string>")
        )
        testIsOption(java.util.OptionalInt.empty()) <==> Data.map(
          "folding" -> Data("empty"),
          "getOrElse" -> Data("<not an option of string>"),
          "building" -> Data("<not an option of string>")
        )
      }

      test("for Java OptionalLong") {
        testIsOption(java.util.OptionalLong.of(42L)) <==> Data.map(
          "folding" -> Data("some: 42"),
          "getOrElse" -> Data("<not an option of string>"),
          "building" -> Data("<not an option of string>")
        )
        testIsOption(java.util.OptionalLong.empty()) <==> Data.map(
          "folding" -> Data("empty"),
          "getOrElse" -> Data("<not an option of string>"),
          "building" -> Data("<not an option of string>")
        )
      }

      test("for Java OptionalDouble") {
        testIsOption(java.util.OptionalDouble.of(3.14)) <==> Data.map(
          "folding" -> Data("some: 3.14"),
          "getOrElse" -> Data("<not an option of string>"),
          "building" -> Data("<not an option of string>")
        )
        testIsOption(java.util.OptionalDouble.empty()) <==> Data.map(
          "folding" -> Data("empty"),
          "getOrElse" -> Data("<not an option of string>"),
          "building" -> Data("<not an option of string>")
        )
      }
    }

    group("class: IsValueType[A], returns preprocessed value type") {
      import StdExtensionsFixtures.testIsValueType

      test("for Java Byte") {
        testIsValueType(java.lang.Byte.valueOf(42.toByte)) <==> Data.map(
          "unwrapping" -> Data("unwrapped: 42"),
          "wrapping" -> Data("<not a value type of string>")
        )
      }
      test("for Java Boolean") {
        testIsValueType(java.lang.Boolean.valueOf(true)) <==> Data.map(
          "unwrapping" -> Data("unwrapped: true"),
          "wrapping" -> Data("<not a value type of string>")
        )
      }
      test("for Java Character") {
        testIsValueType(java.lang.Character.valueOf('A')) <==> Data.map(
          "unwrapping" -> Data("unwrapped: A"),
          "wrapping" -> Data("<not a value type of string>")
        )
      }
      test("for Java Integer") {
        testIsValueType(java.lang.Integer.valueOf(42)) <==> Data.map(
          "unwrapping" -> Data("unwrapped: 42"),
          "wrapping" -> Data("<not a value type of string>")
        )
      }
      test("for Java Long") {
        testIsValueType(java.lang.Long.valueOf(42L)) <==> Data.map(
          "unwrapping" -> Data("unwrapped: 42"),
          "wrapping" -> Data("<not a value type of string>")
        )
      }
      test("for Java Short") {
        testIsValueType(java.lang.Short.valueOf(42.toShort)) <==> Data.map(
          "unwrapping" -> Data("unwrapped: 42"),
          "wrapping" -> Data("<not a value type of string>")
        )
      }
      test("for Java Float") {
        testIsValueType(java.lang.Float.valueOf(42.0f)) <==> Data.map(
          "unwrapping" -> Data("unwrapped: 42.0"),
          "wrapping" -> Data("<not a value type of string>")
        )
      }
      test("for Java Double") {
        testIsValueType(java.lang.Double.valueOf(42.0)) <==> Data.map(
          "unwrapping" -> Data("unwrapped: 42.0"),
          "wrapping" -> Data("<not a value type of string>")
        )
      }
    }
  }
}
