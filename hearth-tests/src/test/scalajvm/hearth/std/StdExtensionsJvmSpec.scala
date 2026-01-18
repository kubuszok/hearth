package hearth
package std

import hearth.data.Data

/** Macro implementation is in [[StdExtensionsFixturesImpl]] */
final class StdExtensionsJvmSpec extends MacroSuite {

  group("trait std.StdExtensions") {

    group("class: IsCollection[A], returns preprocessed collection") {
      import StdExtensionsFixtures.testIsCollection

      test("for Scala WeakHashMap") {
        testIsCollection(scala.collection.mutable.WeakHashMap(1 -> "one", 2 -> "two")) <==> Data.map(
          "iteration" -> Data.list(
            Data.map("key" -> Data("2"), "value" -> Data("two")),
            Data.map("key" -> Data("1"), "value" -> Data("one"))
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
        testIsCollection[java.util.PriorityQueue[String]](new java.util.PriorityQueue[String]() {
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
        testIsCollection[java.util.HashSet[String]](new java.util.HashSet[String]() {
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
          "iteration" -> Data.list(
            Data("one"),
            Data("two"),
            Data("three")
          ),
          "building" -> Data("[one]")
        )
        testIsCollection[java.util.PriorityQueue[String]](new java.util.PriorityQueue[String]() {
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
      }

      test("for java.util.Set") {
        val set: java.util.Set[String] = new java.util.HashSet[String]() {
          add("one")
          add("two")
          add("three")
        }
        testIsCollection(set) <==> Data.map(
          "iteration" -> Data.list(
            Data("one"),
            Data("two"),
            Data("three")
          ),
          "building" -> Data("[one]")
        )
        testIsCollection[java.util.HashSet[String]](new java.util.HashSet[String]() {
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
          "iteration" -> Data.list(
            Data.map("key" -> Data("2"), "value" -> Data("two")),
            Data.map("key" -> Data("1"), "value" -> Data("one"))
          ),
          "building" -> Data("{1=one}")
        )
        testIsCollection[java.util.Hashtable[Int, String]](new java.util.Hashtable[Int, String]() {
          put(1, "one")
          put(2, "two")
        }) <==> Data.map(
          "iteration" -> Data.list(
            Data.map("key" -> Data("2"), "value" -> Data("two")),
            Data.map("key" -> Data("1"), "value" -> Data("one"))
          ),
          "building" -> Data("{1=one}")
        )
        testIsCollection {
          val props = new java.util.Properties()
          props.put("one", "1")
          props.put("two", "2")
          props
        } <==> Data.map(
          "iteration" -> Data.list(
            Data.map("key" -> Data("one"), "value" -> Data("1")),
            Data.map("key" -> Data("two"), "value" -> Data("2"))
          ),
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
          "iteration" -> Data.list(
            Data.map("key" -> Data("1"), "value" -> Data("one")),
            Data.map("key" -> Data("2"), "value" -> Data("two"))
          ),
          "building" -> Data("{1=one}")
        )
        testIsCollection[java.util.HashMap[Int, String]](new java.util.HashMap[Int, String]() {
          put(1, "one")
          put(2, "two")
        }) <==> Data.map(
          "iteration" -> Data.list(
            Data.map("key" -> Data("1"), "value" -> Data("one")),
            Data.map("key" -> Data("2"), "value" -> Data("two"))
          ),
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
        testIsCollection[java.util.WeakHashMap[Int, String]](new java.util.WeakHashMap[Int, String]() {
          put(1, "one")
          put(2, "two")
        }) <==> Data.map(
          "iteration" -> Data.list(
            Data.map("key" -> Data("2"), "value" -> Data("two")),
            Data.map("key" -> Data("1"), "value" -> Data("one"))
          ),
          "building" -> Data("{1=one}")
        )
        testIsCollection[java.util.IdentityHashMap[Int, String]](new java.util.IdentityHashMap[Int, String]() {
          put(1, "one")
          put(2, "two")
        }) <==> Data.map(
          "iteration" -> Data.list(
            Data.map("key" -> Data("1"), "value" -> Data("one")),
            Data.map("key" -> Data("2"), "value" -> Data("two"))
          ),
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
