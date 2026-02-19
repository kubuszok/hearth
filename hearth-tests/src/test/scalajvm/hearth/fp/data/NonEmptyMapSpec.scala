package hearth
package fp
package data

import org.scalacheck.Prop.*
import scala.collection.immutable.ListMap

final class NonEmptyMapSpec extends ScalaCheckSuite with Laws {

  group("Constructor and Factory Methods") {

    test("apply should create NonEmptyMap with head and varargs") {
      val nem = NonEmptyMap(("a", 1), ("b", 2), ("c", 3), ("d", 4), ("e", 5))
      nem.head === ("a", 1)
      nem.tail === ListMap(("b", 2), ("c", 3), ("d", 4), ("e", 5))
      nem.toList === List(("a", 1), ("b", 2), ("c", 3), ("d", 4), ("e", 5))
    }

    test("apply should create single element NonEmptyMap") {
      val nem = NonEmptyMap(("key", 42))
      nem.head === ("key", 42)
      nem.tail === ListMap.empty
      nem.toList === List(("key", 42))
    }

    test("fromListMap should return Some for non-empty ListMap") {
      val listMap = ListMap(("a", 1), ("b", 2), ("c", 3), ("d", 4), ("e", 5))
      val result = NonEmptyMap.fromListMap(listMap)
      assert(result.isDefined)
      result.get.head === ("a", 1)
      result.get.tail === ListMap(("b", 2), ("c", 3), ("d", 4), ("e", 5))
    }

    test("fromListMap should return None for empty ListMap") {
      val result = NonEmptyMap.fromListMap(ListMap.empty[String, Int])
      assert(result.isEmpty)
    }

    test("one should create single-element NonEmptyMap") {
      val nem = NonEmptyMap.one(("key", 42))
      nem.head === ("key", 42)
      nem.tail === ListMap.empty
      nem.toList === List(("key", 42))
    }
  }

  group("Collection Operations") {

    test("+: should prepend key-value pair") {
      val nem = NonEmptyMap(("b", 2), ("c", 3), ("d", 4))
      val result = ("a", 1) +: nem
      result.head === ("a", 1)
      result.tail === ListMap(("b", 2), ("c", 3), ("d", 4))
      result.toList === List(("a", 1), ("b", 2), ("c", 3), ("d", 4))
    }

    test(":+ should append key-value pair") {
      val nem = NonEmptyMap(("a", 1), ("b", 2), ("c", 3))
      val result = nem :+ ("d", 4)
      result.head === ("a", 1)
      result.tail === ListMap(("b", 2), ("c", 3), ("d", 4))
      result.toList === List(("a", 1), ("b", 2), ("c", 3), ("d", 4))
    }

    test("+: should replace existing key") {
      val nem = NonEmptyMap(("a", 1), ("b", 2))
      val result = ("a", 10) +: nem
      result.head === ("a", 10)
      result.tail === ListMap(("b", 2))
      result.toList === List(("a", 10), ("b", 2))
    }

    test(":+ should replace existing key") {
      val nem = NonEmptyMap(("a", 1), ("b", 2))
      val result = nem :+ ("b", 20)
      result.head === ("a", 1)
      result.tail === ListMap(("b", 20))
      result.toList === List(("a", 1), ("b", 20))
    }

    test("map should transform all key-value pairs") {
      val nem = NonEmptyMap(("a", 1), ("b", 2), ("c", 3), ("d", 4), ("e", 5))
      val result = nem.map { case (k, v) => (k.toUpperCase, v * 2) }
      result.head === ("A", 2)
      result.tail === ListMap(("B", 4), ("C", 6), ("D", 8), ("E", 10))
      result.toList === List(("A", 2), ("B", 4), ("C", 6), ("D", 8), ("E", 10))
    }

    test("flatMap should flatten and transform") {
      val nem = NonEmptyMap(("a", 1), ("b", 2), ("c", 3))
      val result = nem.flatMap { case (k, v) => NonEmptyMap((k, v), (k + k, v * 2)) }
      result.head === ("a", 1)
      result.tail === ListMap(("aa", 2), ("b", 2), ("bb", 4), ("c", 3), ("cc", 6))
      result.toList === List(("a", 1), ("aa", 2), ("b", 2), ("bb", 4), ("c", 3), ("cc", 6))
    }

    test("flatMap should handle single element transformation") {
      val nem = NonEmptyMap(("a", 1))
      val result = nem.flatMap { case (k, v) => NonEmptyMap((k, v * 10)) }
      result.head === ("a", 10)
      result.tail === ListMap.empty
      result.toList === List(("a", 10))
    }

    test("++ should combine two NonEmptyMaps") {
      val nem1 = NonEmptyMap(("a", 1), ("b", 2))
      val nem2 = NonEmptyMap(("c", 3), ("d", 4))
      val result = nem1 ++ nem2
      result.toList === List(("a", 1), ("b", 2), ("c", 3), ("d", 4))
    }

    test("++ should let right-hand side override duplicate keys") {
      val nem1 = NonEmptyMap(("a", 1), ("b", 2))
      val nem2 = NonEmptyMap(("b", 20), ("c", 3))
      val result = nem1 ++ nem2
      result.toList === List(("a", 1), ("b", 20), ("c", 3))
    }

    test("++ should handle single-element maps") {
      val nem1 = NonEmptyMap.one(("a", 1))
      val nem2 = NonEmptyMap.one(("b", 2))
      val result = nem1 ++ nem2
      result.toList === List(("a", 1), ("b", 2))
    }

    test("++ should handle complete overlap") {
      val nem1 = NonEmptyMap(("a", 1), ("b", 2))
      val nem2 = NonEmptyMap(("a", 10), ("b", 20))
      val result = nem1 ++ nem2
      result.toList === List(("a", 10), ("b", 20))
    }
  }

  group("Conversion Methods") {

    test("iterator should iterate over all key-value pairs") {
      val nem = NonEmptyMap(("a", 1), ("b", 2), ("c", 3), ("d", 4), ("e", 5))
      val iterator = nem.iterator
      iterator.toList === List(("a", 1), ("b", 2), ("c", 3), ("d", 4), ("e", 5))
    }

    test("toListMap should convert to ListMap") {
      val nem = NonEmptyMap(("a", 1), ("b", 2), ("c", 3), ("d", 4), ("e", 5))
      val listMap = nem.toListMap
      listMap === ListMap(("a", 1), ("b", 2), ("c", 3), ("d", 4), ("e", 5))
    }

    test("toList should convert to List") {
      val nem = NonEmptyMap(("a", 1), ("b", 2), ("c", 3), ("d", 4), ("e", 5))
      val list = nem.toList
      list === List(("a", 1), ("b", 2), ("c", 3), ("d", 4), ("e", 5))
    }

    test("toVector should convert to Vector") {
      val nem = NonEmptyMap(("a", 1), ("b", 2), ("c", 3), ("d", 4), ("e", 5))
      val vector = nem.toVector
      vector === Vector(("a", 1), ("b", 2), ("c", 3), ("d", 4), ("e", 5))
    }

    test("toNonEmptyList should convert to NonEmptyList") {
      val nem = NonEmptyMap(("a", 1), ("b", 2), ("c", 3), ("d", 4), ("e", 5))
      val nel = nem.toNonEmptyList
      nel.head === ("a", 1)
      nel.tail === List(("b", 2), ("c", 3), ("d", 4), ("e", 5))
    }

    test("toNonEmptyVector should convert to NonEmptyVector") {
      val nem = NonEmptyMap(("a", 1), ("b", 2), ("c", 3), ("d", 4), ("e", 5))
      val nev = nem.toNonEmptyVector
      nev.head === ("a", 1)
      nev.tail === Vector(("b", 2), ("c", 3), ("d", 4), ("e", 5))
    }
  }

  group("String Representation") {

    test("mkString should format with separator") {
      val nem = NonEmptyMap(("a", 1), ("b", 2), ("c", 3), ("d", 4), ("e", 5))
      nem.mkString(",") === "(a,1),(b,2),(c,3),(d,4),(e,5)"
      nem.mkString("|") === "(a,1)|(b,2)|(c,3)|(d,4)|(e,5)"
    }

    test("mkString should format with start, sep, end") {
      val nem = NonEmptyMap(("a", 1), ("b", 2), ("c", 3), ("d", 4), ("e", 5))
      nem.mkString("[", ",", "]") === "[(a,1),(b,2),(c,3),(d,4),(e,5)]"
      nem.mkString("(", "|", ")") === "((a,1)|(b,2)|(c,3)|(d,4)|(e,5))"
    }

    test("toString should format as NonEmptyMap(...)") {
      val nem = NonEmptyMap(("a", 1), ("b", 2), ("c", 3), ("d", 4), ("e", 5))
      nem.toString === "NonEmptyMap((a,1), (b,2), (c,3), (d,4), (e,5))"
    }

    test("toString should handle single element") {
      val nem = NonEmptyMap(("key", 42))
      nem.toString === "NonEmptyMap((key,42))"
    }
  }

  group("Edge Cases and Properties") {

    test("should maintain non-emptiness after operations") {
      val nem = NonEmptyMap(("a", 1), ("b", 2), ("c", 3))

      // Prepend
      val prepended = ("x", 0) +: nem
      assert(prepended.toList.nonEmpty)

      // Append
      val appended = nem :+ ("d", 4)
      assert(appended.toList.nonEmpty)

      // Map
      val mapped = nem.map { case (k, v) => (k.toUpperCase, v * 2) }
      assert(mapped.toList.nonEmpty)

      // FlatMap
      val flatMapped = nem.flatMap { case (k, v) => NonEmptyMap((k, v), (k + k, v * 2)) }
      assert(flatMapped.toList.nonEmpty)
    }

    test("should preserve insertion order in all operations") {
      val original = ListMap(("a", 1), ("b", 2), ("c", 3), ("d", 4), ("e", 5))
      val nem = NonEmptyMap.fromListMap(original).get

      // Prepend
      val prepended = ("x", 0) +: nem
      prepended.toList === (("x", 0) +: original.toList)

      // Append
      val appended = nem :+ ("f", 6)
      appended.toList === (original.toList :+ ("f", 6))

      // Map
      val mapped = nem.map { case (k, v) => (k, v * 2) }
      mapped.toList === original.toList.map { case (k, v) => (k, v * 2) }
    }

    test("should handle single element correctly") {
      val nem = NonEmptyMap.one(("key", 42))

      // Operations on single element
      val prepended = ("x", 0) +: nem
      prepended.toList === List(("x", 0), ("key", 42))

      val appended = nem :+ ("y", 100)
      appended.toList === List(("key", 42), ("y", 100))

      val mapped = nem.map { case (k, v) => (k, v * 2) }
      mapped.toList === List(("key", 84))

      val flatMapped = nem.flatMap { case (k, v) => NonEmptyMap((k, v), (k + k, v * 2)) }
      flatMapped.toList === List(("key", 42), ("keykey", 84))
    }

    test("should handle key replacement correctly") {
      val nem = NonEmptyMap(("a", 1), ("b", 2))

      // Replace existing key with +:
      val prepended = ("a", 10) +: nem
      prepended.toList === List(("a", 10), ("b", 2))

      // Replace existing key with :+
      val appended = nem :+ ("b", 20)
      appended.toList === List(("a", 1), ("b", 20))
    }
  }

  group("Property-based Tests") {

    property("map should preserve length") {
      forAll { (nem: NonEmptyMap[String, Int]) =>
        val mapped = nem.map { case (k, v) => (k.toUpperCase, v * 2) }
        mapped.toList.length == nem.toList.length
      }
    }

    property("flatMap should produce non-empty result") {
      forAll { (nem: NonEmptyMap[String, Int]) =>
        val flatMapped = nem.flatMap { case (k, v) => NonEmptyMap((k, v), (k + k, v * 2)) }
        flatMapped.toList.nonEmpty
      }
    }

    property("conversion methods should preserve elements") {
      forAll { (nem: NonEmptyMap[String, Int]) =>
        val list = nem.toList
        val vector = nem.toVector
        val listMap = nem.toListMap
        val nel = nem.toNonEmptyList
        val nev = nem.toNonEmptyVector

        list == vector.toList &&
        list == listMap.toList &&
        list == nel.toList &&
        list == (nev.head +: nev.tail.toList)
      }
    }

    property("prepend and append should maintain non-emptiness") {
      forAll { (nem: NonEmptyMap[String, Int], key: String, value: Int) =>
        val prepended = (key, value) +: nem
        val appended = nem :+ (key, value)
        prepended.toList.nonEmpty && appended.toList.nonEmpty
      }
    }

    property("++ should produce non-empty result") {
      forAll { (nem1: NonEmptyMap[String, Int], nem2: NonEmptyMap[String, Int]) =>
        val combined = nem1 ++ nem2
        combined.toList.nonEmpty
      }
    }

    property("++ should contain all keys from both maps") {
      forAll { (nem1: NonEmptyMap[String, Int], nem2: NonEmptyMap[String, Int]) =>
        val combined = nem1 ++ nem2
        val combinedKeys = combined.toList.map(_._1).toSet
        val allKeys = nem1.toList.map(_._1).toSet ++ nem2.toList.map(_._1).toSet
        combinedKeys == allKeys
      }
    }

    property("++ should be equivalent to ListMap concatenation") {
      forAll { (nem1: NonEmptyMap[String, Int], nem2: NonEmptyMap[String, Int]) =>
        val combined = nem1 ++ nem2
        combined.toListMap == (nem1.toListMap ++ nem2.toListMap)
      }
    }
  }
}
