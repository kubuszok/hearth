package hearth
package fp
package data

import hearth.fp.instances.*
import org.scalacheck.Prop.*

final class NonEmptyListSpec extends ScalaCheckSuite with Laws {

  group("Constructor and Factory Methods") {

    test("apply should create NonEmptyList with head and varargs") {
      val nel = NonEmptyList(1, 2, 3, 4, 5)
      nel.head === 1
      nel.tail === List(2, 3, 4, 5)
      nel.toList === List(1, 2, 3, 4, 5)
    }

    test("apply should create single element NonEmptyList") {
      val nel = NonEmptyList(42)
      nel.head === 42
      nel.tail === List.empty
      nel.toList === List(42)
    }

    test("fromList should return Some for non-empty list") {
      val list = List(1, 2, 3, 4, 5)
      val result = NonEmptyList.fromList(list)
      assert(result.isDefined)
      result.get.head === 1
      result.get.tail === List(2, 3, 4, 5)
    }

    test("fromList should return None for empty list") {
      val result = NonEmptyList.fromList(List.empty[Int])
      assert(result.isEmpty)
    }

    test("one should create single-element NonEmptyList") {
      val nel = NonEmptyList.one(42)
      nel.head === 42
      nel.tail === List.empty
      nel.toList === List(42)
    }
  }

  group("Collection Operations") {

    test("+: should prepend element") {
      val nel = NonEmptyList(2, 3, 4)
      val result = 1 +: nel
      result.head === 1
      result.tail === List(2, 3, 4)
      result.toList === List(1, 2, 3, 4)
    }

    test(":+ should append element") {
      val nel = NonEmptyList(1, 2, 3)
      val result = nel :+ 4
      result.head === 1
      result.tail === List(2, 3, 4)
      result.toList === List(1, 2, 3, 4)
    }

    test("++ should concatenate two NonEmptyLists") {
      val nel1 = NonEmptyList(1, 2, 3)
      val nel2 = NonEmptyList(4, 5, 6)
      val result = nel1 ++ nel2
      result.head === 1
      result.tail === List(2, 3, 4, 5, 6)
      result.toList === List(1, 2, 3, 4, 5, 6)
    }

    test("map should transform all elements") {
      val nel = NonEmptyList(1, 2, 3, 4, 5)
      val result = nel.map(_ * 2)
      result.head === 2
      result.tail === List(4, 6, 8, 10)
      result.toList === List(2, 4, 6, 8, 10)
    }

    test("flatMap should flatten and transform") {
      val nel = NonEmptyList(1, 2, 3)
      val result = nel.flatMap(n => NonEmptyList(n, n * 2))
      result.head === 1
      result.tail === List(2, 2, 4, 3, 6)
      result.toList === List(1, 2, 2, 4, 3, 6)
    }

    test("flatMap should handle single element transformation") {
      val nel = NonEmptyList(1)
      val result = nel.flatMap(n => NonEmptyList(n * 10))
      result.head === 10
      result.tail === List.empty
      result.toList === List(10)
    }
  }

  group("Conversion Methods") {

    test("iterator should iterate over all elements") {
      val nel = NonEmptyList(1, 2, 3, 4, 5)
      val iterator = nel.iterator
      iterator.toList === List(1, 2, 3, 4, 5)
    }

    test("toList should convert to List") {
      val nel = NonEmptyList(1, 2, 3, 4, 5)
      val list = nel.toList
      list === List(1, 2, 3, 4, 5)
    }

    test("toVector should convert to Vector") {
      val nel = NonEmptyList(1, 2, 3, 4, 5)
      val vector = nel.toVector
      vector === Vector(1, 2, 3, 4, 5)
    }

    test("toNonEmptyVector should convert to NonEmptyVector") {
      val nel = NonEmptyList(1, 2, 3, 4, 5)
      val nev = nel.toNonEmptyVector
      nev.head === 1
      nev.tail === Vector(2, 3, 4, 5)
    }
  }

  group("String Representation") {

    test("mkString should format with separator") {
      val nel = NonEmptyList(1, 2, 3, 4, 5)
      nel.mkString(",") === "1,2,3,4,5"
      nel.mkString("|") === "1|2|3|4|5"
    }

    test("mkString should format with start, sep, end") {
      val nel = NonEmptyList(1, 2, 3, 4, 5)
      nel.mkString("[", ",", "]") === "[1,2,3,4,5]"
      nel.mkString("(", "|", ")") === "(1|2|3|4|5)"
    }

    test("toString should format as NonEmptyList(...)") {
      val nel = NonEmptyList(1, 2, 3, 4, 5)
      nel.toString === "NonEmptyList(1, 2, 3, 4, 5)"
    }

    test("toString should handle single element") {
      val nel = NonEmptyList(42)
      nel.toString === "NonEmptyList(42)"
    }
  }

  group("Edge Cases and Properties") {

    test("should maintain non-emptiness after operations") {
      val nel = NonEmptyList(1, 2, 3)

      // Prepend
      val prepended = 0 +: nel
      assert(prepended.toList.nonEmpty)

      // Append
      val appended = nel :+ 4
      assert(appended.toList.nonEmpty)

      // Concatenate
      val concatenated = nel ++ NonEmptyList(4, 5)
      assert(concatenated.toList.nonEmpty)

      // Map
      val mapped = nel.map(_ * 2)
      assert(mapped.toList.nonEmpty)

      // FlatMap
      val flatMapped = nel.flatMap(n => NonEmptyList(n, n * 2))
      assert(flatMapped.toList.nonEmpty)
    }

    test("should preserve order in all operations") {
      val original = List(1, 2, 3, 4, 5)
      val nel = NonEmptyList.fromList(original).get

      // Prepend
      val prepended = 0 +: nel
      prepended.toList === 0 +: original

      // Append
      val appended = nel :+ 6
      appended.toList === original :+ 6

      // Map
      val mapped = nel.map(_ * 2)
      mapped.toList === original.map(_ * 2)
    }

    test("should handle single element correctly") {
      val nel = NonEmptyList.one(42)

      // Operations on single element
      val prepended = 0 +: nel
      prepended.toList === List(0, 42)

      val appended = nel :+ 100
      appended.toList === List(42, 100)

      val mapped = nel.map(_ * 2)
      mapped.toList === List(84)

      val flatMapped = nel.flatMap(n => NonEmptyList(n, n * 2))
      flatMapped.toList === List(42, 84)
    }
  }

  group("Property-based Tests") {

    property("prepend and append should produce different results") {
      forAll { (nel: NonEmptyList[Int], x: Int) =>
        val prepended = x +: nel
        val appended = nel :+ x
        prepended.toList != appended.toList
      }
    }

    property("map should preserve length") {
      forAll { (nel: NonEmptyList[Int]) =>
        val mapped = nel.map(_ * 2)
        mapped.toList.length == nel.toList.length
      }
    }

    property("flatMap should produce non-empty result") {
      forAll { (nel: NonEmptyList[Int]) =>
        val flatMapped = nel.flatMap(n => NonEmptyList(n, n * 2))
        flatMapped.toList.nonEmpty
      }
    }

    property("conversion methods should preserve elements") {
      forAll { (nel: NonEmptyList[Int]) =>
        val list = nel.toList
        val vector = nel.toVector
        val nev = nel.toNonEmptyVector

        list == vector.toList &&
        list == (nev.head +: nev.tail.toList)
      }
    }
  }

  group("Instances for NonEmptyList") {

    group("should follow Traverse laws") {
      traverseLaws[NonEmptyList, Option, String]
    }
  }

  group("Instances for Either[NonEmptyList, *]") {

    group("should follow Functor laws") {
      functorLaws[Either[NonEmptyList[String], *], Int]
    }

    group("should follow Applicative laws") {
      applicativeLaws[Either[NonEmptyList[String], *], Int, String, Int]
    }

    group("should follow Parallel laws") {
      parallelLaws[Either[NonEmptyList[String], *], Int, String]
    }

    group("should follow Traverse laws") {
      traverseLaws[Either[NonEmptyList[String], *], Option, String]
    }
  }
}
