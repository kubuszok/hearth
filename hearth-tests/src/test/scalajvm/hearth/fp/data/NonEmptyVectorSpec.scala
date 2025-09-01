package hearth
package fp
package data

import hearth.fp.instances.*
import org.scalacheck.Prop.*

final class NonEmptyVectorSpec extends ScalaCheckSuite with Laws {

  group("Constructor and Factory Methods") {

    test("apply should create NonEmptyVector with head and varargs") {
      val nel = NonEmptyVector(1, 2, 3, 4, 5)
      nel.head === 1
      nel.tail === Vector(2, 3, 4, 5)
      nel.toVector === Vector(1, 2, 3, 4, 5)
    }

    test("apply should create single element NonEmptyVector") {
      val nel = NonEmptyVector(42)
      nel.head === 42
      nel.tail === Vector.empty
      nel.toVector === Vector(42)
    }

    test("fromVector should return Some for non-empty list") {
      val list = Vector(1, 2, 3, 4, 5)
      val result = NonEmptyVector.fromVector(list)
      assert(result.isDefined)
      result.get.head === 1
      result.get.tail === Vector(2, 3, 4, 5)
    }

    test("fromVector should return None for empty list") {
      val result = NonEmptyVector.fromVector(Vector.empty[Int])
      assert(result.isEmpty)
    }

    test("one should create single-element NonEmptyVector") {
      val nel = NonEmptyVector.one(42)
      nel.head === 42
      nel.tail === Vector.empty
      nel.toVector === Vector(42)
    }
  }

  group("Collection Operations") {

    test("+: should prepend element") {
      val nel = NonEmptyVector(2, 3, 4)
      val result = 1 +: nel
      result.head === 1
      result.tail === Vector(2, 3, 4)
      result.toVector === Vector(1, 2, 3, 4)
    }

    test(":+ should append element") {
      val nel = NonEmptyVector(1, 2, 3)
      val result = nel :+ 4
      result.head === 1
      result.tail === Vector(2, 3, 4)
      result.toVector === Vector(1, 2, 3, 4)
    }

    test("++ should concatenate two NonEmptyVectors") {
      val nel1 = NonEmptyVector(1, 2, 3)
      val nel2 = NonEmptyVector(4, 5, 6)
      val result = nel1 ++ nel2
      result.head === 1
      result.tail === Vector(2, 3, 4, 5, 6)
      result.toVector === Vector(1, 2, 3, 4, 5, 6)
    }

    test("map should transform all elements") {
      val nel = NonEmptyVector(1, 2, 3, 4, 5)
      val result = nel.map(_ * 2)
      result.head === 2
      result.tail === Vector(4, 6, 8, 10)
      result.toVector === Vector(2, 4, 6, 8, 10)
    }

    test("flatMap should flatten and transform") {
      val nel = NonEmptyVector(1, 2, 3)
      val result = nel.flatMap(n => NonEmptyVector(n, n * 2))
      result.head === 1
      result.tail === Vector(2, 2, 4, 3, 6)
      result.toVector === Vector(1, 2, 2, 4, 3, 6)
    }

    test("flatMap should handle single element transformation") {
      val nel = NonEmptyVector(1)
      val result = nel.flatMap(n => NonEmptyVector(n * 10))
      result.head === 10
      result.tail === Vector.empty
      result.toVector === Vector(10)
    }
  }

  group("Conversion Methods") {

    test("iterator should iterate over all elements") {
      val nel = NonEmptyVector(1, 2, 3, 4, 5)
      val iterator = nel.iterator
      iterator.toVector === Vector(1, 2, 3, 4, 5)
    }

    test("toVector should convert to Vector") {
      val nel = NonEmptyVector(1, 2, 3, 4, 5)
      val list = nel.toVector
      list === Vector(1, 2, 3, 4, 5)
    }

    test("toList should convert to List") {
      val nel = NonEmptyVector(1, 2, 3, 4, 5)
      val vector = nel.toList
      vector === List(1, 2, 3, 4, 5)
    }

    test("toNonEmptyList should convert to NonEmptyList") {
      val nel = NonEmptyVector(1, 2, 3, 4, 5)
      val nev = nel.toNonEmptyList
      nev.head === 1
      nev.tail === List(2, 3, 4, 5)
    }
  }

  group("String Representation") {

    test("mkString should format with separator") {
      val nel = NonEmptyVector(1, 2, 3, 4, 5)
      nel.mkString(",") === "1,2,3,4,5"
      nel.mkString("|") === "1|2|3|4|5"
    }

    test("mkString should format with start, sep, end") {
      val nel = NonEmptyVector(1, 2, 3, 4, 5)
      nel.mkString("[", ",", "]") === "[1,2,3,4,5]"
      nel.mkString("(", "|", ")") === "(1|2|3|4|5)"
    }

    test("toString should format as NonEmptyVector(...)") {
      val nel = NonEmptyVector(1, 2, 3, 4, 5)
      nel.toString === "NonEmptyVector(1, 2, 3, 4, 5)"
    }

    test("toString should handle single element") {
      val nel = NonEmptyVector(42)
      nel.toString === "NonEmptyVector(42)"
    }
  }

  group("Edge Cases and Properties") {

    test("should maintain non-emptiness after operations") {
      val nel = NonEmptyVector(1, 2, 3)

      // Prepend
      val prepended = 0 +: nel
      assert(prepended.toVector.nonEmpty)

      // Append
      val appended = nel :+ 4
      assert(appended.toVector.nonEmpty)

      // Concatenate
      val concatenated = nel ++ NonEmptyVector(4, 5)
      assert(concatenated.toVector.nonEmpty)

      // Map
      val mapped = nel.map(_ * 2)
      assert(mapped.toVector.nonEmpty)

      // FlatMap
      val flatMapped = nel.flatMap(n => NonEmptyVector(n, n * 2))
      assert(flatMapped.toVector.nonEmpty)
    }

    test("should preserve order in all operations") {
      val original = Vector(1, 2, 3, 4, 5)
      val nel = NonEmptyVector.fromVector(original).get

      // Prepend
      val prepended = 0 +: nel
      prepended.toVector === 0 +: original

      // Append
      val appended = nel :+ 6
      appended.toVector === original :+ 6

      // Map
      val mapped = nel.map(_ * 2)
      mapped.toVector === original.map(_ * 2)
    }

    test("should handle single element correctly") {
      val nel = NonEmptyVector.one(42)

      // Operations on single element
      val prepended = 0 +: nel
      prepended.toVector === Vector(0, 42)

      val appended = nel :+ 100
      appended.toVector === Vector(42, 100)

      val mapped = nel.map(_ * 2)
      mapped.toVector === Vector(84)

      val flatMapped = nel.flatMap(n => NonEmptyVector(n, n * 2))
      flatMapped.toVector === Vector(42, 84)
    }
  }

  group("Property-based Tests") {

    property("map should preserve length") {
      forAll { (nel: NonEmptyVector[Int]) =>
        val mapped = nel.map(_ * 2)
        mapped.toVector.length == nel.toVector.length
      }
    }

    property("flatMap should produce non-empty result") {
      forAll { (nel: NonEmptyVector[Int]) =>
        val flatMapped = nel.flatMap(n => NonEmptyVector(n, n * 2))
        flatMapped.toVector.nonEmpty
      }
    }

    property("conversion methods should preserve elements") {
      forAll { (nel: NonEmptyVector[Int]) =>
        val list = nel.toVector
        val vector = nel.toList
        val nev = nel.toNonEmptyList

        list == vector.toVector &&
        list == (nev.head +: nev.tail.toVector)
      }
    }
  }

  group("Instances for NonEmptyVector") {

    group("should follow Traverse laws") {
      traverseLaws[NonEmptyVector, Option, String]
    }
  }

  group("Instances for Either[NonEmptyVector, *]") {

    group("should follow Functor laws") {
      functorLaws[Either[NonEmptyVector[String], *], Int]
    }

    group("should follow Applicative laws") {
      applicativeLaws[Either[NonEmptyVector[String], *], Int, String, Int]
    }

    group("should follow Parallel laws") {
      parallelLaws[Either[NonEmptyVector[String], *], Int, String]
    }

    group("should follow Traverse laws") {
      traverseLaws[Either[NonEmptyVector[String], *], Option, String]
    }
  }
}
