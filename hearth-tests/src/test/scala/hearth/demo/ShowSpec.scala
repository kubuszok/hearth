package hearth.demo

import hearth.MacroSuite

/** Macro implementation of [[Show]] is in [[ShowMacrosImpl]]. */
final class ShowSpec extends MacroSuite {

  group("Show") {

    group("should be able to derive type class") {

      test("for values with built-in support") {
        // import hearth.demo.debug.logDerivation // Uncomment to see how the derivation is done.

        Show.derived[Boolean].show(true) <==> "true"
        Show.derived[Byte].show(1.toByte) <==> "1.toByte"
        Show.derived[Short].show(1.toShort) <==> "1.toShort"
        Show.derived[Int].show(1) <==> "1"
        Show.derived[Long].show(1L) <==> "1L"
        Show.derived[Float].show(1.0f) <==> "1.0f"
        Show.derived[Double].show(1.0) <==> "1.0"
        Show.derived[Char].show('a') <==> "'a'"
        Show.derived[String].show("hello") <==> "\"hello\""
      }

      test("for values with iterable support") {
        // import hearth.demo.debug.logDerivation // Uncomment to see how the derivation is done.

        Show.derived[Iterable[Int]].show(List(1, 2, 3)) <==> "List(1, 2, 3)"
      }

      test("values with case class support") {
        // import hearth.demo.debug.logDerivation // Uncomment to see how the derivation is done.

        case class Person(name: String, age: Int)
        Show.show(Person("John", 30)) <==> "Person(name = \"John\", age = 30)"
      }

      test("values with enum support") {
        // import hearth.demo.debug.logDerivation // Uncomment to see how the derivation is done.

        sealed trait Color
        case object Red extends Color
        case object Green extends Color
        case object Blue extends Color

        def impl(color: Color): String = Show.show(color)
        impl(Red) <==> "Red"
        impl(Green) <==> "Green"
        impl(Blue) <==> "Blue"
      }
    }

    group("should be able to inline showing for") {

      test("values with built-in support") {
        // import hearth.demo.debug.logDerivation // Uncomment to see how the derivation is done.

        Show.show(true) <==> "true"
        Show.show(1.toByte) <==> "1.toByte"
        Show.show(1.toShort) <==> "1.toShort"
        Show.show(1) <==> "1"
        Show.show(1L) <==> "1L"
        Show.show(1.0f) <==> "1.0f"
        Show.show(1.0) <==> "1.0"
        Show.show('a') <==> "'a'"
        Show.show("hello") <==> "\"hello\""
      }

      test("for values with iterable support") {
        // import hearth.demo.debug.logDerivation // Uncomment to see how the derivation is done.

        Show.show(List(1, 2, 3)) <==> "List(1, 2, 3)"
      }

      test("values with case class support") {
        // import hearth.demo.debug.logDerivation // Uncomment to see how the derivation is done.

        case class Person(name: String, age: Int)
        Show.derived[Person].show(Person("John", 30)) <==> "Person(name = \"John\", age = 30)"
      }

      test("values with enum support") {
        // import hearth.demo.debug.logDerivation // Uncomment to see how the derivation is done.

        sealed trait Color
        case object Red extends Color
        case object Green extends Color
        case object Blue extends Color

        Show.derived[Color].show(Red) <==> "Red"
        Show.derived[Color].show(Green) <==> "Green"
        Show.derived[Color].show(Blue) <==> "Blue"
      }
    }
  }
}
