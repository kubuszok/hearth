package hearth.demo

import hearth.MacroSuite

/** Macro implementation is in [[ShowMacrosImpl]] */
class ShowSpec extends MacroSuite {

  group("Show") {

    group("should be able to derive type class") {

      test("for values with built-in support") {

        Show.derived[Int].show(1) <==> "1"
        Show.derived[String].show("hello") <==> "\"hello\""
        Show.derived[Boolean].show(true) <==> "true"
      }
    }

    group("should be able to inline showing for") {

      test("values with built-in support") {

        Show.show(1) <==> "1"
        Show.show("hello") <==> "\"hello\""
        Show.show(true) <==> "true"
      }
    }
  }
}
