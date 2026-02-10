package hearth.demo.allfeatures

import hearth.MacroSuite

case class Person(name: String, age: Int)
case class Empty()
case class Single(value: Int)
case class Address(street: String, city: String)
case class PersonWithAddress(name: String, age: Int, address: Address)
case class Team(name: String, members: List[Person])

@scala.annotation.nowarn // TODO: unused values - we cannot suppress them with val _ = ... until we fix cross-quotes on Scala 2!!!
final class FastShowPrettySpec extends MacroSuite {

  group("FastShowPretty") {

    group("render") {

      group("primitive types") {

        test("Boolean true") {
          // import hearth.demo.sanely_automatic.debug.logDerivation // Uncomment to see how the derivation is done. is done.
          val result = FastShowPretty.render(true, RenderConfig.Default)
          assertEquals(result, "true")
        }

        test("Boolean false") {
          // import hearth.demo.sanely_automatic.debug.logDerivation // Uncomment to see how the derivation is done. is done.
          val result = FastShowPretty.render(false, RenderConfig.Default)
          assertEquals(result, "false")
        }

        test("Byte") {
          // import hearth.demo.sanely_automatic.debug.logDerivation // Uncomment to see how the derivation is done. is done.
          val result = FastShowPretty.render(42.toByte, RenderConfig.Default)
          assertEquals(result, "42.toByte")
        }

        test("Short") {
          // import hearth.demo.sanely_automatic.debug.logDerivation // Uncomment to see how the derivation is done. is done.
          val result = FastShowPretty.render(42.toShort, RenderConfig.Default)
          assertEquals(result, "42.toShort")
        }

        test("Int") {
          // import hearth.demo.sanely_automatic.debug.logDerivation // Uncomment to see how the derivation is done. is done.
          val result = FastShowPretty.render(42, RenderConfig.Default)
          assertEquals(result, "42")
        }

        test("Long") {
          // import hearth.demo.sanely_automatic.debug.logDerivation // Uncomment to see how the derivation is done. is done.
          val result = FastShowPretty.render(42L, RenderConfig.Default)
          assertEquals(result, "42L")
        }

        test("Float") {
          // import hearth.demo.sanely_automatic.debug.logDerivation // Uncomment to see how the derivation is done. is done.
          val result = FastShowPretty.render(42.5f, RenderConfig.Default)
          assertEquals(result, "42.5f")
        }

        test("Double") {
          // import hearth.demo.sanely_automatic.debug.logDerivation // Uncomment to see how the derivation is done. is done.
          val result = FastShowPretty.render(42.5, RenderConfig.Default)
          assertEquals(result, "42.5d")
        }

        test("Char") {
          // import hearth.demo.sanely_automatic.debug.logDerivation // Uncomment to see how the derivation is done. is done.
          val result = FastShowPretty.render('a', RenderConfig.Default)
          assertEquals(result, "'a'")
        }

        test("String") {
          // import hearth.demo.sanely_automatic.debug.logDerivation // Uncomment to see how the derivation is done. is done.
          val result = FastShowPretty.render("hello", RenderConfig.Default)
          assertEquals(result, "\"hello\"")
        }

        test("String with quotes") {
          // import hearth.demo.sanely_automatic.debug.logDerivation // Uncomment to see how the derivation is done. is done.
          val result = FastShowPretty.render("say \"hello\"", RenderConfig.Default)
          assertEquals(result, "\"say \\\"hello\\\"\"")
        }

        test("String with newlines") {
          // import hearth.demo.sanely_automatic.debug.logDerivation // Uncomment to see how the derivation is done. is done.
          val result = FastShowPretty.render("line1\nline2", RenderConfig.Default)
          assertEquals(result, "\"line1\\nline2\"")
        }
      }

      group("case classes") {

        test("compact (no indent)") {
          // import hearth.demo.sanely_automatic.debug.logDerivation // Uncomment to see how the derivation is done. is done.
          val result = FastShowPretty.render(Person("Alice", 30), RenderConfig.Compact)
          assertEquals(
            result,
            """Person(
              |name = "Alice",
              |age = 30
              |)""".stripMargin
          )
        }

        test("tabs") {
          // import hearth.demo.sanely_automatic.debug.logDerivation // Uncomment to see how the derivation is done. is done.
          val result = FastShowPretty.render(Person("Alice", 30), RenderConfig.Tabs)
          assertEquals(
            result,
            s"""Person(
               |\tname = "Alice",
               |\tage = 30
               |)""".stripMargin
          )
        }

        test("four spaces") {
          // import hearth.demo.sanely_automatic.debug.logDerivation // Uncomment to see how the derivation is done. is done.
          val result = FastShowPretty.render(Person("Alice", 30), RenderConfig.FourSpaces)
          assertEquals(
            result,
            """Person(
              |    name = "Alice",
              |    age = 30
              |)""".stripMargin
          )
        }

        test("nested with tabs") {
          // import hearth.demo.sanely_automatic.debug.logDerivation // Uncomment to see how the derivation is done. is done.
          val address = Address("123 Main St", "New York")
          val person = PersonWithAddress("Bob", 25, address)
          val result = FastShowPretty.render(person, RenderConfig.Tabs)
          assertEquals(
            result,
            s"""PersonWithAddress(
               |\tname = "Bob",
               |\tage = 25,
               |\taddress = Address(
               |\t\tstreet = "123 Main St",
               |\t\tcity = "New York"
               |\t)
               |)""".stripMargin
          )
        }

        test("case class with collection field") {
          // import hearth.demo.sanely_automatic.debug.logDerivation // Uncomment to see how the derivation is done. is done.
          val result = FastShowPretty.render(
            Team("Engineering", List(Person("Alice", 30), Person("Bob", 25))),
            RenderConfig.Default
          )
          assertEquals(
            result,
            """Team(
              |  name = "Engineering",
              |  members = List(
              |    Person(
              |      name = "Alice",
              |      age = 30
              |    ),
              |    Person(
              |      name = "Bob",
              |      age = 25
              |    )
              |  )
              |)""".stripMargin
          )
        }
      }

      group("collections") {

        test("List of Ints") {
          // import hearth.demo.sanely_automatic.debug.logDerivation // Uncomment to see how the derivation is done. is done.
          val result = FastShowPretty.render(List(1, 2, 3), RenderConfig.Default)
          assertEquals(
            result,
            """List(
              |  1,
              |  2,
              |  3
              |)""".stripMargin
          )
        }

        test("empty List") {
          // import hearth.demo.sanely_automatic.debug.logDerivation // Uncomment to see how the derivation is done. is done.
          val result = FastShowPretty.render(List.empty[Int], RenderConfig.Default)
          assertEquals(result, "List()")
        }

        test("Vector of Strings") {
          // import hearth.demo.sanely_automatic.debug.logDerivation // Uncomment to see how the derivation is done. is done.
          val result = FastShowPretty.render(Vector("a", "b", "c"), RenderConfig.Default)
          assertEquals(
            result,
            """Vector(
              |  "a",
              |  "b",
              |  "c"
              |)""".stripMargin
          )
        }

        test("Set of Ints") {
          // import hearth.demo.sanely_automatic.debug.logDerivation // Uncomment to see how the derivation is done. is done.
          val result = FastShowPretty.render(Set(1), RenderConfig.Default)
          assertEquals(
            result,
            """Set(
              |  1
              |)""".stripMargin
          )
        }

        test("List of case classes") {
          // import hearth.demo.sanely_automatic.debug.logDerivation // Uncomment to see how the derivation is done. is done.
          val result = FastShowPretty.render(
            List(PersonWithAddress("Bob", 25, Address("123 Main St", "New York"))),
            RenderConfig.Default
          )
          assertEquals(
            result,
            """List(
              |  PersonWithAddress(
              |    name = "Bob",
              |    age = 25,
              |    address = Address(
              |      street = "123 Main St",
              |      city = "New York"
              |    )
              |  )
              |)""".stripMargin
          )
        }
      }

      group("maps") {

        test("Map of String to Int") {
          // import hearth.demo.sanely_automatic.debug.logDerivation // Uncomment to see how the derivation is done. is done.
          val result = FastShowPretty.render(Map("a" -> 1), RenderConfig.Default)
          assertEquals(
            result,
            """Map(
              |  ("a", 1)
              |)""".stripMargin
          )
        }

        test("empty Map") {
          // import hearth.demo.sanely_automatic.debug.logDerivation // Uncomment to see how the derivation is done. is done.
          val result = FastShowPretty.render(Map.empty[String, Int], RenderConfig.Default)
          assertEquals(result, "Map()")
        }

        test("Map with multiple entries") {
          // import hearth.demo.sanely_automatic.debug.logDerivation // Uncomment to see how the derivation is done. is done.
          val result =
            FastShowPretty.render(scala.collection.immutable.ListMap("x" -> 10, "y" -> 20), RenderConfig.Default)
          assertEquals(
            result,
            """ListMap(
              |  ("x", 10),
              |  ("y", 20)
              |)""".stripMargin
          )
        }
      }

      group("custom implicit instances") {

        test("uses custom implicit instance") {
          // import hearth.demo.sanely_automatic.debug.logDerivation // Uncomment to see how the derivation is done. is done.
          implicit val customIntInstance: FastShowPretty[Int] = new FastShowPretty[Int] {
            def render(sb: StringBuilder, config: RenderConfig, level: Int)(value: Int): StringBuilder =
              sb.append("custom(").append(value).append(")")
          }

          val result = FastShowPretty.render(42, RenderConfig.Default)
          assertEquals(result, "custom(42)")
        }
      }

      group("edge cases") {

        test("zero values") {
          // import hearth.demo.sanely_automatic.debug.logDerivation // Uncomment to see how the derivation is done. is done.
          assertEquals(FastShowPretty.render(0, RenderConfig.Default), "0")
          assertEquals(FastShowPretty.render(0L, RenderConfig.Default), "0L")
          assertEquals(FastShowPretty.render(0.0f, RenderConfig.Default), "0.0f")
          assertEquals(FastShowPretty.render(0.0, RenderConfig.Default), "0.0d")
        }

        test("negative numbers") {
          // import hearth.demo.sanely_automatic.debug.logDerivation // Uncomment to see how the derivation is done. is done.
          assertEquals(FastShowPretty.render(-42, RenderConfig.Default), "-42")
          assertEquals(FastShowPretty.render(-42L, RenderConfig.Default), "-42L")
        }

        test("empty string") {
          // import hearth.demo.sanely_automatic.debug.logDerivation // Uncomment to see how the derivation is done. is done.
          assertEquals(FastShowPretty.render("", RenderConfig.Default), "\"\"")
        }

        test("unicode characters") {
          // import hearth.demo.sanely_automatic.debug.logDerivation // Uncomment to see how the derivation is done. is done.
          val result = FastShowPretty.render("Hello 世界", RenderConfig.Default)
          assertEquals(result, "\"Hello 世界\"")
        }

        test("special characters in string") {
          // import hearth.demo.sanely_automatic.debug.logDerivation // Uncomment to see how the derivation is done. is done.
          val result = FastShowPretty.render("tab\tquote\"newline\n", RenderConfig.Default)
          assertEquals(result, "\"tab\\tquote\\\"newline\\n\"")
        }

        test("backslash in string") {
          // import hearth.demo.sanely_automatic.debug.logDerivation // Uncomment to see how the derivation is done. is done.
          val result = FastShowPretty.render("path\\to\\file", RenderConfig.Default)
          assertEquals(result, "\"path\\\\to\\\\file\"")
        }
      }
    }

    group("derived") {

      test("Int instance") {
        // import hearth.demo.sanely_automatic.debug.logDerivation // Uncomment to see how the derivation is done. is done.
        val instance = implicitly[FastShowPretty[Int]]
        val sb = new StringBuilder
        val result = instance.render(sb, RenderConfig.Default, 0)(42).toString
        assertEquals(result, "42")
      }

      test("case class instance") {
        // import hearth.demo.sanely_automatic.debug.logDerivation // Uncomment to see how the derivation is done. is done.
        val instance = implicitly[FastShowPretty[Person]]
        val sb = new StringBuilder
        val result = instance.render(sb, RenderConfig.Default, 0)(Person("Alice", 30)).toString
        assertEquals(
          result,
          """Person(
            |  name = "Alice",
            |  age = 30
            |)""".stripMargin
        )
      }

      test("instance reuse StringBuilder") {
        // import hearth.demo.sanely_automatic.debug.logDerivation // Uncomment to see how the derivation is done. is done.
        val instance = implicitly[FastShowPretty[Int]]
        val sb = new StringBuilder("prefix: ")
        val result = instance.render(sb, RenderConfig.Default, 0)(42).toString
        assertEquals(result, "prefix: 42")
      }

      test("instance with custom config") {
        // import hearth.demo.sanely_automatic.debug.logDerivation // Uncomment to see how the derivation is done. is done.
        val instance = implicitly[FastShowPretty[Person]]
        val sb = new StringBuilder
        val result = instance.render(sb, RenderConfig.Tabs, 0)(Person("Alice", 30)).toString
        assertEquals(
          result,
          s"""Person(
             |\tname = "Alice",
             |\tage = 30
             |)""".stripMargin
        )
      }

      test("instance with start level") {
        // import hearth.demo.sanely_automatic.debug.logDerivation // Uncomment to see how the derivation is done. is done.
        val instance = implicitly[FastShowPretty[Person]]
        val sb = new StringBuilder
        val result = instance.render(sb, RenderConfig.Default, 1)(Person("Alice", 30)).toString
        assertEquals(
          result,
          """Person(
            |    name = "Alice",
            |    age = 30
            |  )""".stripMargin
        )
      }
    }

    group("compile-time") {

      test("render compiles for supported types") {
        // import hearth.demo.sanely_automatic.debug.logDerivation // Uncomment to see how the derivation is done. is done.
        val _: String = FastShowPretty.render(42, RenderConfig.Default)
        val _: String = FastShowPretty.render("test", RenderConfig.Default)
        val _: String = FastShowPretty.render(Person("Alice", 30), RenderConfig.Default)
        assert(true)
      }

      test("derived compiles for supported types") {
        // import hearth.demo.sanely_automatic.debug.logDerivation // Uncomment to see how the derivation is done. is done.
        import FastShowPretty.derived
        val _: FastShowPretty[Int] = implicitly[FastShowPretty[Int]]
        val _: FastShowPretty[Person] = implicitly[FastShowPretty[Person]]
        assert(true)
      }
    }

    group("StringBuilder reuse") {

      test("multiple appends") {
        // import hearth.demo.sanely_automatic.debug.logDerivation // Uncomment to see how the derivation is done. is done.
        import FastShowPretty.derived
        val instance = implicitly[FastShowPretty[Int]]
        val sb = new StringBuilder("start: ")
        instance.render(sb, RenderConfig.Default, 0)(1)
        sb.append(", ")
        instance.render(sb, RenderConfig.Default, 0)(2)
        sb.append(", ")
        instance.render(sb, RenderConfig.Default, 0)(3)
        assertEquals(sb.toString, "start: 1, 2, 3")
      }
    }
  }
}
