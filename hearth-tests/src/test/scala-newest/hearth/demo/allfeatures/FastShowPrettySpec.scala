package hearth.demo.allfeatures

import munit.FunSuite

case class Person(name: String, age: Int)
case class Empty()
case class Single(value: Int)
case class Address(street: String, city: String)
case class PersonWithAddress(name: String, age: Int, address: Address)

@scala.annotation.nowarn // TODO: unused values - we cannot suppress them with val _ = ... until we fix cross-quotes on Scala 2!!!
final class FastShowPrettySpec extends FunSuite {

  // ============================================================================
  // TEST PLAN FOR FastShowPretty
  // ============================================================================
  //
  // FastShowPretty is a type class that provides:
  // 1. Inline `render[A](value: A): String` - macro that inlines rendering logic
  // 2. `derived[A]: FastShowPretty[A]` - macro that derives type class instances
  // 3. Built-in support for primitives: Boolean, Byte, Short, Int, Long, Float, Double, Char, String
  // 4. Automatic derivation for case classes (with pretty formatting)
  // 5. Automatic derivation for enums (Scala 3)
  // 6. Support for implicit instances
  //
  // ============================================================================
  // TEST CATEGORIES TO IMPLEMENT:
  // ============================================================================

  // ----------------------------------------------------------------------------
  // 1. PRIMITIVE TYPES - Testing inline render() method
  // ----------------------------------------------------------------------------
  // Test that all built-in primitives render correctly:
  // - Boolean: true/false
  // - Byte: value.toByte
  // - Short: value.toShort
  // - Int: value (no suffix)
  // - Long: valueL
  // - Float: value.0f (with workaround for Scala.js)
  // - Double: value.0d (with workaround for Scala.js)
  // - Char: 'c'
  // - String: "text" (with escaped quotes and newlines)

  test("render - Boolean true") {
    val result = FastShowPretty.render(true, RenderConfig.Default)
    assertEquals(result, "true")
  }

  test("render - Boolean false") {
    val result = FastShowPretty.render(false, RenderConfig.Default)
    assertEquals(result, "false")
  }

  test("render - Byte") {
    val result = FastShowPretty.render(42.toByte, RenderConfig.Default)
    assertEquals(result, "42.toByte")
  }

  test("render - Short") {
    val result = FastShowPretty.render(42.toShort, RenderConfig.Default)
    assertEquals(result, "42.toShort")
  }

  test("render - Int") {
    val result = FastShowPretty.render(42, RenderConfig.Default)
    assertEquals(result, "42")
  }

  test("render - Long") {
    val result = FastShowPretty.render(42L, RenderConfig.Default)
    assertEquals(result, "42L")
  }

  test("render - Float") {
    val result = FastShowPretty.render(42.5f, RenderConfig.Default)
    assertEquals(result, "42.5.0f")
  }

  test("render - Double") {
    val result = FastShowPretty.render(42.5, RenderConfig.Default)
    assertEquals(result, "42.5.0d")
  }

  test("render - Char") {
    val result = FastShowPretty.render('a', RenderConfig.Default)
    assertEquals(result, "'a'")
  }

  test("render - String") {
    val result = FastShowPretty.render("hello", RenderConfig.Default)
    assertEquals(result, "\"hello\"")
  }

  test("render - String with quotes") {
    val result = FastShowPretty.render("say \"hello\"", RenderConfig.Default)
    assertEquals(result, "\"say \\\"hello\\\"\"")
  }

  test("render - String with newlines") {
    val result = FastShowPretty.render("line1\nline2", RenderConfig.Default)
    assertEquals(result, "\"line1\\nline2\"")
  }

  // ----------------------------------------------------------------------------
  // 2. CASE CLASSES - Testing automatic derivation
  // ----------------------------------------------------------------------------
  // Test that case classes are derived automatically with pretty formatting:
  // - Empty case class: Name()
  // - Single field: Name(field = value)
  // - Multiple fields: Name(\n  field1 = value1,\n  field2 = value2\n)
  // - Nested case classes
  // - Case classes with primitive fields
  // - Case classes with Option fields (if supported)
  // - Case classes with collections (if supported)

  test("render - compact (no indent)") {
    val result = FastShowPretty.render(Person("Alice", 30), RenderConfig.Compact)
    assertEquals(result, "Person(\nname = \"Alice\",\nage = 30\n)")
  }

  test("render - tabs") {
    val result = FastShowPretty.render(Person("Alice", 30), RenderConfig.Tabs)
    assertEquals(result, "Person(\n\tname = \"Alice\",\n\tage = 30\n)")
  }

  test("render - four spaces") {
    val result = FastShowPretty.render(Person("Alice", 30), RenderConfig.FourSpaces)
    assertEquals(result, "Person(\n    name = \"Alice\",\n    age = 30\n)")
  }

  test("render - nested with tabs") {
    val address = Address("123 Main St", "New York")
    val person = PersonWithAddress("Bob", 25, address)
    val result = FastShowPretty.render(person, RenderConfig.Tabs)
    assertEquals(
      result,
      "PersonWithAddress(\n\tname = \"Bob\",\n\tage = 25,\n\taddress = Address(\n\t\tstreet = \"123 Main St\",\n\t\tcity = \"New York\"\n\t)\n)"
    )
  }

  // ----------------------------------------------------------------------------
  // 3. ENUMS (Scala 3) - Testing automatic derivation
  // ----------------------------------------------------------------------------
  // Test that Scala 3 enums are derived automatically:
  // - Simple enum: (value): EnumName
  // - Enum with parameters: (value): EnumName
  // - Nested enum cases

  // Note: This will only compile in Scala 3
  // enum Color {
  //   case Red, Green, Blue
  //   case RGB(r: Int, g: Int, b: Int)
  // }

  // test("render - simple enum") {
  //   val result = FastShowPretty.render(Color.Red)
  //   assertEquals(result, "(Red): Color")
  // }

  // test("render - enum with parameters") {
  //   val result = FastShowPretty.render(Color.RGB(255, 128, 0))
  //   assertEquals(result, "(RGB(\nr = 255,\ng = 128,\nb = 0\n)): Color")
  // }

  // ----------------------------------------------------------------------------
  // 4. TYPE CLASS INSTANCE - Testing derived type class
  // ----------------------------------------------------------------------------
  // Test that derived instances work correctly:
  // - Can derive instance for primitives
  // - Can derive instance for case classes
  // - Instance can be used with render method
  // - Instance can be used directly with StringBuilder

  test("derived - Int instance") {
    val instance = implicitly[FastShowPretty[Int]]
    val sb = new StringBuilder
    val result = instance.render(sb, RenderConfig.Default, 0)(42).toString
    assertEquals(result, "42")
  }

  test("derived - case class instance") {
    val instance = implicitly[FastShowPretty[Person]]
    val sb = new StringBuilder
    val result = instance.render(sb, RenderConfig.Default, 0)(Person("Alice", 30)).toString
    assertEquals(result, "Person(\n  name = \"Alice\",\n  age = 30\n)")
  }

  test("derived - instance reuse StringBuilder") {
    val instance = implicitly[FastShowPretty[Int]]
    val sb = new StringBuilder("prefix: ")
    val result = instance.render(sb, RenderConfig.Default, 0)(42).toString
    assertEquals(result, "prefix: 42")
  }

  test("derived - instance with custom config") {
    val instance = implicitly[FastShowPretty[Person]]
    val sb = new StringBuilder
    val result = instance.render(sb, RenderConfig.Tabs, 0)(Person("Alice", 30)).toString
    assertEquals(result, "Person(\n\tname = \"Alice\",\n\tage = 30\n)")
  }

  test("derived - instance with start level") {
    val instance = implicitly[FastShowPretty[Person]]
    val sb = new StringBuilder
    // Starting at level 1 means the first level of fields is at level 2
    val result = instance.render(sb, RenderConfig.Default, 1)(Person("Alice", 30)).toString
    assertEquals(result, "Person(\n    name = \"Alice\",\n    age = 30\n  )")
  }

  // ----------------------------------------------------------------------------
  // 5. IMPLICIT INSTANCES - Testing custom instances
  // ----------------------------------------------------------------------------
  // Test that custom implicit instances take precedence:
  // - Custom instance for a type
  // - Custom instance overrides derivation
  // - Custom instance can be used with render

  test("render - uses custom implicit instance") {
    implicit val customIntInstance: FastShowPretty[Int] = new FastShowPretty[Int] {
      def render(sb: StringBuilder, config: RenderConfig, level: Int)(value: Int): StringBuilder =
        sb.append("custom(").append(value).append(")")
    }

    val result = FastShowPretty.render(42, RenderConfig.Default)
    assertEquals(result, "custom(42)")
  }

  // ----------------------------------------------------------------------------
  // 6. EDGE CASES
  // ----------------------------------------------------------------------------
  // Test edge cases:
  // - Zero values
  // - Negative numbers
  // - Max/Min values for numeric types
  // - Empty strings
  // - Special characters in strings
  // - Unicode characters
  // - Very long strings
  // - Deeply nested structures
  // - Recursive structures (if supported)

  test("render - zero values") {
    assertEquals(FastShowPretty.render(0, RenderConfig.Default), "0")
    assertEquals(FastShowPretty.render(0L, RenderConfig.Default), "0L")
    assertEquals(FastShowPretty.render(0.0f, RenderConfig.Default), "0.0.0f")
    assertEquals(FastShowPretty.render(0.0, RenderConfig.Default), "0.0.0d")
  }

  test("render - negative numbers") {
    assertEquals(FastShowPretty.render(-42, RenderConfig.Default), "-42")
    assertEquals(FastShowPretty.render(-42L, RenderConfig.Default), "-42L")
  }

  test("render - empty string") {
    assertEquals(FastShowPretty.render("", RenderConfig.Default), "\"\"")
  }

  test("render - unicode characters") {
    val result = FastShowPretty.render("Hello 世界", RenderConfig.Default)
    assertEquals(result, "\"Hello 世界\"")
  }

  test("render - special characters in string") {
    val result = FastShowPretty.render("tab\tquote\"newline\n", RenderConfig.Default)
    assertEquals(result, "\"tab\\tquote\\\"newline\\n\"")
  }

  test("render - backslash in string") {
    val result = FastShowPretty.render("path\\to\\file", RenderConfig.Default)
    assertEquals(result, "\"path\\\\to\\\\file\"")
  }

  // ----------------------------------------------------------------------------
  // 7. COMPILE-TIME VERIFICATION
  // ----------------------------------------------------------------------------
  // Test that macros work at compile time:
  // - render() should inline (verify by checking it compiles)
  // - derived should work at compile time
  // - Type errors for unsupported types (if applicable)

  test("compile-time - render compiles for supported types") {
    // This test verifies that render compiles for various types
    val _: String = FastShowPretty.render(42, RenderConfig.Default)
    val _: String = FastShowPretty.render("test", RenderConfig.Default)
    val _: String = FastShowPretty.render(Person("Alice", 30), RenderConfig.Default)
    // If this compiles, the test passes
    assert(true)
  }

  test("compile-time - derived compiles for supported types") {
    import FastShowPretty.derived
    val _: FastShowPretty[Int] = implicitly[FastShowPretty[Int]]
    val _: FastShowPretty[Person] = implicitly[FastShowPretty[Person]]
    // If this compiles, the test passes
    assert(true)
  }

  // ----------------------------------------------------------------------------
  // 8. PERFORMANCE / STRINGBUILDER REUSE
  // ----------------------------------------------------------------------------
  // Test StringBuilder reuse behavior:
  // - Multiple renders append to same StringBuilder
  // - StringBuilder state is preserved

  test("StringBuilder - multiple appends") {
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

  // ----------------------------------------------------------------------------
  // 9. PROPERTY-BASED TESTS (using ScalaCheck)
  // ----------------------------------------------------------------------------
  // Use ScalaCheck to test properties:
  // - render is idempotent (render(render(x)) might not make sense, but we can test other properties)
  // - render never throws exceptions for valid inputs
  // - render produces non-empty strings for non-empty inputs (where applicable)

  // Example property-based test:
  // test("property - render never throws for Int") {
  //   forAll { (n: Int) =>
  //     noException should be thrownBy FastShowPretty.render(n)
  //   }
  // }

  // test("property - render produces valid output for Int") {
  //   forAll { (n: Int) =>
  //     val result = FastShowPretty.render(n)
  //     assert(result.nonEmpty)
  //   }
  // }

  // ----------------------------------------------------------------------------
  // 10. ERROR CASES (if applicable)
  // ----------------------------------------------------------------------------
  // Test error handling:
  // - Unsupported types (should compile-time error or runtime error?)
  // - Null values (if applicable)
  // - Circular references in case classes (if applicable)

  // Note: Based on the implementation, unsupported types should fail at compile time
  // with a macro error, so these might not be testable at runtime.

  // ----------------------------------------------------------------------------
  // 11. COLLECTIONS - Testing IsCollection support
  // ----------------------------------------------------------------------------
  // Test that collections render correctly when item types have implicit instances.

  // FIXME: these are broken

  test("render - List of Ints") {
    val result = FastShowPretty.render(List(1, 2, 3), RenderConfig.Default)
    assertEquals(result, "List(1, 2, 3)")
  }

  // test("render - empty List") {
  //   val result = FastShowPretty.render(List.empty[Int], RenderConfig.Default)
  //   assertEquals(result, "List()")
  // }

  // test("render - Vector of Strings") {
  //   val result = FastShowPretty.render(Vector("a", "b", "c"), RenderConfig.Default)
  //   assertEquals(result, "Vector(\"a\", \"b\", \"c\")")
  // }

  // test("render - Set of Ints") {
  //   val result = FastShowPretty.render(Set(1), RenderConfig.Default)
  //   assertEquals(result, "Set(1)")
  // }

  // ----------------------------------------------------------------------------
  // 12. MAPS - Testing map support (via collection of tuples)
  // ----------------------------------------------------------------------------
  // Note: Maps are handled as collections of tuples. The tuple rendering requires
  // special handling which will be added in a future enhancement.
  // For now, map tests are disabled.

  // FIXME: these are broken

  // test("render - Map of String to Int") {
  //   val result = FastShowPretty.render(Map("a" -> 1))
  //   assertEquals(result, "Map((\"a\", 1))")  // Maps render as collections of tuples
  // }

  // test("render - empty Map") {
  //   val result = FastShowPretty.render(Map.empty[String, Int])
  //   assertEquals(result, "Map()")
  // }

  // test("render - Map with multiple entries") {
  //   val result = FastShowPretty.render(scala.collection.immutable.ListMap("x" -> 10, "y" -> 20))
  //   assertEquals(result, "ListMap((\"x\", 10), (\"y\", 20))")
  // }

}
