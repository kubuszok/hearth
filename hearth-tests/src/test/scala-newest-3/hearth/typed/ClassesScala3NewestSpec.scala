package hearth
package typed

import hearth.data.Data

/** NamedTuple class view tests — only compiled on Scala 3.7+ (scala-newest-3 source set).
  *
  * Macro implementation is in [[ClassesFixturesImpl]]
  */
final class ClassesScala3NewestSpec extends MacroSuite {

  group("trait typed.Classes") {

    group("class: Class[A], returns preprocessed class") {
      import ClassesFixtures.testClass

      test("for named tuple (Scala 3.7+)") {

        testClass[(name: String, age: Int)](
          "clone",
          "equals",
          "finalize",
          "getClass",
          "hashCode",
          "notify",
          "notifyAll",
          "toString",
          "wait",
          "asInstanceOf",
          "isInstanceOf",
          "synchronized",
          "==",
          "!=",
          "eq",
          "ne",
          "##",
          // Tuple-specific methods
          "*:",
          "++",
          ":*",
          "apply",
          "head",
          "init",
          "last",
          "tail",
          "toList",
          "toArray",
          "toIArray",
          "size",
          "drop",
          "take",
          "splitAt",
          "zip",
          "map",
          "reverse",
          // Product methods
          "productArity",
          "productElement",
          "productElementName",
          "productElementNames",
          "productIterator",
          "productPrefix",
          "canEqual",
          // _1/_2 from Tuple2
          "_1",
          "_2",
          "swap",
          "copy"
        ) <==> Data.map(
          "commons" -> Data.map(
            "constructors" -> Data.list(Data("(name: java.lang.String, age: scala.Int)")),
            "methods" -> Data.list() // all excluded above
          ),
          "asSingleton" -> Data("<no singleton>"),
          "asNamedTuple" -> Data.map(
            "fields" -> Data("(name: java.lang.String, age: scala.Int)")
          ),
          "asCaseClass" -> Data("<no case class>"),
          "asEnum" -> Data("<no enum>"),
          "asJavaBean" -> Data("<no java bean>")
        )
      }
    }

    test("NamedTuple[A].{construct and fields} should construct a named tuple and expose fields") {
      import ClassesFixtures.testNamedTupleConstructAndFields

      val result = testNamedTupleConstructAndFields[(name: String, age: Int)]
      assert(result.contains("fields: (name: java.lang.String, age: scala.Int)"), s"Unexpected fields: $result")
      assert(result.contains("construct:"), s"Expected construct result: $result")
      assert(!result.contains("<no named tuple>"), s"Expected named tuple to be parsed: $result")
      assert(!result.contains("<failed"), s"Expected successful construction: $result")
    }

    test("NamedTuple[A].{construct and fields} should return <no named tuple> for non-named-tuple types") {
      import ClassesFixtures.testNamedTupleConstructAndFields

      testNamedTupleConstructAndFields[String] <==> "<no named tuple>"
    }
  }
}
