package hearth
package typename

final class TypeNameSpec extends MacroSuite {

  group("TypeName") {

    group("auto-derived plainPrint") {

      test("simple type") {
        assertEquals(TypeName[String].plainPrint, "java.lang.String")
      }

      test("applied type") {
        assertEquals(TypeName[Option[String]].plainPrint, "scala.Option[java.lang.String]")
      }

      test("multi-arg applied type") {
        assertEquals(
          TypeName[Map[String, Int]].plainPrint,
          "scala.collection.immutable.Map[java.lang.String, scala.Int]"
        )
      }

      test("nested applied type") {
        assertEquals(
          TypeName[Option[List[String]]].plainPrint,
          "scala.Option[scala.collection.immutable.List[java.lang.String]]"
        )
      }
    }

    group("auto-derived shortPrint") {

      test("simple type") {
        assertEquals(TypeName[String].shortPrint, "String")
      }

      test("applied type — no type params") {
        assertEquals(TypeName[Option[String]].shortPrint, "Option")
      }
    }

    group("auto-derived simplePrint") {

      test("simple type") {
        assertEquals(TypeName[String].simplePrint, "String")
      }

      test("applied type — short names with type params") {
        assertEquals(TypeName[Option[String]].simplePrint, "Option[String]")
      }

      test("multi-arg applied type") {
        assertEquals(TypeName[Map[String, Int]].simplePrint, "Map[String, Int]")
      }
    }

    group("auto-derived prettyPrint") {

      test("contains ANSI codes") {
        val result = TypeName[Boolean].prettyPrint
        assert(result.stripANSI != result, s"Expected ANSI codes in: $result")
      }
    }

    group("toString delegates to plainPrint") {

      test("toString matches plainPrint") {
        val tn = TypeName[String]
        assertEquals(tn.toString, tn.plainPrint)
      }
    }

  }
}
