package hearth
package typename

import hearth.data.Data

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

    group("auto-derived instances for other kinds of types") {
      import hearth.examples.miscedge.*

      def render[A](tn: TypeName[A]): Data = Data.map(
        "plainPrint" -> Data(tn.plainPrint),
        "simplePrint" -> Data(tn.simplePrint),
        "shortPrint" -> Data(tn.shortPrint),
        "prettyPrintWithoutANSI" -> Data(tn.prettyPrint.stripANSI)
      )
      def expected(plain: String, simple: String, short: String): Data = Data.map(
        "plainPrint" -> Data(plain),
        "simplePrint" -> Data(simple),
        "shortPrint" -> Data(short),
        "prettyPrintWithoutANSI" -> Data(plain)
      )

      test("tuple") {
        render(TypeName[(String, Int)]) <==> expected(
          "scala.Tuple2[java.lang.String, scala.Int]",
          "Tuple2[String, Int]",
          "Tuple2"
        )
      }

      test("function") {
        render(TypeName[Int => String]) <==> expected(
          "scala.Function1[scala.Int, java.lang.String]",
          "Function1[Int, String]",
          "Function1"
        )
      }

      test("array") {
        render(TypeName[Array[Int]]) <==> expected("scala.Array[scala.Int]", "Array[Int]", "Array")
      }

      test("user-defined generic class") {
        render(TypeName[Box[Option[Int]]]) <==> expected(
          "hearth.examples.miscedge.Box[scala.Option[scala.Int]]",
          "Box[Option[Int]]",
          "Box"
        )
      }

      test("class nested in an object") {
        render(TypeName[Outer.Nested]) <==> expected("hearth.examples.miscedge.Outer.Nested", "Nested", "Nested")
      }

      test("singleton type of an object") {
        render(TypeName[SingletonObject.type]) <==> expected(
          "hearth.examples.miscedge.SingletonObject.type",
          "SingletonObject",
          "SingletonObject"
        )
      }

      test("type alias is printed as the aliased type") {
        render(TypeName[Aliases.StringOption]) <==> expected(
          "scala.Option[java.lang.String]",
          "Option[String]",
          "Option"
        )
      }

      test("Nothing and Any") {
        render(TypeName[Nothing]) <==> expected("scala.Nothing", "Nothing", "Nothing")
        render(TypeName[Any]) <==> expected("scala.Any", "Any", "Any")
      }

      test("prettyPrint without ANSI codes is the same as plainPrint for applied types") {
        render(TypeName[Map[String, List[Int]]]) <==> expected(
          "scala.collection.immutable.Map[java.lang.String, scala.collection.immutable.List[scala.Int]]",
          "Map[String, List[Int]]",
          "Map"
        )
      }
    }

    group("TypeName.derived called explicitly") {

      test("gives the same names as the summoned instance") {
        val derived: TypeName[Option[String]] = TypeName.derived[Option[String]]
        derived.plainPrint <==> TypeName[Option[String]].plainPrint
        derived.simplePrint <==> TypeName[Option[String]].simplePrint
        derived.shortPrint <==> TypeName[Option[String]].shortPrint
      }
    }
  }
}
