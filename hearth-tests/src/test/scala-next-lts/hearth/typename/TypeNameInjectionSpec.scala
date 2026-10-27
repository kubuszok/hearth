package hearth
package typename

/** Tests for TypeName injection via context bounds.
  *
  * These tests require `summonImplicitIgnoring` (Scala 2.13.17+ / Scala 3), so they are placed in `scala-next-lts` to
  * avoid running on Scala 2.13.16 where summoning is disabled.
  */
final class TypeNameInjectionSpec extends MacroSuite {

  group("TypeName injection") {

    test("custom TypeName overrides inner type in plainPrint") {
      implicit val custom: TypeName[String] = new TypeName[String] {
        def prettyPrint: String = "CustomString"
        def plainPrint: String = "CustomString"
        def simplePrint: String = "CustomString"
        def shortPrint: String = "CustomString"
      }
      assertEquals(TypeName[Option[String]].plainPrint, "scala.Option[CustomString]")
    }

    test("custom TypeName overrides inner type in simplePrint") {
      implicit val custom: TypeName[String] = new TypeName[String] {
        def prettyPrint: String = "Str"
        def plainPrint: String = "Str"
        def simplePrint: String = "Str"
        def shortPrint: String = "Str"
      }
      assertEquals(TypeName[Option[String]].simplePrint, "Option[Str]")
    }

    test("custom TypeName overrides both args in Map") {
      implicit val customStr: TypeName[String] = new TypeName[String] {
        def prettyPrint: String = "MyStr"
        def plainPrint: String = "MyStr"
        def simplePrint: String = "MyStr"
        def shortPrint: String = "MyStr"
      }
      implicit val customInt: TypeName[Int] = new TypeName[Int] {
        def prettyPrint: String = "MyInt"
        def plainPrint: String = "MyInt"
        def simplePrint: String = "MyInt"
        def shortPrint: String = "MyInt"
      }
      assertEquals(
        TypeName[Map[String, Int]].plainPrint,
        "scala.collection.immutable.Map[MyStr, MyInt]"
      )
    }

    test("no TypeName instance falls back to default") {
      assertEquals(TypeName[Boolean].plainPrint, "scala.Boolean")
    }

    group("context bound injection") {

      def listTypeName[A: TypeName]: TypeName[List[A]] = TypeName.derived

      test("uses the context bound's instance for the abstract type parameter") {
        listTypeName[String].plainPrint <==> "scala.collection.immutable.List[java.lang.String]"
        listTypeName[String].simplePrint <==> "List[String]"
        listTypeName[String].shortPrint <==> "List"
      }

      test("uses a custom instance passed through the context bound") {
        implicit val custom: TypeName[String] = new TypeName[String] {
          def prettyPrint: String = "Str"
          def plainPrint: String = "Str"
          def simplePrint: String = "Str"
          def shortPrint: String = "Str"
        }
        listTypeName[String].plainPrint <==> "scala.collection.immutable.List[Str]"
        listTypeName[String].simplePrint <==> "List[Str]"
        assert(listTypeName[String].prettyPrint.contains("Str"))
      }
    }
  }
}
