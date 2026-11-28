package hearth
package crossquotes

final class QualifiedContextBoundSpec extends MacroSuite {

  test("a macro entry point may write its context bound as scala.quoted.Type") {
    QualifiedContextBoundFixtures.typeName[Int] <==> "scala.Int"
  }
}
