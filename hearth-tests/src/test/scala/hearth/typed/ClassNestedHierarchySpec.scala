package hearth
package typed

/** Macro implementation is in [[ClassesFixturesImpl]] */
final class ClassNestedHierarchySpec extends MacroSuite {

  // Declared inside the class, unlike the fixtures in `hearth.examples`: here the companion `Shape` is a module that
  // is NOT static, so its case object's type must be reached through `ClassNestedHierarchySpec.this.Shape`.
  sealed trait Shape
  object Shape {
    case object Empty extends Shape
    final case class Square(side: Double) extends Shape
  }
  final case class Circle(r: Double) extends Shape

  test("Enum[A].matchOn should type the children of a sealed trait declared inside a class") {
    import ClassesFixtures.testEnumMatchOnWithTypedHelper

    def code(shape: Shape) = testEnumMatchOnWithTypedHelper(shape)

    code(Shape.Empty) <==> "Empty"
    code(Shape.Square(1.0)) <==> "Square"
    code(Circle(1.0)) <==> "Circle"
  }
}
