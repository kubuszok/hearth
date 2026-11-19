package hearth
package typed

/** Macro implementation is in [[ClassesFixturesImpl]] */
final class ClassNestedEnumSpec extends MacroSuite {

  // Declared inside the class: the enum's companion is a module that is NOT static.
  enum Level {
    case Debug
    case Custom(name: String)
  }

  test("Enum[A].matchOn should type the cases of an enum declared inside a class") {
    import ClassesFixtures.testEnumMatchOnWithTypedHelper

    def code(level: Level) = testEnumMatchOnWithTypedHelper(level)

    code(Level.Debug) <==> "Debug"
    code(Level.Custom("x")) <==> "Custom"
  }
}
