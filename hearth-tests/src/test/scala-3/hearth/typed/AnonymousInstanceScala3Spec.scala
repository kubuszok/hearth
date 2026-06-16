package hearth
package typed

/** Macro implementation is in [[AnonymousInstanceScala3Fixtures]] — Scala 3-only AnonymousInstance shapes. */
final class AnonymousInstanceScala3Spec extends MacroSuite {

  group("typed.Classes") {

    group("AnonymousInstance (Scala 3-only)") {

      test("context-function return stays a context function (not a using-param clause)") {
        // build(k): Int ?=> String — override returns `(i) ?=> k + i`, invoked as build("k")(using 7)
        AnonymousInstanceScala3Fixtures.testContextFunctionReturn <==> "k7"
      }
    }
  }
}
