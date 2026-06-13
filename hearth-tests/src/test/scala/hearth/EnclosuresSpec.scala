package hearth

import hearth.data.Data

final class EnclosuresSpec extends MacroSuite {

  // A nested structure: object -> class -> method, so we can assert the enclosing-scope chain shape and prove that an
  // in-scope member can be located and called. The macro is expanded INSIDE `Nest.Inner.scopeChain` / `callsHelper`,
  // NOT inside a munit test lambda (which would add synthetic `$anonfun` owners).
  object Nest {

    final class Inner {

      def helper: Int = 42

      def scopeChain: Data = EnclosuresFixtures.testEnclosingScope

      def reachesPackage: Data = EnclosuresFixtures.testEnclosingScopeReachesPackage

      def callsHelper: Int = EnclosuresFixtures.testCallEnclosingHelper
    }
  }

  object NestObj {

    def helper: Int = 7

    def callsHelper: Int = EnclosuresFixtures.testCallEnclosingHelper
  }

  group("trait hearth.Enclosures") {

    group("Environment.enclosingScope, expected behavior") {

      test("returns the lexical enclosure chain from innermost method outwards (identical on Scala 2 and 3)") {
        (new Nest.Inner).scopeChain <==> Data.list(
          Data.map("kind" -> Data("method"), "name" -> Data("scopeChain")),
          Data.map("kind" -> Data("class"), "name" -> Data("Inner")),
          Data.map("kind" -> Data("object"), "name" -> Data("Nest")),
          Data.map("kind" -> Data("class"), "name" -> Data("EnclosuresSpec"))
        )
      }

      test("always reaches a package enclosure at the end of the chain") {
        (new Nest.Inner).reachesPackage <==> Data(true)
      }
    }

    group("Environment.enclosingScope member calling, expected behavior") {

      test("locates and calls a nullary Int-returning member on the enclosing class instance") {
        (new Nest.Inner).callsHelper ==> 42
      }

      test("locates and calls a nullary Int-returning member on the enclosing object") {
        NestObj.callsHelper ==> 7
      }
    }
  }
}
