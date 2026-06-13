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

  // The macro is expanded inside a method that declares local vals BEFORE the call (and one AFTER, which must not be
  // discovered). Defined as plain methods (not test lambdas) to avoid synthetic `$anonfun` owners.
  object LocalNest {

    def localValues: Data = {
      val a: Int = 1
      val b: String = "x"
      val discovered: Data = EnclosuresFixtures.testEnclosingLocalValues
      val c: Long = 3L // declared AFTER the macro call: must NOT appear
      val _ = (a, b, c)
      discovered
    }

    def sumInts: Int = {
      val a: Int = 10
      val b: Int = 20
      val c: Int = 30
      val result: Int = EnclosuresFixtures.testSumEnclosingLocalInts
      val _ = (a, b, c)
      result
    }
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

    group("Environment.enclosingScope local values (macwire case), expected behavior") {

      // PLATFORM ASYMMETRY: on Scala 2 `c.enclosingMethod` gives the enclosing method body, so local vals declared
      // before the macro call are discoverable. On Scala 3 `Symbol.tree` returns a `DefDef` whose `rhs` is `None`
      // during the method's own expansion (the body is not yet available), so `localValues` is empty there. See
      // `Enclosure.LocalValue` scaladoc.
      if (LanguageVersion.byHearth.isScala3) {

        test("Scala 3: localValues is empty (method body unavailable during expansion)") {
          LocalNest.localValues <==> Data.list()
        }

        test("Scala 3: no refs to sum, result is 0") {
          LocalNest.sumInts ==> 0
        }
      } else {

        test("Scala 2: discovers local vals declared before the macro call, and not those declared after") {
          LocalNest.localValues <==> Data.list(
            Data.map("name" -> Data("a"), "type" -> Data("scala.Int")),
            Data.map("name" -> Data("b"), "type" -> Data("java.lang.String"))
          )
        }

        test("Scala 2: the discovered refs are usable as expressions in generated code") {
          LocalNest.sumInts ==> 60
        }
      }
    }
  }
}
