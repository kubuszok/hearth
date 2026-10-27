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

  // A trait carrying a self-type requirement: the member `fromSelfType` is declared on the required `SelfProvider`,
  // NOT on `SelfTyped` itself, so it is only visible through `this`. The macro is expanded inside `SelfTyped`.
  trait SelfProvider {
    def fromSelfType: Int
  }
  trait SelfTyped { this: SelfProvider =>
    def selfTypeMembers: Data = EnclosuresFixtures.testEnclosingClassHasSelfTypeMember
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

  // Macros expanded inside methods with parameters - plain methods (not test lambdas) to avoid `$anonfun` owners.
  object ParamNest {
    import EnclosuresFixtures.{
      testEnclosingMethodParameterNames as names,
      testEnclosingMethodParameters as params,
      testSumEnclosingMethodParameters as sum
    }

    def clauses(a: Int, b: String)(implicit c: Long): Data = { val _ = (a, b, c); params }
    // (only names/flags: Scala 3 prints a method type parameter with its owner prefix, Scala 2 does not)
    def generic[T](n: Int, t: T): Data = { val _ = (n, t); names }
    def contextBound[T: Ordering](t: T): Data = { val _ = (t, implicitly[Ordering[T]]); names }
    def byName(x: => Int): Data = { val _ = x; params }
    def varargs(xs: Int*): Data = { val _ = xs; params }
    def emptyClause(): Data = params
    def noClause: Data = params
    def inLambda(a: Int): Data = List("x").map { _ =>
      val _ = a; params
    }.head
    def inLocalDef(a: Int): Data = {
      def local(b: String): Data = { val _ = b; params }
      val _ = a
      local("b")
    }
    def inLocalClass(a: Int): Data = {
      class Local { val data: Data = params }
      val _ = a
      (new Local).data
    }
    val inClassBody: Data = params

    // `b` is deliberately not touched here: it must be evaluated exactly once, by the generated code.
    def sumAll(a: Int, b: => Int, s: String)(xs: Int*): Int = { val _ = (a, s, xs); sum }
  }

  object ParamNest2 {
    import EnclosuresFixtures.testEnclosingMethodParameters as params

    def implicitOnly(implicit c: Long): Data = { val _ = c; params }
  }

  // Values and traits as the innermost enclosures, and a lambda between the method and the expansion.
  object ValueNest {
    val valueChain: Data = EnclosuresFixtures.testEnclosingScope
    lazy val lazyValueChain: Data = EnclosuresFixtures.testEnclosingScope
    val valueType: String = EnclosuresFixtures.testEnclosingValueType
    def inLambda: Data = List(1).map(_ => EnclosuresFixtures.testEnclosingScope).head
  }
  trait TraitNest {
    def traitChain: Data = EnclosuresFixtures.testEnclosingScope
  }

  // Only the closest class gets a `thisRef`, an object always has one.
  object DetailsNest {
    final class Outer {
      final class Inner {
        def details: Data = EnclosuresFixtures.testEnclosureDetails
      }
    }
    val valueDetails: Data = EnclosuresFixtures.testEnclosureDetails
  }

  object PositionsNest {
    final class Inner {
      def sorted: Data = EnclosuresFixtures.testEnclosurePositionsSorted
    }
  }

  // `helper` is declared only on the parent class, so it can be found only among the inherited members.
  abstract class HelperParent {
    def helper: Int = 5
  }
  final class InheritsHelper extends HelperParent {
    def callsHelper: Int = EnclosuresFixtures.testCallEnclosingHelper
  }

  private def param(name: String, tpe: String, isImplicit: Boolean = false, isByName: Boolean = false): Data =
    Data.map(
      "name" -> Data(name),
      "type" -> Data(tpe),
      "isImplicit" -> Data(isImplicit),
      "isByName" -> Data(isByName)
    )

  group("trait hearth.Enclosures") {

    group("Environment.enclosingMethodParameters, expected behavior") {

      test("lists every value clause of the enclosing method, including implicit ones") {
        ParamNest.clauses(1, "x")(2L) <==> Data.list(
          Data.list(param("a", "scala.Int"), param("b", "java.lang.String")),
          Data.list(param("c", "scala.Long", isImplicit = true))
        )
      }

      test("does not list type parameters") {
        ParamNest.generic[String](1, "s") <==> Data.list(Data.list(Data("n"), Data("t")))
      }

      test("lists a context bound's evidence as an implicit parameter") {
        ParamNest.contextBound(1) <==> Data.list(Data.list(Data("t")), Data.list(Data("implicit evidence$1")))
      }

      test("unwraps by-name (=> A) and vararg (A*) parameter types") {
        ParamNest.byName(1) <==> Data.list(Data.list(param("x", "scala.Int", isByName = true)))
        ParamNest.varargs(1, 2) <==> Data.list(
          Data.list(param("xs", "scala.collection.immutable.Seq[scala.Int]"))
        )
      }

      test("distinguishes an empty clause from no clause") {
        ParamNest.emptyClause() <==> Data.list(Data.list())
        ParamNest.noClause <==> Data.list()
      }

      test("skips lambdas (their parameters are not listed) and stops at the closest method") {
        ParamNest.inLambda(1) <==> Data.list(Data.list(param("a", "scala.Int")))
        ParamNest.inLocalDef(1) <==> Data.list(Data.list(param("b", "java.lang.String")))
      }

      test("is empty in a class body (also of a local class inside a method)") {
        ParamNest.inClassBody <==> Data.list()
        ParamNest.inLocalClass(1) <==> Data.list()
      }

      test("refs are usable in generated code (also of by-name and vararg parameters)") {
        var evaluated = 0
        ParamNest.sumAll(1, { evaluated += 1; 10 }, "ignored")(100, 1000) ==> 1111
        evaluated ==> 1
      }
    }

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

      // A trait's self-type requirement (`this: SelfProvider =>`) contributes members visible on `this`.
      // These must appear in the enclosing class's `members` (wiring/DI relies on it) on BOTH platforms.
      test("includes self-type requirement members of the enclosing trait") {
        val instance = new SelfTyped with SelfProvider { def fromSelfType: Int = 1 }
        instance.selfTypeMembers <==> Data(true)
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

    group("Environment.enclosingMethodParameters edge cases, expected behavior") {

      test("lists a method with only an implicit clause") {
        ParamNest2.implicitOnly(1L) <==> Data.list(Data.list(param("c", "scala.Long", isImplicit = true)))
      }
    }

    group("Environment.enclosingScope edge cases, expected behavior") {

      test("lists a val as a value enclosure") {
        ValueNest.valueChain <==> Data.list(
          Data.map("kind" -> Data("value"), "name" -> Data("valueChain")),
          Data.map("kind" -> Data("object"), "name" -> Data("ValueNest")),
          Data.map("kind" -> Data("class"), "name" -> Data("EnclosuresSpec"))
        )
      }

      test("lists a lazy val as a value enclosure") {
        ValueNest.lazyValueChain <==> Data.list(
          Data.map("kind" -> Data("value"), "name" -> Data("lazyValueChain")),
          Data.map("kind" -> Data("object"), "name" -> Data("ValueNest")),
          Data.map("kind" -> Data("class"), "name" -> Data("EnclosuresSpec"))
        )
      }

      test("gives a value enclosure the declared type of the value") {
        ValueNest.valueType <==> "java.lang.String"
      }

      test("skips a lambda between the expansion and the enclosing method") {
        ValueNest.inLambda <==> Data.list(
          Data.map("kind" -> Data("method"), "name" -> Data("inLambda")),
          Data.map("kind" -> Data("object"), "name" -> Data("ValueNest")),
          Data.map("kind" -> Data("class"), "name" -> Data("EnclosuresSpec"))
        )
      }

      test("lists a trait as a class enclosure") {
        (new TraitNest {}).traitChain <==> Data.list(
          Data.map("kind" -> Data("method"), "name" -> Data("traitChain")),
          Data.map("kind" -> Data("class"), "name" -> Data("TraitNest")),
          Data.map("kind" -> Data("class"), "name" -> Data("EnclosuresSpec"))
        )
      }

      test("gives thisRef only to the closest class, gives no type to a method and positions to all definitions") {
        val outer = new DetailsNest.Outer
        (new outer.Inner).details <==> Data.list(
          Data.map(
            "kind" -> Data("method"),
            "name" -> Data("details"),
            "hasPosition" -> Data(true),
            "hasTpe" -> Data(false)
          ),
          Data.map(
            "kind" -> Data("class"),
            "name" -> Data("Inner"),
            "hasPosition" -> Data(true),
            "hasThisRef" -> Data(true)
          ),
          Data.map(
            "kind" -> Data("class"),
            "name" -> Data("Outer"),
            "hasPosition" -> Data(true),
            "hasThisRef" -> Data(false)
          ),
          Data.map("kind" -> Data("object"), "name" -> Data("DetailsNest"), "hasPosition" -> Data(true)),
          Data.map(
            "kind" -> Data("class"),
            "name" -> Data("EnclosuresSpec"),
            "hasPosition" -> Data(true),
            "hasThisRef" -> Data(false)
          )
        )
        DetailsNest.valueDetails <==> Data.list(
          Data.map(
            "kind" -> Data("value"),
            "name" -> Data("valueDetails"),
            "hasPosition" -> Data(true)
          ),
          Data.map("kind" -> Data("object"), "name" -> Data("DetailsNest"), "hasPosition" -> Data(true)),
          Data.map(
            "kind" -> Data("class"),
            "name" -> Data("EnclosuresSpec"),
            "hasPosition" -> Data(true),
            "hasThisRef" -> Data(true)
          )
        )
      }

      test("enclosure positions sort from the outermost definition to the expansion point") {
        (new PositionsNest.Inner).sorted <==> Data.list(
          Data("EnclosuresSpec"),
          Data("PositionsNest"),
          Data("Inner"),
          Data("sorted"),
          Data("<expansion>")
        )
      }
    }

    group("Environment.enclosingScope member calling edge cases, expected behavior") {

      test("locates and calls a member inherited by the enclosing class") {
        (new InheritsHelper).callsHelper ==> 5
      }
    }
  }
}
