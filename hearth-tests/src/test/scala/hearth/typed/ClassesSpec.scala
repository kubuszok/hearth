package hearth
package typed

import hearth.data.Data

/** Macro implementation is in [[ClassesFixturesImpl]] */
final class ClassesSpec extends MacroSuite {

  group("trait typed.Classes") {

    group("class: Class[A], returns preprocessed class") {
      import ClassesFixtures.testClass

      test("for class") {

        testClass[examples.classes.ExampleClass](
          "clone",
          "equals",
          "finalize",
          "getClass",
          "hashCode",
          "notify",
          "notifyAll",
          "toString",
          "wait",
          "asInstanceOf",
          "isInstanceOf",
          "synchronized",
          "==",
          "!=",
          "eq",
          "ne",
          "##"
        ) <==> Data.map(
          "commons" -> Data.map(
            "constructors" -> Data.list(Data("()")),
            "methods" -> Data.list()
          ),
          "asSingleton" -> Data("<no singleton>"),
          "asNamedTuple" -> Data("<no named tuple>"),
          "asCaseClass" -> Data("<no case class>"),
          "asEnum" -> Data("<no enum>"),
          "asJavaBean" -> Data.map(
            "defaultConstructor" -> Data("()")
          )
        )
      }

      test("for case class") {

        // It seems some methods exist only on Scala 2.13, and some only on Scala 3.
        val methods =
          if (LanguageVersion.byHearth.isScala2_13)
            Data.list(
              // Data.map("name" -> Data("_1"), "parameters" -> Data("")), // Scala 3-only, alternative to `a`?
              Data.map("name" -> Data("a"), "parameters" -> Data("")),
              Data
                .map("name" -> Data("andThen"), "parameters" -> Data("")), // Scala 2-only, I guess from the companion?
              Data.map("name" -> Data("apply"), "parameters" -> Data("(a: scala.Int)")), // Scala 2-only
              Data.map("name" -> Data("canEqual"), "parameters" -> Data("(x$1: scala.Any)")),
              Data
                .map("name" -> Data("compose"), "parameters" -> Data("")), // Scala 2-only, I guess from the companion?
              Data.map("name" -> Data("copy"), "parameters" -> Data("(a: scala.Int)")),
              // Data.map("name" -> Data("fromProduct"), "parameters" -> Data("(x$0: scala.Product)")),
              Data.map("name" -> Data("productArity"), "parameters" -> Data("")),
              Data.map("name" -> Data("productElement"), "parameters" -> Data("(x$1: scala.Int)")),
              Data.map("name" -> Data("productElementName"), "parameters" -> Data("(x$1: scala.Int)")),
              Data.map("name" -> Data("productElementNames"), "parameters" -> Data("")),
              Data.map("name" -> Data("productIterator"), "parameters" -> Data("")),
              Data.map("name" -> Data("productPrefix"), "parameters" -> Data("")),
              Data.map(
                "name" -> Data("unapply"),
                "parameters" -> Data("(x$0: hearth.examples.classes.ExampleCaseClass)")
              ),
              Data.map("name" -> Data("writeReplace"), "parameters" -> Data("()"))
            )
          else
            Data.list(
              Data.map("name" -> Data("_1"), "parameters" -> Data("")), // Scala 3-only, alternative to `a`?
              Data.map("name" -> Data("a"), "parameters" -> Data("")),
              // Data.map("name" -> Data("andThen"), "parameters" -> Data("")), // Scala 2-only, I guess from the companion?
              // Data.map("name" -> Data("apply"), "parameters" -> Data("(a: scala.Int)")), // Scala 2-only
              Data.map("name" -> Data("canEqual"), "parameters" -> Data("(that: scala.Any)")),
              // Data.map("name" -> Data("compose"), "parameters" -> Data("")), // Scala 2-only, I guess from the companion?
              Data.map("name" -> Data("copy"), "parameters" -> Data("(a: scala.Int)")),
              Data.map("name" -> Data("fromProduct"), "parameters" -> Data("(x$0: scala.Product)")),
              Data.map("name" -> Data("productArity"), "parameters" -> Data("")),
              Data.map("name" -> Data("productElement"), "parameters" -> Data("(n: scala.Int)")),
              Data.map("name" -> Data("productElementName"), "parameters" -> Data("(n: scala.Int)")),
              Data.map("name" -> Data("productElementNames"), "parameters" -> Data("")),
              Data.map("name" -> Data("productIterator"), "parameters" -> Data("")),
              Data.map("name" -> Data("productPrefix"), "parameters" -> Data("")),
              Data.map(
                "name" -> Data("unapply"),
                "parameters" -> Data("(x$1: hearth.examples.classes.ExampleCaseClass)")
              ),
              Data.map("name" -> Data("writeReplace"), "parameters" -> Data("()"))
            )

        testClass[examples.classes.ExampleCaseClass](
          "clone",
          "equals",
          "finalize",
          "getClass",
          "hashCode",
          "notify",
          "notifyAll",
          "toString",
          "wait",
          "asInstanceOf",
          "isInstanceOf",
          "synchronized",
          "==",
          "!=",
          "eq",
          "ne",
          "##"
        ) <==> Data.map(
          "commons" -> Data.map(
            "constructors" -> Data.list(Data("(a: scala.Int)")),
            "methods" -> methods
          ),
          "asSingleton" -> Data("<no singleton>"),
          "asNamedTuple" -> Data("<no named tuple>"),
          "asCaseClass" -> Data.map(
            "primaryConstructor" -> Data("(a: scala.Int)"),
            "nonPrimaryConstructors" -> Data.list(Data("(a: scala.Int)")),
            "caseFields" -> Data.list(
              Data.map(
                "name" -> Data("a"),
                "parameters" -> Data("")
              )
            )
          ),
          "asEnum" -> Data("<no enum>"),
          "asJavaBean" -> Data("<no java bean>")
        )
      }

      test("for sealed trait") {

        // It seems some methods exist only on Scala 2.13, and some only on Scala 3.
        val constructors = if (LanguageVersion.byHearth.isScala2_13) Data.list() else Data.list(Data("()"))

        // It seems some methods exist only on Scala 2.13, and some only on Scala 3.
        val methods =
          if (LanguageVersion.byHearth.isScala2_13)
            Data.list(
              Data.map("name" -> Data("ExampleSealedTraitClass"), "parameters" -> Data("")),
              Data.map("name" -> Data("ExampleSealedTraitObject"), "parameters" -> Data(""))
            )
          else
            Data.list(
              Data.map("name" -> Data("ExampleSealedTraitClass"), "parameters" -> Data("")),
              Data.map(
                "name" -> Data("ordinal"),
                "parameters" -> Data("(x$0: hearth.examples.enums.ExampleSealedTrait)")
              ),
              Data.map("name" -> Data("writeReplace"), "parameters" -> Data("()"))
            )

        testClass[examples.enums.ExampleSealedTrait](
          "clone",
          "equals",
          "finalize",
          "getClass",
          "hashCode",
          "notify",
          "notifyAll",
          "toString",
          "wait",
          "asInstanceOf",
          "isInstanceOf",
          "synchronized",
          "==",
          "!=",
          "eq",
          "ne",
          "##"
        ) <==> Data.map(
          "commons" -> Data.map(
            "constructors" -> constructors,
            "methods" -> methods
          ),
          "asSingleton" -> Data("<no singleton>"),
          "asNamedTuple" -> Data("<no named tuple>"),
          "asCaseClass" -> Data("<no case class>"),
          "asEnum" -> Data.map(
            "directChildren" -> Data(
              "(ExampleSealedTraitClass: hearth.examples.enums.ExampleSealedTrait.ExampleSealedTraitClass, ExampleSealedTraitObject: hearth.examples.enums.ExampleSealedTrait.ExampleSealedTraitObject.type)"
            ),
            "exhaustiveChildren" -> Data(
              "(ExampleSealedTraitClass: hearth.examples.enums.ExampleSealedTrait.ExampleSealedTraitClass, ExampleSealedTraitObject: hearth.examples.enums.ExampleSealedTrait.ExampleSealedTraitObject.type)"
            )
          ),
          "asJavaBean" -> Data("<no java bean>")
        )
      }
    }

    group("directChildren ordering") {
      import ClassesFixtures.testEnumParseDiagnostic

      // A sealed hierarchy's children must come back in DECLARATION order, deterministically, on both platforms. On
      // Scala 3 the synthetic module-CLASS symbols frequently lack a position during macro expansion, so sorting them
      // by source position can intermittently collapse the whole list onto an alphabetical name fallback -
      // declaration order on a "warm" build, alphabetical on a "cold"/from-TASTy one, for the SAME sources.
      test("sealed trait children preserve declaration order (not alphabetical)") {
        testEnumParseDiagnostic[ClassesSpec.TrafficLight] <==>
          "parsed with children: Red, Yellow, Green"
      }
    }

    test("CaseClass[A].{construct and parConstruct} should construct an instance of the case class") {
      import ClassesFixtures.testCaseClassConstructAndParConstruct

      testCaseClassConstructAndParConstruct[examples.classes.ExampleCaseClass] <==>
        "sequential: new hearth.examples.classes.ExampleCaseClass(0), parallel: new hearth.examples.classes.ExampleCaseClass(0)"
    }

    test("SingletonValue[A] should return the singleton expression for a case object") {
      import ClassesFixtures.testSingletonExpr

      val result =
        testSingletonExpr[examples.enums.ExampleSealedTrait.ExampleSealedTraitObject.type]
      assert(result.startsWith("singletonExpr: "), s"Expected 'singletonExpr: ...' but got: $result")
      assert(result.contains("ExampleSealedTraitObject"), s"Expected singleton reference but got: $result")
    }

    test("CaseClass[A].{construct and parConstruct} should return <no case class> for a case object") {
      import ClassesFixtures.testCaseClassConstructAndParConstruct

      testCaseClassConstructAndParConstruct[examples.enums.ExampleSealedTrait.ExampleSealedTraitObject.type] <==>
        "<no case class>"
    }

    test("CaseClass[A].caseFieldValuesAt should extract fields of the case class") {
      import ClassesFixtures.testCaseClassCaseFieldValuesAt

      testCaseClassCaseFieldValuesAt(hearth.examples.classes.ExampleCaseClass(0)) <==>
        "(a: hearth.examples.classes.ExampleCaseClass.apply(0).a)"
    }

    // caseFieldValuesAt(instance, visibility) filters case fields by accessibility. The DEFAULT is AtCallSite: only
    // fields accessible at the macro expansion point are returned, because the generated `instance.field` access has
    // to compile there. `private[hearth]` field b IS
    // accessible at this call site (inside hearth), so default + AtCallSite both keep it, while explicit
    // Everywhere (b is not public) skips it on both platforms. Anywhere keeps every field regardless.
    test("CaseClass[A].caseFieldValuesAt should filter fields by the requested visibility") {
      import ClassesFixtures.{
        testCaseClassCaseFieldValuesAt,
        testCaseClassCaseFieldValuesAtCallSite,
        testCaseClassCaseFieldValuesAtEverywhere,
        testCaseClassCaseFieldValuesAtAnywhere
      }

      // On Scala 2 the non-public case field b is read through the public synthetic accessor b$access$1, on
      // Scala 3 through the restricted accessor b. The map key is normalized to the declared name b, and the
      // visibility filtering follows the declared field's visibility on both platforms - only the printed
      // accessor call differs.
      val bCall = if (LanguageVersion.byHearth.isScala2_13) "b$access$1" else "b"

      // Default (AtCallSite): b is private[hearth] and we expand inside hearth, so it is kept.
      testCaseClassCaseFieldValuesAt(hearth.examples.classes.ExampleCaseClassWithPrivateField(0, "b")) <==>
        s"(a: hearth.examples.classes.ExampleCaseClassWithPrivateField.apply(0, \"b\").a, b: hearth.examples.classes.ExampleCaseClassWithPrivateField.apply(0, \"b\").$bCall)"

      // AtCallSite (explicit): same as the default.
      testCaseClassCaseFieldValuesAtCallSite(hearth.examples.classes.ExampleCaseClassWithPrivateField(0, "b")) <==>
        s"(a: hearth.examples.classes.ExampleCaseClassWithPrivateField.apply(0, \"b\").a, b: hearth.examples.classes.ExampleCaseClassWithPrivateField.apply(0, \"b\").$bCall)"

      // Everywhere: b is not public, so it is dropped on both platforms.
      testCaseClassCaseFieldValuesAtEverywhere(hearth.examples.classes.ExampleCaseClassWithPrivateField(0, "b")) <==>
        "(a: hearth.examples.classes.ExampleCaseClassWithPrivateField.apply(0, \"b\").a)"

      // Anywhere: every case field regardless of visibility - kept here too.
      testCaseClassCaseFieldValuesAtAnywhere(hearth.examples.classes.ExampleCaseClassWithPrivateField(0, "b")) <==>
        s"(a: hearth.examples.classes.ExampleCaseClassWithPrivateField.apply(0, \"b\").a, b: hearth.examples.classes.ExampleCaseClassWithPrivateField.apply(0, \"b\").$bCall)"
    }

    // The class-private case field `b` (`private val`) is inaccessible even at the in-hearth call site, so it
    // distinguishes the three scopes: default (AtCallSite) and Everywhere DROP it, while Anywhere KEEPS it.
    test("CaseClass[A].caseFieldValuesAt distinguishes AtCallSite/Everywhere (drop) from Anywhere (keep)") {
      import ClassesFixtures.{
        testCaseClassCaseFieldValuesAt,
        testCaseClassCaseFieldValuesAtCallSite,
        testCaseClassCaseFieldValuesAtEverywhere,
        testCaseClassCaseFieldValuesAtAnywhere
      }

      // On Scala 2 the class-private case field b is read through the public synthetic accessor b$access$1, on
      // Scala 3 through the restricted accessor b. Same normalization as above.
      val bCall = if (LanguageVersion.byHearth.isScala2_13) "b$access$1" else "b"

      // Default (AtCallSite): b is inaccessible at the call site -> dropped.
      testCaseClassCaseFieldValuesAt(hearth.examples.classes.ExampleCaseClassWithClassPrivateField(0, "b")) <==>
        "(a: hearth.examples.classes.ExampleCaseClassWithClassPrivateField.apply(0, \"b\").a)"

      // AtCallSite (explicit): same as default -> dropped.
      testCaseClassCaseFieldValuesAtCallSite(hearth.examples.classes.ExampleCaseClassWithClassPrivateField(0, "b")) <==>
        "(a: hearth.examples.classes.ExampleCaseClassWithClassPrivateField.apply(0, \"b\").a)"

      // Everywhere: b is not public -> dropped.
      testCaseClassCaseFieldValuesAtEverywhere(
        hearth.examples.classes.ExampleCaseClassWithClassPrivateField(0, "b")
      ) <==>
        "(a: hearth.examples.classes.ExampleCaseClassWithClassPrivateField.apply(0, \"b\").a)"

      // Anywhere: ALL case fields regardless of visibility -> b kept.
      testCaseClassCaseFieldValuesAtAnywhere(hearth.examples.classes.ExampleCaseClassWithClassPrivateField(0, "b")) <==>
        s"(a: hearth.examples.classes.ExampleCaseClassWithClassPrivateField.apply(0, \"b\").a, b: hearth.examples.classes.ExampleCaseClassWithClassPrivateField.apply(0, \"b\").$bCall)"
    }

    test("CaseClass[A] should detect default values on constructor parameters") {
      import ClassesFixtures.testCaseClassDefaultValues

      testCaseClassDefaultValues[examples.classes.ExampleCaseClassWithDefaults] <==>
        "a: hasDefault=false, default=<no default>, b: hasDefault=true, default=resolved"
    }

    test("CaseClass round-trip: construct and extract field values at runtime") {
      import ClassesFixtures.testCaseClassConstructRoundTrip

      testCaseClassConstructRoundTrip[examples.classes.ExampleCaseClass] <==> "a=42"
    }

    test("CaseClass with vararg field: parse, construct via vararg-aware ApplyValues, Seq-typed accessor") {
      import ClassesFixtures.testVarargCaseClassConstruct

      // The vararg constructor parameter is normalized to scala.collection.immutable.Seq[A] (isVararg=true),
      // construct receives one Expr[Seq[A]] argument and re-splices it as `seq*`, and the case-field accessor
      // exposes a plain Seq[A]. Runtime equality verifies the constructed instance.
      testVarargCaseClassConstruct(hearth.examples.classes.ExampleCaseClassWithVarargs(1, 2, 3)) <==> Data.map(
        "primaryConstructor" -> Data("(xs: scala.collection.immutable.Seq[scala.Int] (vararg))"),
        "caseFields" -> Data("(xs: scala.collection.immutable.Seq[scala.Int])"),
        "constructedEqualsExpected" -> Data(true)
      )
    }

    test("CaseClass[A].caseFieldValuesAt should extract the Seq-typed field of a vararg case class") {
      import ClassesFixtures.testCaseClassCaseFieldValuesAt

      testCaseClassCaseFieldValuesAt(hearth.examples.classes.ExampleCaseClassWithVarargs(1, 2, 3)) <==>
        "(xs: hearth.examples.classes.ExampleCaseClassWithVarargs.apply(1, 2, 3).xs)"
    }

    test("SingletonValue round-trip: evaluate singleton expression at runtime") {
      import ClassesFixtures.testSingletonRoundTrip

      testSingletonRoundTrip[examples.enums.ExampleSealedTrait.ExampleSealedTraitObject.type] <==>
        "ExampleSealedTraitObject"
    }

    // Handlers build their results from nested Expr.quote calls that splice the matched expression and an Inlined
    // macro argument - splice ownership from these inner quotes must not cross the match case's splice boundary.
    test("Enum[A].parMatchOn with handler results built from nested Expr.quote") {
      import ClassesFixtures.testEnumParMatchOnNestedQuote

      val suffix = List(" (handled)").mkString // a local val, so on Scala 3 it arrives as an Inlined argument

      def code(input: hearth.examples.enums.ExampleSealedTrait) = testEnumParMatchOnNestedQuote(input, suffix)
      code(hearth.examples.enums.ExampleSealedTrait.ExampleSealedTraitClass(1)) <==>
        "ExampleSealedTraitClass(1) (handled)"
      code(hearth.examples.enums.ExampleSealedTrait.ExampleSealedTraitObject) <==>
        "ExampleSealedTraitObject (handled)"
    }

    // The individual `parse` methods of the class-views return a ClassViewResult.Incompatible carrying a
    // human-readable reason when the type does not fit the view. Those reason strings are public-facing
    // diagnostics; here we pin them per rejected view. The type-name prefix is stripped in the fixture so the
    // asserted suffix is identical on Scala 2 and Scala 3.
    group("class-view parse: Incompatible reasons") {
      import ClassesFixtures.testClassViewParseReasons

      test("a plain trait is rejected by every specific view (and Class is always compatible)") {
        testClassViewParseReasons[examples.classes.ExampleTrait] <==> Data.map(
          "asSingleton" -> Data("is not a singleton type"),
          "asNamedTuple" -> Data("is not a named tuple"),
          "asCaseClass" -> Data("is not a case class"),
          "asEnum" -> Data("is not sealed, not an enumeration, and not a union type"),
          "asJavaBean" -> Data("is not a plain old Java object"),
          "asClass" -> Data("<compatible>")
        )
      }

      test("a case class is compatible with CaseClass but rejected by the other specific views") {
        testClassViewParseReasons[examples.classes.ExampleCaseClass] <==> Data.map(
          "asSingleton" -> Data("is not a singleton type"),
          "asNamedTuple" -> Data("is not a named tuple"),
          "asCaseClass" -> Data("<compatible>"),
          "asEnum" -> Data("is not sealed, not an enumeration, and not a union type"),
          // A Scala case class is recognised as a POJO on the JVM, but its primary constructor takes arguments,
          // so it is rejected as a JavaBean for lacking a public default (no-arg) constructor.
          "asJavaBean" -> Data("is a POJO but has no public default constructor"),
          "asClass" -> Data("<compatible>")
        )
      }

      test("a case object is compatible as SingletonValue and Enum-rejection points to SingletonValue for CaseClass") {
        testClassViewParseReasons[examples.enums.ExampleSealedTrait.ExampleSealedTraitObject.type] <==> Data.map(
          "asSingleton" -> Data("<compatible>"),
          "asNamedTuple" -> Data("is not a named tuple"),
          "asCaseClass" -> Data("is a singleton, use SingletonValue instead of CaseClass"),
          "asEnum" -> Data("is not sealed, not an enumeration, and not a union type"),
          "asJavaBean" -> Data("is not a plain old Java object"),
          "asClass" -> Data("<compatible>")
        )
      }

      test("a sealed trait is compatible as Enum but rejected by the value-shaped views") {
        testClassViewParseReasons[examples.enums.ExampleSealedTrait] <==> Data.map(
          "asSingleton" -> Data("is not a singleton type"),
          "asNamedTuple" -> Data("is not a named tuple"),
          "asCaseClass" -> Data("is not a case class"),
          "asEnum" -> Data("<compatible>"),
          "asJavaBean" -> Data("is not a plain old Java object"),
          "asClass" -> Data("<compatible>")
        )
      }
    }

    // CaseClass.copyMethod returns the canonical, compiler-synthesized `copy` (matching the primary
    // constructor's value-parameter clauses), filtered by an availability scope that defaults to AtCallSite.
    group("CaseClass[A].copyMethod") {
      import ClassesFixtures.testCaseClassCopyMethod

      test("finds the canonical copy for a simple case class under every visibility") {
        testCaseClassCopyMethod[examples.classes.ExampleCaseClass] <==> Data.map(
          "atCallSite" -> Data("(a)"),
          "everywhere" -> Data("(a)"),
          "anywhere" -> Data("(a)")
        )
      }

      test("matches multiple parameter lists of the primary constructor") {
        testCaseClassCopyMethod[examples.classes.ExampleCaseClassMultipleParamLists] <==> Data.map(
          "atCallSite" -> Data("(i)(s)"),
          "everywhere" -> Data("(i)(s)"),
          "anywhere" -> Data("(i)(s)")
        )
      }

      test("finds the canonical copy for a generic case class (untyped params known before type args)") {
        testCaseClassCopyMethod[examples.classes.ExampleCaseClassWithTypeParam[Int]] <==> Data.map(
          "atCallSite" -> Data("(a)"),
          "everywhere" -> Data("(a)"),
          "anywhere" -> Data("(a)")
        )
      }

      test("returns None for a vararg case class (no copy is synthesized on either platform)") {
        // Neither Scala 2 nor Scala 3 generates `copy` for a case class with a repeated parameter, so there is no
        // canonical copy to return regardless of visibility.
        testCaseClassCopyMethod[examples.classes.ExampleCaseClassWithVarargs] <==> Data.map(
          "atCallSite" -> Data("<none>"),
          "everywhere" -> Data("<none>"),
          "anywhere" -> Data("<none>")
        )
      }

      test("respects the availability scope for a private-constructor copy") {
        // A private primary constructor makes `copy` non-public on Scala 3 (dropped by AtCallSite/Everywhere, kept
        // by Anywhere), whereas Scala 2.13 leaves `copy` public. Either way, `copyMethod` reflects the platform's
        // own accessibility via the availability filter.
        val expected =
          if (LanguageVersion.byHearth.isScala2_13)
            Data.map("atCallSite" -> Data("(a)"), "everywhere" -> Data("(a)"), "anywhere" -> Data("(a)"))
          else
            Data.map("atCallSite" -> Data("<none>"), "everywhere" -> Data("<none>"), "anywhere" -> Data("(a)"))
        testCaseClassCopyMethod[examples.classes.ExampleCaseClassPrivateCopy] <==> expected
      }

      test("an abstract case class has no synthesized copy under any visibility") {
        testCaseClassCopyMethod[examples.classes.ExampleSealedAbstractCaseClass] <==> Data.map(
          "atCallSite" -> Data("<none>"),
          "everywhere" -> Data("<none>"),
          "anywhere" -> Data("<none>")
        )
      }
    }

    test("CaseClass[A].copyMethod round-trip: override an Int field, keep the rest at their defaults") {
      import ClassesFixtures.testCaseClassCopyRoundTrip

      testCaseClassCopyRoundTrip(hearth.examples.classes.ExampleCaseClass(1)) <==> "ExampleCaseClass(99)"
      testCaseClassCopyRoundTrip(hearth.examples.classes.ExampleCaseClassWithDefaults(1, "kept")) <==>
        "ExampleCaseClassWithDefaults(99,kept)"
    }

    test("Enum[A].{matchOn and parMatchOn} should match on the sealed trait") {
      import ClassesFixtures.testEnumMatchOnAndParMatchOn

      def code(input: hearth.examples.enums.ExampleSealedTrait) = testEnumMatchOnAndParMatchOn(input)
      code(hearth.examples.enums.ExampleSealedTrait.ExampleSealedTraitClass(1)) <==>
        "sequential: subtype name: hearth.examples.enums.ExampleSealedTrait.ExampleSealedTraitClass, expr: ExampleSealedTraitClass, parallel: subtype name: hearth.examples.enums.ExampleSealedTrait.ExampleSealedTraitClass, expr: ExampleSealedTraitClass"
      code(hearth.examples.enums.ExampleSealedTrait.ExampleSealedTraitObject) <==>
        "sequential: subtype name: hearth.examples.enums.ExampleSealedTrait.ExampleSealedTraitObject.type, expr: ExampleSealedTraitObject, parallel: subtype name: hearth.examples.enums.ExampleSealedTrait.ExampleSealedTraitObject.type, expr: ExampleSealedTraitObject"
    }

    group("Class.apply picks the most specific view") {
      import ClassesFixtures.testClassViewKind

      test("for a case class, a sealed hierarchy, objects, a bean and a plain trait") {
        testClassViewKind[examples.classesedge.CCDeclarationOrder] <==> "CaseClass"
        testClassViewKind[examples.classesedge.Animal] <==> "Enum"
        testClassViewKind[examples.classesedge.Animal.Cat.type] <==> "SingletonValue"
        testClassViewKind[examples.classesedge.PlainObject.type] <==> "SingletonValue"
        testClassViewKind[examples.classesedge.ScalaBean] <==> "JavaBean"
        testClassViewKind[examples.classes.ExampleTrait] <==> "Class"
      }
    }

    group("Class[A].method(name)") {
      import ClassesFixtures.testClassMethodOverloads

      test("returns every overload with the given name, and nothing for an unknown name") {
        testClassMethodOverloads[examples.classesedge.ClassWithOverloads]("foo") <==> Data.list(
          Data("(s: java.lang.String)"),
          Data("(i: scala.Int)")
        )
        testClassMethodOverloads[examples.classesedge.ClassWithOverloads]("bar") <==> Data.list(Data(""))
        testClassMethodOverloads[examples.classesedge.ClassWithOverloads]("missing") <==> Data.list()
      }
    }

    group("CaseClass[A] constructors and case fields") {
      import ClassesFixtures.testCaseClassSummary

      test("case fields keep declaration order") {
        testCaseClassSummary[examples.classesedge.CCDeclarationOrder] <==> Data.map(
          "primaryConstructor" -> Data("(zeta: scala.Int, alpha: java.lang.String, mid: scala.Boolean)"),
          "nonPrimaryConstructors" -> Data.list(),
          "caseFields" -> Data.list(
            Data("zeta: scala.Int"),
            Data("alpha: java.lang.String"),
            Data("mid: scala.Boolean")
          )
        )
      }

      test("nonPrimaryConstructors does not contain the primary constructor") {
        testCaseClassSummary[examples.classes.ExampleCaseClass] <==> Data.map(
          "primaryConstructor" -> Data("(a: scala.Int)"),
          "nonPrimaryConstructors" -> Data.list(),
          "caseFields" -> Data.list(Data("a: scala.Int"))
        )
      }

      test("nonPrimaryConstructors contains only the auxiliary constructors") {
        testCaseClassSummary[examples.classesedge.CCWithAuxiliaryCtor] <==> Data.map(
          "primaryConstructor" -> Data("(i: scala.Int, s: java.lang.String)"),
          "nonPrimaryConstructors" -> Data.list(Data("(i: scala.Int)")),
          "caseFields" -> Data.list(Data("i: scala.Int"), Data("s: java.lang.String"))
        )
      }

      test("a case class without fields") {
        testCaseClassSummary[examples.classesedge.CCEmpty] <==> Data.map(
          "primaryConstructor" -> Data("()"),
          "nonPrimaryConstructors" -> Data.list(),
          "caseFields" -> Data.list()
        )
      }

      test("a generic case class seen through an alias reordering its type parameters") {
        testCaseClassSummary[examples.classesedge.CCGeneric2.Flipped[String, Int]] <==> Data.map(
          "primaryConstructor" -> Data("(a: scala.Int, b: java.lang.String)"),
          "nonPrimaryConstructors" -> Data.list(),
          "caseFields" -> Data.list(Data("a: scala.Int"), Data("b: java.lang.String"))
        )
      }

      test("only the first parameter list holds case fields, but the constructor has all of them") {
        testCaseClassSummary[examples.classesedge.CCMultipleParamLists] <==> Data.map(
          "primaryConstructor" -> Data("(i: scala.Int)(s: java.lang.String)"),
          "nonPrimaryConstructors" -> Data.list(),
          "caseFields" -> Data.list(Data("i: scala.Int"))
        )
        testCaseClassSummary[examples.classesedge.CCImplicitParam] <==> Data.map(
          "primaryConstructor" -> Data("(i: scala.Int)(s: java.lang.String)"),
          "nonPrimaryConstructors" -> Data.list(),
          "caseFields" -> Data.list(Data("i: scala.Int"))
        )
      }

      test("a value class") {
        testCaseClassSummary[examples.classesedge.CCValueClass] <==> Data.map(
          "primaryConstructor" -> Data("(value: scala.Int)"),
          "nonPrimaryConstructors" -> Data.list(),
          "caseFields" -> Data.list(Data("value: scala.Int"))
        )
      }
    }

    group("CaseClass[A].{construct, parConstruct, caseFieldValuesAt} evaluated at runtime") {
      import ClassesFixtures.testCaseClassRuntimeRoundTrip

      test("for a case class with fields in non-alphabetical order") {
        testCaseClassRuntimeRoundTrip[examples.classesedge.CCDeclarationOrder] <==>
          "CCDeclarationOrder(42,hello,true) | CCDeclarationOrder(42,hello,true) | zeta=42, alpha=hello, mid=true"
      }

      test("for a type alias of a case class") {
        testCaseClassRuntimeRoundTrip[examples.classesedge.CCAliases.Plain] <==>
          "CCDeclarationOrder(42,hello,true) | CCDeclarationOrder(42,hello,true) | zeta=42, alpha=hello, mid=true"
      }

      test("for a case class without fields") {
        testCaseClassRuntimeRoundTrip[examples.classesedge.CCEmpty] <==> "CCEmpty() | CCEmpty() | <no fields>"
      }

      test("for generic case classes, also through aliases that reorder or fix type parameters") {
        testCaseClassRuntimeRoundTrip[examples.classes.ExampleCaseClassWithTypeParam[String]] <==>
          "ExampleCaseClassWithTypeParam(hello) | ExampleCaseClassWithTypeParam(hello) | a=hello"
        testCaseClassRuntimeRoundTrip[examples.classesedge.CCGeneric2[Int, String]] <==>
          "CCGeneric2(42,hello) | CCGeneric2(42,hello) | a=42, b=hello"
        testCaseClassRuntimeRoundTrip[examples.classesedge.CCGeneric2.Flipped[String, Int]] <==>
          "CCGeneric2(42,hello) | CCGeneric2(42,hello) | a=42, b=hello"
        testCaseClassRuntimeRoundTrip[examples.classesedge.CCGeneric2.IntFirst[Boolean]] <==>
          "CCGeneric2(42,true) | CCGeneric2(42,true) | a=42, b=true"
      }

      test("for a case class with default values (every parameter is still provided)") {
        testCaseClassRuntimeRoundTrip[examples.classes.ExampleCaseClassWithDefaults] <==>
          "ExampleCaseClassWithDefaults(42,hello) | ExampleCaseClassWithDefaults(42,hello) | a=42, b=hello"
      }

      test("for a case class with multiple parameter lists") {
        testCaseClassRuntimeRoundTrip[examples.classesedge.CCMultipleParamLists] <==>
          "CCMultipleParamLists(42)(hello) | CCMultipleParamLists(42)(hello) | i=42"
      }

      test("for a case class with an implicit parameter list") {
        testCaseClassRuntimeRoundTrip[examples.classesedge.CCImplicitParam] <==>
          "CCImplicitParam(42)(hello) | CCImplicitParam(42)(hello) | i=42"
      }

      test("for a value class") {
        testCaseClassRuntimeRoundTrip[examples.classesedge.CCValueClass] <==>
          "CCValueClass(42) | CCValueClass(42) | value=42"
      }

      test("for a case class nested in an object") {
        testCaseClassRuntimeRoundTrip[examples.classesedge.CCOuter.CCNested] <==>
          "CCNested(42) | CCNested(42) | i=42"
      }

      test("for a path-dependent case class nested in a class instance") {
        testCaseClassRuntimeRoundTrip[examples.classesedge.CCContainerInstance.CCInner] <==>
          "CCInner(42,hello) | CCInner(42,hello) | i=42, s=hello"
      }

      test("for a case class defined locally at the call site") {
        case class LocalCaseClass(i: Int, s: String)
        testCaseClassRuntimeRoundTrip[LocalCaseClass] <==>
          "LocalCaseClass(42,hello) | LocalCaseClass(42,hello) | i=42, s=hello"
      }
    }

    group("CaseClass[A].construct visibility") {
      import ClassesFixtures.testCaseClassConstructVisibility

      test("a public constructor is available in every scope") {
        testCaseClassConstructVisibility[examples.classesedge.CCDeclarationOrder] <==> Data.map(
          "atCallSite" -> Data("<constructed>"),
          "everywhere" -> Data("<constructed>"),
          "anywhere" -> Data("<constructed>")
        )
      }

      test("a private constructor is available only when accessibility is ignored") {
        testCaseClassConstructVisibility[examples.classesedge.CCPrivateCtor] <==> Data.map(
          "atCallSite" -> Data("<inaccessible>"),
          "everywhere" -> Data("<inaccessible>"),
          "anywhere" -> Data("<constructed>")
        )
      }

      test("a package-private constructor is available at a call site inside that package, but is not public") {
        testCaseClassConstructVisibility[examples.classesedge.CCHearthPrivateCtor] <==> Data.map(
          "atCallSite" -> Data("<constructed>"),
          "everywhere" -> Data("<inaccessible>"),
          "anywhere" -> Data("<constructed>")
        )
      }
    }

    test("CaseClass[A].parConstruct aggregates errors of all fields, while construct stops at the first one") {
      import ClassesFixtures.testCaseClassConstructErrorAggregation

      testCaseClassConstructErrorAggregation[examples.classesedge.CCDeclarationOrder] <==> Data.map(
        "construct" -> Data.list(Data("no value for zeta")),
        "parConstruct" -> Data.list(
          Data("no value for zeta"),
          Data("no value for alpha"),
          Data("no value for mid")
        )
      )
    }

    group("Enum[A] children") {
      import ClassesFixtures.testEnumChildren

      test("for a sealed abstract class") {
        testEnumChildren[examples.classesedge.SealedAbstractShape] <==> Data.map(
          "directChildren" -> Data.list(
            Data("Circle: hearth.examples.classesedge.SealedAbstractShape.Circle"),
            Data("Dot: hearth.examples.classesedge.SealedAbstractShape.Dot.type")
          ),
          "exhaustiveChildren" -> Data.list(
            Data("Circle: hearth.examples.classesedge.SealedAbstractShape.Circle"),
            Data("Dot: hearth.examples.classesedge.SealedAbstractShape.Dot.type")
          )
        )
      }

      test("for a nested sealed hierarchy: direct children keep the nested trait, exhaustive children flatten it") {
        testEnumChildren[examples.classesedge.Animal] <==> Data.map(
          "directChildren" -> Data.list(
            Data("Fish: hearth.examples.classesedge.Animal.Fish"),
            Data("Mammal: hearth.examples.classesedge.Animal.Mammal")
          ),
          "exhaustiveChildren" -> Data.list(
            Data("Fish: hearth.examples.classesedge.Animal.Fish"),
            Data("Cat: hearth.examples.classesedge.Animal.Cat.type"),
            Data("Dog: hearth.examples.classesedge.Animal.Dog")
          )
        )
      }

      test("for an applied generic sealed trait") {
        testEnumChildren[examples.enums.ExampleSealedTraitWithTypeParam[Int]] <==> Data.map(
          "directChildren" -> Data.list(
            Data(
              "ExampleSealedTraitWithTypeParamClass: hearth.examples.enums.ExampleSealedTraitWithTypeParam.ExampleSealedTraitWithTypeParamClass[scala.Int]"
            ),
            Data(
              "ExampleSealedTraitWithTypeParamObject: hearth.examples.enums.ExampleSealedTraitWithTypeParam.ExampleSealedTraitWithTypeParamObject.type"
            )
          ),
          "exhaustiveChildren" -> Data.list(
            Data(
              "ExampleSealedTraitWithTypeParamClass: hearth.examples.enums.ExampleSealedTraitWithTypeParam.ExampleSealedTraitWithTypeParamClass[scala.Int]"
            ),
            Data(
              "ExampleSealedTraitWithTypeParamObject: hearth.examples.enums.ExampleSealedTraitWithTypeParam.ExampleSealedTraitWithTypeParamObject.type"
            )
          )
        )
      }

      test("for a Scala Enumeration") {
        val days = List("Fri", "Mon", "Sat", "Sun", "Thu", "Tue", "Wed")
          .map(day => Data(s"$day: hearth.examples.enums.WeekDay.$day.type"))
        testEnumChildren[examples.enums.WeekDay.Value] <==> Data.map(
          "directChildren" -> Data(days),
          "exhaustiveChildren" -> Data(days)
        )
      }

      test("a sealed trait without children is not an Enum") {
        testEnumChildren[examples.classesedge.SealedWithoutChildren] <==> Data("<no enum>")
      }
    }

    group("Enum[A].{matchOn and parMatchOn} evaluated at runtime") {
      import ClassesFixtures.testEnumMatchOnRuntime

      test("for a sealed abstract class") {
        def code(input: examples.classesedge.SealedAbstractShape) = testEnumMatchOnRuntime(input)
        code(examples.classesedge.SealedAbstractShape.Circle(1)) <==>
          "hearth.examples.classesedge.SealedAbstractShape.Circle: Circle(1) / hearth.examples.classesedge.SealedAbstractShape.Circle: Circle(1)"
        code(examples.classesedge.SealedAbstractShape.Dot) <==>
          "hearth.examples.classesedge.SealedAbstractShape.Dot.type: Dot / hearth.examples.classesedge.SealedAbstractShape.Dot.type: Dot"
      }

      test("for a nested sealed hierarchy, matching on the direct children") {
        def code(input: examples.classesedge.Animal) = testEnumMatchOnRuntime(input)
        code(examples.classesedge.Animal.Dog("rex")) <==>
          "hearth.examples.classesedge.Animal.Mammal: Dog(rex) / hearth.examples.classesedge.Animal.Mammal: Dog(rex)"
        code(examples.classesedge.Animal.Cat) <==>
          "hearth.examples.classesedge.Animal.Mammal: Cat / hearth.examples.classesedge.Animal.Mammal: Cat"
        code(examples.classesedge.Animal.Fish("nemo")) <==>
          "hearth.examples.classesedge.Animal.Fish: Fish(nemo) / hearth.examples.classesedge.Animal.Fish: Fish(nemo)"
      }

      test("for an applied generic sealed trait") {
        def code(input: examples.enums.ExampleSealedTraitWithTypeParam[Int]) = testEnumMatchOnRuntime(input)
        code(examples.enums.ExampleSealedTraitWithTypeParam.ExampleSealedTraitWithTypeParamClass(5)) <==>
          "hearth.examples.enums.ExampleSealedTraitWithTypeParam.ExampleSealedTraitWithTypeParamClass[scala.Int]: ExampleSealedTraitWithTypeParamClass(5) / hearth.examples.enums.ExampleSealedTraitWithTypeParam.ExampleSealedTraitWithTypeParamClass[scala.Int]: ExampleSealedTraitWithTypeParamClass(5)"
        code(examples.enums.ExampleSealedTraitWithTypeParam.ExampleSealedTraitWithTypeParamObject) <==>
          "hearth.examples.enums.ExampleSealedTraitWithTypeParam.ExampleSealedTraitWithTypeParamObject.type: ExampleSealedTraitWithTypeParamObject / hearth.examples.enums.ExampleSealedTraitWithTypeParam.ExampleSealedTraitWithTypeParamObject.type: ExampleSealedTraitWithTypeParamObject"
      }

      test("for a Scala Enumeration") {
        def code(input: examples.enums.WeekDay.Value) = testEnumMatchOnRuntime(input)
        code(examples.enums.WeekDay.Tue) <==>
          "hearth.examples.enums.WeekDay.Tue.type: Tue / hearth.examples.enums.WeekDay.Tue.type: Tue"
        code(examples.enums.WeekDay.Sun) <==>
          "hearth.examples.enums.WeekDay.Sun.type: Sun / hearth.examples.enums.WeekDay.Sun.type: Sun"
      }
    }

    group("SingletonValue[A] evaluated at runtime") {
      import ClassesFixtures.testSingletonRoundTrip

      test("for a plain object, a case object, a nested case object and None") {
        testSingletonRoundTrip[examples.classesedge.PlainObject.type] <==> "PlainObject"
        testSingletonRoundTrip[examples.classesedge.SealedAbstractShape.Dot.type] <==> "Dot"
        testSingletonRoundTrip[examples.classesedge.Animal.Cat.type] <==> "Cat"
        testSingletonRoundTrip[None.type] <==> "None"
      }

      test("for a Scala Enumeration value") {
        testSingletonRoundTrip[examples.enums.WeekDay.Mon.type] <==> "Mon"
      }

      test("not for literal singleton types") {
        testSingletonRoundTrip[42] <==> "<no singleton>"
        testSingletonRoundTrip["abc"] <==> "<no singleton>"
        testSingletonRoundTrip[true] <==> "<no singleton>"
      }

      test("not for a case class, a non-singleton class or Unit") {
        testSingletonRoundTrip[examples.classesedge.CCDeclarationOrder] <==> "<no singleton>"
        testSingletonRoundTrip[examples.classesedge.ScalaBean] <==> "<no singleton>"
        testSingletonRoundTrip[Unit] <==> "<no singleton>"
      }
    }

    group("class-view parse: Incompatible reasons for edge cases") {
      import ClassesFixtures.testClassViewParseReasons

      test("a sealed trait without children is rejected by Enum") {
        testClassViewParseReasons[examples.classesedge.SealedWithoutChildren] <==> Data.map(
          "asSingleton" -> Data("is not a singleton type"),
          "asNamedTuple" -> Data("is not a named tuple"),
          "asCaseClass" -> Data("is not a case class"),
          "asEnum" -> Data("is sealed/enumeration/union but has no direct children"),
          "asJavaBean" -> Data("is not a plain old Java object"),
          "asClass" -> Data("<compatible>")
        )
      }

      test("an abstract class is not a JavaBean, since it cannot be instantiated") {
        testClassViewParseReasons[examples.classes.ExampleAbstractClass] <==> Data.map(
          "asSingleton" -> Data("is not a singleton type"),
          "asNamedTuple" -> Data("is not a named tuple"),
          "asCaseClass" -> Data("is not a case class"),
          "asEnum" -> Data("is not sealed, not an enumeration, and not a union type"),
          "asJavaBean" -> Data("is not a plain old Java object"),
          "asClass" -> Data("<compatible>")
        )
      }

      test("a plain object is a SingletonValue only") {
        testClassViewParseReasons[examples.classesedge.PlainObject.type] <==> Data.map(
          "asSingleton" -> Data("<compatible>"),
          "asNamedTuple" -> Data("is not a named tuple"),
          "asCaseClass" -> Data("is not a case class"),
          "asEnum" -> Data("is not sealed, not an enumeration, and not a union type"),
          "asJavaBean" -> Data("is not a plain old Java object"),
          "asClass" -> Data("<compatible>")
        )
      }

      test("a class whose default constructor is not public is not a JavaBean") {
        testClassViewParseReasons[examples.classesedge.ScalaBeanHearthPrivateCtor] <==> Data.map(
          "asSingleton" -> Data("is not a singleton type"),
          "asNamedTuple" -> Data("is not a named tuple"),
          "asCaseClass" -> Data("is not a case class"),
          "asEnum" -> Data("is not sealed, not an enumeration, and not a union type"),
          "asJavaBean" -> Data("is a POJO but has no public default constructor"),
          "asClass" -> Data("<compatible>")
        )
      }
    }

    group("JavaBean[A] for a class with Scala-defined getters and setters") {
      import ClassesFixtures.{
        testJavaBeanConstructRoundTrip,
        testJavaBeanConstructVisibility,
        testJavaBeanConstructWithoutSettersRoundTrip,
        testJavaBeanSetterErrorAggregation,
        testJavaBeanSummary
      }

      test("beanGetters include boolean `is` getters, beanSetters are listed with their parameters") {
        testJavaBeanSummary[examples.classesedge.ScalaBean] <==> Data.map(
          "defaultConstructor" -> Data("()"),
          "beanGetters" -> Data.list(Data("getCount"), Data("getName"), Data("isActive")),
          "beanSetters" -> Data.list(
            Data.map("name" -> Data("setActive"), "parameters" -> Data("(active: scala.Boolean)")),
            Data.map("name" -> Data("setCount"), "parameters" -> Data("(count: scala.Int)")),
            Data.map("name" -> Data("setName"), "parameters" -> Data("(name: java.lang.String)"))
          )
        )
      }

      test("constructWithoutSetters uses only the default constructor") {
        testJavaBeanConstructWithoutSettersRoundTrip[examples.classesedge.ScalaBean] <==> "ScalaBean(, 0, false)"
      }

      test("constructWithSetters calls every setter") {
        testJavaBeanConstructRoundTrip[examples.classesedge.ScalaBean] <==> "ScalaBean(hello, 42, true)"
      }

      test("constructWithoutSetters is available in every scope for a public default constructor") {
        testJavaBeanConstructVisibility[examples.classesedge.ScalaBean] <==> Data.map(
          "atCallSite" -> Data("<constructed>"),
          "everywhere" -> Data("<constructed>"),
          "anywhere" -> Data("<constructed>")
        )
      }

      test("parConstructWithSetters aggregates errors of all setters, while constructWithSetters stops at the first") {
        testJavaBeanSetterErrorAggregation[examples.classesedge.ScalaBean] <==> Data.map(
          "constructWithSetters (number of errors)" -> Data(1),
          "parConstructWithSetters" -> Data.list(
            Data("no value for active"),
            Data("no value for count"),
            Data("no value for name")
          ),
          "parConstructWithSetters(SetField)" -> Data.list(
            Data("no value for active"),
            Data("no value for count"),
            Data("no value for name")
          )
        )
      }
    }
  }
}

object ClassesSpec {

  /** The children are declared in an order (`Red, Yellow, Green`) that differs from their alphabetical order (`Green,
    * Red, Yellow`), so the test can tell declaration order apart from an accidental alphabetical sort. Defined in the
    * test sources (same compilation unit as the spec) so both Scala 2 and Scala 3 resolve it identically.
    */
  sealed trait TrafficLight
  object TrafficLight {
    case object Red extends TrafficLight
    case object Yellow extends TrafficLight
    case object Green extends TrafficLight
  }
}
