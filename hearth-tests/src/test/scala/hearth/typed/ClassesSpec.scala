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

    test("CaseClass[A].{construct and parConstruct} should construct an instance of the case class") {
      import ClassesFixtures.testCaseClassConstructAndParConstruct

      testCaseClassConstructAndParConstruct[examples.classes.ExampleCaseClass] <==>
        "sequential: new hearth.examples.classes.ExampleCaseClass(0), parallel: new hearth.examples.classes.ExampleCaseClass(0)"
    }

    test("CaseClass[A].caseFieldValuesAt should extract fields of the case class") {
      import ClassesFixtures.testCaseClassCaseFieldValuesAt

      testCaseClassCaseFieldValuesAt(hearth.examples.classes.ExampleCaseClass(0)) <==>
        "(a: hearth.examples.classes.ExampleCaseClass.apply(0).a)"
    }

    test("Enum[A].{matchOn and parMatchOn} should match on the sealed trait") {
      import ClassesFixtures.testEnumMatchOnAndParMatchOn

      def code(input: hearth.examples.enums.ExampleSealedTrait) = testEnumMatchOnAndParMatchOn(input)
      code(hearth.examples.enums.ExampleSealedTrait.ExampleSealedTraitClass(1)) <==>
        "sequential: subtype name: hearth.examples.enums.ExampleSealedTrait.ExampleSealedTraitClass, expr: ExampleSealedTraitClass, parallel: subtype name: hearth.examples.enums.ExampleSealedTrait.ExampleSealedTraitClass, expr: ExampleSealedTraitClass"
      code(hearth.examples.enums.ExampleSealedTrait.ExampleSealedTraitObject) <==>
        "sequential: subtype name: hearth.examples.enums.ExampleSealedTrait.ExampleSealedTraitObject.type, expr: ExampleSealedTraitObject, parallel: subtype name: hearth.examples.enums.ExampleSealedTrait.ExampleSealedTraitObject.type, expr: ExampleSealedTraitObject"
    }
  }
}
