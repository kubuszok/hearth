package hearth
package typed

import hearth.data.Data
import hearth.examples.Color

final class EnumCaseChildrenSpec extends MacroSuite {

  group("Scala 3 parameterless enum cases") {
    import EnumCaseChildrenFixtures.{testEnumCase, testEnumCaseChildren}

    val enumCase = Data.map(
      "Mirror.Of" -> Data("Mirror.Singleton"),
      "ValueOf found" -> Data(true),
      "isSealed" -> Data(false),
      "isCaseClass" -> Data(false),
      "isCaseVal" -> Data(true)
    )

    test("from directChildren resolve the same implicits as the case's singleton type written in the source") {
      testEnumCaseChildren[Color] <==> Data.map("Red" -> enumCase, "Green" -> enumCase, "Blue" -> enumCase)
      testEnumCase[Color.Red.type] <==> enumCase
    }

    test("from directChildren keep the annotations of the case, not the enum's") {
      import EnumCaseChildrenFixtures.testEnumCaseChildrenAnnotations
      import hearth.examples.AnnotatedCasesEnum

      testEnumCaseChildrenAnnotations[AnnotatedCasesEnum] <==> Data.map(
        "AnnotatedValue" -> Data.list(Data("new hearth.examples.methods.ExampleAnnotation()")),
        "AnnotatedClass" -> Data.list(Data("new hearth.examples.methods.ExampleAnnotation2(2)")),
        "NotAnnotatedValue" -> Data.list()
      )
    }

    test("written as a singleton type keep the annotations of the case, not the enum's") {
      import EnumCaseChildrenFixtures.testEnumCaseAnnotations
      import hearth.examples.AnnotatedCasesEnum

      testEnumCaseAnnotations[AnnotatedCasesEnum.AnnotatedValue.type] <==> Data.list(
        Data("new hearth.examples.methods.ExampleAnnotation()")
      )
    }
  }
}
