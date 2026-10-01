package hearth
package typed

import hearth.data.Data

final class DestructuredExprsScala3Spec extends MacroSuite {

  group("typed.DestructuredExpr (Scala 3, context functions)") {

    import DestructuredExprsFixtures.testOutermostMethodCall
    import examples.parsed_exprs.dslContextFunctions.*

    test(".eachCF has Instance, Types (extension type params), and Values (given evidence) applied") {
      testOutermostMethodCall((c: examples.parsed_exprs.Container) => c.items.eachCF) <==> Data.map(
        "methodName" -> Data("eachCF"),
        "appliedKinds" -> Data.list(Data("Instance"), Data("Types"), Data("Values"))
      )
    }

    test(".whenCF[Subtype] has Instance and Types applied (extension type params + explicit type arg)") {
      testOutermostMethodCall((h: examples.parsed_exprs.AnimalHolder) =>
        h.animal.whenCF[examples.parsed_exprs.Dog]
      ) <==> Data.map(
        "methodName" -> Data("whenCF"),
        "appliedKinds" -> Data.list(Data("Instance"), Data("Types"), Data("Types"))
      )
    }

    test("MethodCall.receiver reads extension methods with the same code as implicit classes") {
      import DestructuredExprsFixtures.testReceiverChain
      testReceiverChain((c: examples.parsed_exprs.Container) => c.items.eachCF.length) <==> Data.list(
        Data("items"),
        Data("eachCF"),
        Data("length")
      )
    }

    test("skipContextualWrappers peels the contextual lambda of a context-function argument") {
      import DestructuredExprsFixtures.testSkipContextualWrappersOfSelector
      testSkipContextualWrappersOfSelector(_.name) <==> Data.map(
        "parsed" -> Data
          .map("node" -> Data("Lambda"), "contextual" -> Data(true), "params" -> Data.list(Data("<contextual>"))),
        "skipped" -> Data.map("node" -> Data("Lambda"), "contextual" -> Data(false), "params" -> Data.list(Data("_$1")))
      )
    }
  }
}
