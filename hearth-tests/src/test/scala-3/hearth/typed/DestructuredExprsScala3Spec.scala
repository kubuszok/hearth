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

    test("bindings of an inlined call are kept as a Block") {
      import DestructuredExprsFixtures.testParseBindings
      val proxy = Data.map(
        "node" -> Data("LocalReference"),
        "binding" -> Data(0),
        "name" -> Data("x$proxy1"),
        "external" -> Data(false)
      )
      testParseBindings(examples.parsed_exprs.inlining.twice(scala.util.Random.nextInt())) <==> Data.map(
        "node" -> Data("Block"),
        "statements" -> Data.list(
          Data.map(
            "node" -> Data("ValDefinition"),
            "binding" -> Data(0),
            "name" -> Data("x$proxy1"),
            "type" -> Data("scala.Int"),
            "flags" -> Data.list(),
            "hasPosition" -> Data(true),
            "rhs" -> Data.map(
              "node" -> Data("MethodCall"),
              "name" -> Data("nextInt"),
              "receiver" -> Data.map(
                "node" -> Data("MethodCall"),
                "name" -> Data("Random"),
                "receiver" -> Data.map(
                  "node" -> Data("MethodCall"),
                  "name" -> Data("util"),
                  "receiver" -> Data.map("node" -> Data("Other"), "plainPrint" -> Data("scala")),
                  "args" -> Data.list()
                ),
                "args" -> Data.list()
              ),
              "args" -> Data.list()
            )
          )
        ),
        "result" -> Data.map(
          "node" -> Data("MethodCall"),
          "name" -> Data("+"),
          "receiver" -> proxy,
          "args" -> Data.list(proxy)
        )
      )
    }
  }
}
