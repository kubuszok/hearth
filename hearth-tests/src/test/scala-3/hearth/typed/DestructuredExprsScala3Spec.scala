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
  }
}
