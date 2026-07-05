package hearth
package typed

import hearth.data.Data

/** Scala-2-only: `DestructuredExpr.Lambda.Param.declaredTpe` preserves a singleton parameter type that the Scala 2
  * typer widens away in `tpe` (hearth#341). On Scala 3 the compiler retains it in `tpe`, so this asymmetry is
  * Scala-2-specific.
  */
final class DestructuredExprsScala2Spec extends MacroSuite {

  import DestructuredExprsFixtures.testParseLambda

  group("DestructuredExpr.Lambda.Param (Scala 2): declaredTpe preserves singletons (#341)") {

    test("a Java-enum-value singleton parameter is widened in `type` but preserved in `declaredType`") {
      testParseLambda((x: examples.enums.ExampleJavaEnum.VALUE1.type) => x) <==> Data.map(
        "params" -> Data.list(
          Data.map(
            "name" -> Data("x"),
            "type" -> Data("hearth.examples.enums.ExampleJavaEnum"),
            "declaredType" -> Data("hearth.examples.enums.ExampleJavaEnum.VALUE1.type")
          )
        ),
        "body" -> Data.map(
          "nodeType" -> Data("ParamRef"),
          "plainPrint" -> Data("x"),
          "paramName" -> Data("x")
        )
      )
    }
  }
}
