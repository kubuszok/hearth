package hearth
package typed

import hearth.data.Data

final class DestructuredExprsSpec extends MacroSuite {

  group("typed.DestructuredExpr") {

    group("parseDetailed") {
      import DestructuredExprsFixtures.testParseDetailed

      test("literal Int") {
        testParseDetailed(42) <==> Data.map(
          "nodeType" -> Data("Literal"),
          "value" -> Data("42")
        )
      }

      test("literal String") {
        testParseDetailed("hello") <==> Data.map(
          "nodeType" -> Data("Literal"),
          "value" -> Data("\"hello\"")
        )
      }

      test("literal Boolean") {
        testParseDetailed(true) <==> Data.map(
          "nodeType" -> Data("Literal"),
          "value" -> Data("true")
        )
      }

      test("lambda with ParamRef body") {
        testParseDetailed((x: Int) => x) <==> Data.map(
          "nodeType" -> Data("Lambda"),
          "params" -> Data.list(Data.map("name" -> Data("x"), "type" -> Data("scala.Int"))),
          "body" -> Data.map("nodeType" -> Data("ParamRef"), "paramName" -> Data("x"))
        )
      }

      test("lambda with single field access") {
        testParseDetailed((p: examples.parsed_exprs.Person) => p.name) <==> Data.map(
          "nodeType" -> Data("Lambda"),
          "params" -> Data.list(
            Data.map("name" -> Data("p"), "type" -> Data("hearth.examples.parsed_exprs.Person"))
          ),
          "body" -> Data.map(
            "nodeType" -> Data("MethodCall"),
            "methodName" -> Data("name"),
            "returnType" -> Data("java.lang.String"),
            "applied" -> Data.list(
              Data.map(
                "kind" -> Data("Instance"),
                "value" -> Data.map("nodeType" -> Data("ParamRef"), "paramName" -> Data("p"))
              )
            )
          )
        )
      }

      test("lambda with chained field access") {
        testParseDetailed((p: examples.parsed_exprs.Person) => p.address.street) <==> Data.map(
          "nodeType" -> Data("Lambda"),
          "params" -> Data.list(
            Data.map("name" -> Data("p"), "type" -> Data("hearth.examples.parsed_exprs.Person"))
          ),
          "body" -> Data.map(
            "nodeType" -> Data("MethodCall"),
            "methodName" -> Data("street"),
            "returnType" -> Data("java.lang.String"),
            "applied" -> Data.list(
              Data.map(
                "kind" -> Data("Instance"),
                "value" -> Data.map(
                  "nodeType" -> Data("MethodCall"),
                  "methodName" -> Data("address"),
                  "returnType" -> Data("hearth.examples.parsed_exprs.Address"),
                  "applied" -> Data.list(
                    Data.map(
                      "kind" -> Data("Instance"),
                      "value" -> Data.map("nodeType" -> Data("ParamRef"), "paramName" -> Data("p"))
                    )
                  )
                )
              )
            )
          )
        )
      }
    }

    group("parseDetailed: vararg call sites") {
      import DestructuredExprsFixtures.testParseDetailed

      // The vararg slot is represented as ONE argument in AppliedValues (matching Method.ApplyValues, which expects
      // a single Expr[Seq[A]] for a vararg parameter): individual elements become a Varargs node (elements
      // recoverable), a spread sequence (`seq*`) becomes the destructured sequence expression itself.

      test("vararg call with individual elements becomes one Varargs slot") {
        testParseDetailed((w: examples.methods.WithVarargs) => w.varargMethod(1, 2, 3)) <==> Data.map(
          "nodeType" -> Data("Lambda"),
          "params" -> Data.list(
            Data.map("name" -> Data("w"), "type" -> Data("hearth.examples.methods.WithVarargs"))
          ),
          "body" -> Data.map(
            "nodeType" -> Data("MethodCall"),
            "methodName" -> Data("varargMethod"),
            "returnType" -> Data("scala.Int"),
            "applied" -> Data.list(
              Data.map(
                "kind" -> Data("Instance"),
                "value" -> Data.map("nodeType" -> Data("ParamRef"), "paramName" -> Data("w"))
              ),
              Data.map(
                "kind" -> Data("Values"),
                "args" -> Data.list(
                  Data.map(
                    "nodeType" -> Data("Varargs"),
                    "type" -> Data("scala.collection.immutable.Seq[scala.Int]"),
                    "elements" -> Data.list(
                      Data.map("nodeType" -> Data("Literal"), "value" -> Data("1")),
                      Data.map("nodeType" -> Data("Literal"), "value" -> Data("2")),
                      Data.map("nodeType" -> Data("Literal"), "value" -> Data("3"))
                    )
                  )
                )
              )
            )
          )
        )
      }

      test("vararg call with no elements becomes an empty Varargs slot") {
        testParseDetailed((w: examples.methods.WithVarargs) => w.varargMethod()) <==> Data.map(
          "nodeType" -> Data("Lambda"),
          "params" -> Data.list(
            Data.map("name" -> Data("w"), "type" -> Data("hearth.examples.methods.WithVarargs"))
          ),
          "body" -> Data.map(
            "nodeType" -> Data("MethodCall"),
            "methodName" -> Data("varargMethod"),
            "returnType" -> Data("scala.Int"),
            "applied" -> Data.list(
              Data.map(
                "kind" -> Data("Instance"),
                "value" -> Data.map("nodeType" -> Data("ParamRef"), "paramName" -> Data("w"))
              ),
              Data.map(
                "kind" -> Data("Values"),
                "args" -> Data.list(
                  Data.map(
                    "nodeType" -> Data("Varargs"),
                    "type" -> Data("scala.collection.immutable.Seq[scala.Int]"),
                    "elements" -> Data.list()
                  )
                )
              )
            )
          )
        )
      }

      test("vararg call with spread sequence becomes the sequence expression itself") {
        testParseDetailed((w: examples.methods.WithVarargs, xs: Seq[Int]) => w.varargMethod(xs*)) <==> Data.map(
          "nodeType" -> Data("Lambda"),
          "params" -> Data.list(
            Data.map("name" -> Data("w"), "type" -> Data("hearth.examples.methods.WithVarargs")),
            Data.map("name" -> Data("xs"), "type" -> Data("scala.collection.immutable.Seq[scala.Int]"))
          ),
          "body" -> Data.map(
            "nodeType" -> Data("MethodCall"),
            "methodName" -> Data("varargMethod"),
            "returnType" -> Data("scala.Int"),
            "applied" -> Data.list(
              Data.map(
                "kind" -> Data("Instance"),
                "value" -> Data.map("nodeType" -> Data("ParamRef"), "paramName" -> Data("w"))
              ),
              Data.map(
                "kind" -> Data("Values"),
                "args" -> Data.list(
                  Data.map("nodeType" -> Data("ParamRef"), "paramName" -> Data("xs"))
                )
              )
            )
          )
        )
      }

      test("vararg constructor call with individual elements becomes one Varargs slot") {
        testParseDetailed(new examples.methods.WithVarargsCtor("a", "b")) <==> Data.map(
          "nodeType" -> Data("MethodCall"),
          "methodName" -> Data("<init>"),
          "returnType" -> Data("hearth.examples.methods.WithVarargsCtor"),
          "applied" -> Data.list(
            Data.map(
              "kind" -> Data("Values"),
              "args" -> Data.list(
                Data.map(
                  "nodeType" -> Data("Varargs"),
                  "type" -> Data("scala.collection.immutable.Seq[java.lang.String]"),
                  "elements" -> Data.list(
                    Data.map("nodeType" -> Data("Literal"), "value" -> Data("\"a\"")),
                    Data.map("nodeType" -> Data("Literal"), "value" -> Data("\"b\""))
                  )
                )
              )
            )
          )
        )
      }
    }

    group("outermost MethodCall: DSL patterns with implicit evidence") {
      import DestructuredExprsFixtures.testOutermostMethodCall
      import examples.parsed_exprs.dsl.*

      test(".each has Instance and Values applied") {
        testOutermostMethodCall((c: examples.parsed_exprs.Container) => c.items.each) <==> Data.map(
          "methodName" -> Data("each"),
          "appliedKinds" -> Data.list(Data("Instance"), Data("Values"))
        )
      }

      test(".when[Subtype] has Instance and Types applied") {
        testOutermostMethodCall((h: examples.parsed_exprs.AnimalHolder) =>
          h.animal.when[examples.parsed_exprs.Dog]
        ) <==> Data.map(
          "methodName" -> Data("when"),
          "appliedKinds" -> Data.list(Data("Instance"), Data("Types"))
        )
      }

      test("simple field access has only Instance applied") {
        testOutermostMethodCall((p: examples.parsed_exprs.Person) => p.name) <==> Data.map(
          "methodName" -> Data("name"),
          "appliedKinds" -> Data.list(Data("Instance"))
        )
      }

      test("chained field access outermost has only Instance applied") {
        testOutermostMethodCall((p: examples.parsed_exprs.Person) => p.address.street) <==> Data.map(
          "methodName" -> Data("street"),
          "appliedKinds" -> Data.list(Data("Instance"))
        )
      }
    }

    group("extractFieldPath") {
      import DestructuredExprsFixtures.testParseFieldPath

      test("single field: _.name") {
        testParseFieldPath((p: examples.parsed_exprs.Person) => p.name) <==> Data.map(
          "fieldNames" -> Data.list(Data("name")),
          "depth" -> Data(1),
          "plainPrint" -> Data("_.name"),
          "rootType" -> Data("hearth.examples.parsed_exprs.Person"),
          "leafType" -> Data("java.lang.String"),
          "segments" -> Data.list(
            Data.map(
              "name" -> Data("name"),
              "sourceType" -> Data("hearth.examples.parsed_exprs.Person"),
              "resultType" -> Data("java.lang.String"),
              "methodName" -> Data("name")
            )
          )
        )
      }

      test("chained fields: _.address.street") {
        testParseFieldPath((p: examples.parsed_exprs.Person) => p.address.street) <==> Data.map(
          "fieldNames" -> Data.list(Data("address"), Data("street")),
          "depth" -> Data(2),
          "plainPrint" -> Data("_.address.street"),
          "rootType" -> Data("hearth.examples.parsed_exprs.Person"),
          "leafType" -> Data("java.lang.String"),
          "segments" -> Data.list(
            Data.map(
              "name" -> Data("address"),
              "sourceType" -> Data("hearth.examples.parsed_exprs.Person"),
              "resultType" -> Data("hearth.examples.parsed_exprs.Address"),
              "methodName" -> Data("address")
            ),
            Data.map(
              "name" -> Data("street"),
              "sourceType" -> Data("hearth.examples.parsed_exprs.Address"),
              "resultType" -> Data("java.lang.String"),
              "methodName" -> Data("street")
            )
          )
        )
      }

      test("single field: _.address") {
        testParseFieldPath((p: examples.parsed_exprs.Person) => p.address) <==> Data.map(
          "fieldNames" -> Data.list(Data("address")),
          "depth" -> Data(1),
          "plainPrint" -> Data("_.address"),
          "rootType" -> Data("hearth.examples.parsed_exprs.Person"),
          "leafType" -> Data("hearth.examples.parsed_exprs.Address"),
          "segments" -> Data.list(
            Data.map(
              "name" -> Data("address"),
              "sourceType" -> Data("hearth.examples.parsed_exprs.Person"),
              "resultType" -> Data("hearth.examples.parsed_exprs.Address"),
              "methodName" -> Data("address")
            )
          )
        )
      }

      group("error cases") {
        import examples.parsed_exprs.dsl.*

        test("identity lambda (no field access)") {
          testParseFieldPath((p: examples.parsed_exprs.Person) => p) <==> Data.map(
            "error" -> Data("Empty field path - the lambda body must access at least one field")
          )
        }

        test(".each is not a simple field path (has implicit args)") {
          val result = testParseFieldPath((c: examples.parsed_exprs.Container) => c.items.each)
          assert(result.asMap.exists(_.contains("error")), ".each should produce an error for extractFieldPath")
        }
      }
    }

    group("extractLambda") {
      import DestructuredExprsFixtures.testParseLambda

      test("single-param lambda with ParamRef body") {
        testParseLambda((x: Int) => x) <==> Data.map(
          "params" -> Data.list(Data.map("name" -> Data("x"), "type" -> Data("scala.Int"))),
          "body" -> Data.map(
            "nodeType" -> Data("ParamRef"),
            "plainPrint" -> Data("x"),
            "paramName" -> Data("x")
          )
        )
      }

      test("single-param lambda with field access body") {
        testParseLambda((p: examples.parsed_exprs.Person) => p.name) <==> Data.map(
          "params" -> Data.list(
            Data.map("name" -> Data("p"), "type" -> Data("hearth.examples.parsed_exprs.Person"))
          ),
          "body" -> Data.map(
            "nodeType" -> Data("MethodCall"),
            "plainPrint" -> Data("namep"),
            "methodName" -> Data("name"),
            "appliedCount" -> Data(1)
          )
        )
      }
    }

    group("collect") {
      import DestructuredExprsFixtures.testCollectMethodCalls

      test("collects method calls from field chain (depth-first)") {
        testCollectMethodCalls((p: examples.parsed_exprs.Person) => p.address.street) <==>
          Data.list(Data("street"), Data("address"))
      }

      test("single field has one method call") {
        testCollectMethodCalls((p: examples.parsed_exprs.Person) => p.name) <==>
          Data.list(Data("name"))
      }
    }
  }
}
