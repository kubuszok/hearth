package hearth
package typed

import hearth.data.Data
import hearth.examples.destructurededge.*

final class DestructuredExprsEdgeCasesSpec extends MacroSuite {

  private def literal(value: String): Data = Data.map("nodeType" -> Data("Literal"), "value" -> Data(value))
  private def summary(node: String, plainPrint: String, tpe: String, line: Int = 0): Data = Data.map(
    "node" -> Data(node),
    "plainPrint" -> Data(plainPrint),
    "type" -> Data(tpe),
    "rebuiltConforms" -> Data(true),
    "line" -> Data(line)
  )

  group("typed.DestructuredExpr edge cases") {

    group("literals of every primitive kind") {
      import DestructuredExprsFixtures.{testNodeSummaries, testParseDetailed}

      test("are Literal nodes printed with their Scala syntax") {
        testParseDetailed(42L) <==> literal("42L")
        testParseDetailed('c') <==> literal("'c'")
        testParseDetailed(1.5f) <==> literal("1.5f")
        testParseDetailed(1.5) <==> literal("1.5")
        testParseDetailed(null) <==> literal("null")
        testParseDetailed(-7) <==> literal("-7")
        testParseDetailed("") <==> literal("\"\"")
      }

      test("have widened types") {
        testNodeSummaries(42) <==> Data.list(summary("Literal", "42", "scala.Int"))
        testNodeSummaries(42L) <==> Data.list(summary("Literal", "42L", "scala.Long"))
        testNodeSummaries('c') <==> Data.list(summary("Literal", "'c'", "scala.Char"))
        testNodeSummaries(1.5f) <==> Data.list(summary("Literal", "1.5f", "scala.Float"))
        testNodeSummaries(1.5) <==> Data.list(summary("Literal", "1.5", "scala.Double"))
        testNodeSummaries(true) <==> Data.list(summary("Literal", "true", "scala.Boolean"))
        testNodeSummaries("s") <==> Data.list(summary("Literal", "\"s\"", "java.lang.String"))
        testNodeSummaries(()) <==> Data.list(summary("Literal", "()", "scala.Unit"))
        testNodeSummaries(null) <==> Data.list(summary("Literal", "null", "scala.Null"))
      }

      test("ascribed Byte and Short constants keep their own type") {
        testNodeSummaries(1: Byte) <==> Data.list(summary("Literal", "1", "scala.Byte"))
        testNodeSummaries(2: Short) <==> Data.list(summary("Literal", "2", "scala.Short"))
      }
    }

    group("singletons") {
      import DestructuredExprsFixtures.testParseDetailed

      test("None is a Singleton node") {
        testParseDetailed(None) <==> Data.map("nodeType" -> Data("Singleton"), "name" -> Data("None"))
      }

      test("Nil is a Singleton node") {
        testParseDetailed(Nil) <==> Data.map("nodeType" -> Data("Singleton"), "name" -> Data("Nil"))
      }

      test("a top-level case object is a Singleton node") {
        testParseDetailed(Marker) <==> Data.map("nodeType" -> Data("Singleton"), "name" -> Data("Marker"))
      }

      test("a case object nested in a companion is a Singleton node named by its module path") {
        testParseDetailed(Shape.Circle) <==> Data.map("nodeType" -> Data("Singleton"), "name" -> Data("Shape.Circle"))
      }

      test("a case object nested in a companion passed as an argument is a Singleton node") {
        testParseDetailed((c: Calc) => c.pick[Shape](Shape.Circle, Shape.Circle)) <==> Data.map(
          "nodeType" -> Data("Lambda"),
          "params" -> Data.list(Data.map("name" -> Data("c"), "type" -> Data("hearth.examples.destructurededge.Calc"))),
          "body" -> Data.map(
            "nodeType" -> Data("MethodCall"),
            "methodName" -> Data("pick"),
            "returnType" -> Data("hearth.examples.destructurededge.Shape"),
            "applied" -> Data.list(
              Data.map("kind" -> Data("Instance"), "value" -> Data.map("nodeType" -> Data("ParamRef"), "paramName" -> Data("c"))),
              Data.map("kind" -> Data("Types"), "typeArgs" -> Data.list(Data("hearth.examples.destructurededge.Shape"))),
              Data.map(
                "kind" -> Data("Values"),
                "args" -> Data.list(
                  Data.map("nodeType" -> Data("Singleton"), "name" -> Data("Shape.Circle")),
                  Data.map("nodeType" -> Data("Singleton"), "name" -> Data("Shape.Circle"))
                )
              )
            )
          )
        )
      }
    }

    group("compiler noise") {
      import DestructuredExprsFixtures.testParseDetailed

      test("type ascriptions are stripped") {
        testParseDetailed((s: Street) => (s.name: CharSequence)) <==> Data.map(
          "nodeType" -> Data("Lambda"),
          "params" -> Data.list(Data.map("name" -> Data("s"), "type" -> Data("hearth.examples.destructurededge.Street"))),
          "body" -> Data.map(
            "nodeType" -> Data("MethodCall"),
            "methodName" -> Data("name"),
            "returnType" -> Data("java.lang.String"),
            "applied" -> Data.list(
              Data.map("kind" -> Data("Instance"), "value" -> Data.map("nodeType" -> Data("ParamRef"), "paramName" -> Data("s")))
            )
          )
        )
      }
    }

    group("MethodCall.applied mirrors the parameter clauses") {
      import DestructuredExprsFixtures.testParseDetailed

      def calcRef: Data = Data.map("nodeType" -> Data("ParamRef"), "paramName" -> Data("c"))
      def instance: Data = Data.map("kind" -> Data("Instance"), "value" -> calcRef)
      def values(args: Data*): Data = Data.map("kind" -> Data("Values"), "args" -> Data.list(args*))
      def call(name: String, returnType: String, applied: Data*): Data = Data.map(
        "nodeType" -> Data("Lambda"),
        "params" -> Data.list(Data.map("name" -> Data("c"), "type" -> Data("hearth.examples.destructurededge.Calc"))),
        "body" -> Data.map(
          "nodeType" -> Data("MethodCall"),
          "methodName" -> Data(name),
          "returnType" -> Data(returnType),
          "applied" -> Data.list(applied*)
        )
      )

      test("a parameterless method has only the instance") {
        testParseDetailed((c: Calc) => c.parameterless) <==> call("parameterless", "scala.Int", instance)
      }

      test("an empty parameter clause is an empty AppliedValues") {
        testParseDetailed((c: Calc) => c.nullary()) <==> call("nullary", "scala.Int", instance, values())
      }

      test("each parameter clause is its own AppliedValues") {
        testParseDetailed((c: Calc) => c.add(1)(2)) <==> call(
          "add",
          "scala.Int",
          instance,
          values(literal("1")),
          values(literal("2"))
        )
      }

      test("explicit type arguments are AppliedTypes before the values") {
        testParseDetailed((c: Calc) => c.pick[Int](1, 2)) <==> call(
          "pick",
          "scala.Int",
          instance,
          Data.map("kind" -> Data("Types"), "typeArgs" -> Data.list(Data("scala.Int"))),
          values(literal("1"), literal("2"))
        )
      }

      test("an implicit parameter clause is an AppliedValues with the resolved implicit") {
        testParseDetailed((c: Calc) => c.withImplicit(1)) <==> call(
          "withImplicit",
          "scala.Int",
          instance,
          values(literal("1")),
          values(
            Data.map(
              "nodeType" -> Data("MethodCall"),
              "methodName" -> Data("default"),
              "returnType" -> Data("hearth.examples.destructurededge.Scale"),
              "applied" -> Data.list(
                Data.map(
                  "kind" -> Data("Instance"),
                  "value" -> Data.map("nodeType" -> Data("Singleton"), "name" -> Data("Scale"))
                )
              )
            )
          )
        )
      }

      test("a lambda passed as an argument is destructured as a nested Lambda") {
        testParseDetailed((xs: List[Int]) => xs.map(x => x + 1)) <==> Data.map(
          "nodeType" -> Data("Lambda"),
          "params" -> Data.list(
            Data.map("name" -> Data("xs"), "type" -> Data("scala.collection.immutable.List[scala.Int]"))
          ),
          "body" -> Data.map(
            "nodeType" -> Data("MethodCall"),
            "methodName" -> Data("map"),
            "returnType" -> Data("scala.collection.immutable.List[scala.Int]"),
            "applied" -> Data.list(
              Data.map(
                "kind" -> Data("Instance"),
                "value" -> Data.map("nodeType" -> Data("ParamRef"), "paramName" -> Data("xs"))
              ),
              Data.map("kind" -> Data("Types"), "typeArgs" -> Data.list(Data("scala.Int"))),
              values(
                Data.map(
                  "nodeType" -> Data("Lambda"),
                  "params" -> Data.list(Data.map("name" -> Data("x"), "type" -> Data("scala.Int"))),
                  "body" -> Data.map(
                    "nodeType" -> Data("MethodCall"),
                    "methodName" -> Data("+"),
                    "returnType" -> Data("scala.Int"),
                    "applied" -> Data.list(
                      Data.map(
                        "kind" -> Data("Instance"),
                        "value" -> Data.map("nodeType" -> Data("ParamRef"), "paramName" -> Data("x"))
                      ),
                      values(literal("1"))
                    )
                  )
                )
              )
            )
          )
        )
      }
    }

    group("vararg slots") {
      import DestructuredExprsFixtures.testParseDetailed

      def varargCall(name: String, typeArgs: List[String]): Data = Data.map(
        "nodeType" -> Data("Lambda"),
        "params" -> Data.list(Data.map("name" -> Data("h"), "type" -> Data("hearth.examples.destructurededge.VarargHolder"))),
        "body" -> Data.map(
          "nodeType" -> Data("MethodCall"),
          "methodName" -> Data(name),
          "returnType" -> Data("scala.Int"),
          "applied" -> Data.list(
            (List(
              Data.map("kind" -> Data("Instance"), "value" -> Data.map("nodeType" -> Data("ParamRef"), "paramName" -> Data("h")))
            ) ++ (if (typeArgs.isEmpty) Nil else List(Data.map("kind" -> Data("Types"), "typeArgs" -> Data.list(typeArgs.map(Data(_))*)))) :+
              Data.map(
                "kind" -> Data("Values"),
                "args" -> Data.list(
                  Data.map(
                    "nodeType" -> Data("Varargs"),
                    "type" -> Data("scala.collection.immutable.Seq[scala.Int]"),
                    "elements" -> Data.list(literal("1"), literal("2"))
                  )
                )
              ))*
          )
        )
      )

      test("a monomorphic vararg method groups its elements into one Varargs slot") {
        testParseDetailed((h: VarargHolder) => h.monomorphicOf(1, 2)) <==> varargCall("monomorphicOf", Nil)
      }

      test("a generic vararg method groups its elements into one Varargs slot") {
        testParseDetailed((h: VarargHolder) => h.genericOf[Int](1, 2)) <==> varargCall("genericOf", List("scala.Int"))
      }

      test("an inherited generic vararg method groups its elements into one Varargs slot") {
        testParseDetailed((h: VarargHolder) => h.inheritedOf[Int](1, 2)) <==> varargCall("inheritedOf", List("scala.Int"))
      }
    }

    group("MethodCall.applied mirrors method.expectations") {
      import DestructuredExprsFixtures.testAppliedMirrorsExpectations

      def call(method: String, kinds: String*): Data = Data.map(
        "method" -> Data(method),
        "applied" -> Data.list(kinds.map(Data(_))*),
        "expectations" -> Data.list(kinds.map(Data(_))*)
      )

      test("for a generic class constructor") {
        testAppliedMirrorsExpectations(new Box[Int](1)) <==> Data.list(call("<init>", "Values"))
      }

      test("for a method with a type parameter clause") {
        testAppliedMirrorsExpectations((c: Calc) => c.pick[Int](1, 2)) <==> Data.list(
          call("pick", "Instance", "Types", "Values")
        )
      }

      test("for a method with two value parameter clauses") {
        testAppliedMirrorsExpectations((c: Calc) => c.add(1)(2)) <==> Data.list(
          call("add", "Instance", "Values", "Values")
        )
      }

      test("for a resolved implicit argument selected from a companion") {
        testAppliedMirrorsExpectations((c: Calc) => c.withImplicit(3)) <==> Data.list(
          call("withImplicit", "Instance", "Values", "Values"),
          call("default", "Instance")
        )
      }
    }

    group("constructor calls") {
      import DestructuredExprsFixtures.{testParseDetailed, testReceiver}

      test("each constructor parameter clause is its own AppliedValues") {
        testParseDetailed(new TwoClauses(1)("b")) <==> Data.map(
          "nodeType" -> Data("MethodCall"),
          "methodName" -> Data("<init>"),
          "returnType" -> Data("hearth.examples.destructurededge.TwoClauses"),
          "applied" -> Data.list(
            Data.map("kind" -> Data("Values"), "args" -> Data.list(literal("1"))),
            Data.map("kind" -> Data("Values"), "args" -> Data.list(literal("\"b\"")))
          )
        )
      }

      test("constructors have no receiver") {
        testReceiver(new TwoClauses(1)("b")) <==> Data.map("method" -> Data("<init>"), "receiver" -> Data("<none>"))
      }
    }

    group("MethodCall.receiver") {
      import DestructuredExprsFixtures.testReceiver

      test("is the instance of a plain method call") {
        testReceiver((c: Calc) => c.add(1)(2)) <==> Data.map(
          "method" -> Data("add"),
          "receiver" -> Data.map("node" -> Data("ParamRef"), "plainPrint" -> Data("c"))
        )
      }

      test("sees through an implicit conversion") {
        import conversions.*
        testReceiver((i: Int) => i.doubled) <==> Data.map(
          "method" -> Data("doubled"),
          "receiver" -> Data.map("node" -> Data("ParamRef"), "plainPrint" -> Data("i"))
        )
      }

      test("is empty for a method imported from a module") {
        import functions.twice
        testReceiver((i: Int) => twice(i)) <==> Data.map("method" -> Data("twice"), "receiver" -> Data("<none>"))
      }

      test("is the module of a method called through its module") {
        testReceiver((i: Int) => functions.twice(i)) <==> Data.map(
          "method" -> Data("twice"),
          "receiver" -> Data.map("node" -> Data("Singleton"), "plainPrint" -> Data("functions"))
        )
      }
    }

    group("binding identity") {
      import DestructuredExprsFixtures.testParseBindings

      def paramRef(id: Int, name: String): Data =
        Data.map("node" -> Data("ParamRef"), "binding" -> Data(id), "name" -> Data(name))

      test("nested lambdas: inner body references both the outer and the inner parameter") {
        testParseBindings((a: Int) => (b: Int) => a + b) <==> Data.map(
          "node" -> Data("Lambda"),
          "params" -> Data.list(Data.map("binding" -> Data(0), "name" -> Data("a"))),
          "body" -> Data.map(
            "node" -> Data("Lambda"),
            "params" -> Data.list(Data.map("binding" -> Data(1), "name" -> Data("b"))),
            "body" -> Data.map(
              "node" -> Data("MethodCall"),
              "name" -> Data("+"),
              "receiver" -> paramRef(0, "a"),
              "args" -> Data.list(paramRef(1, "b"))
            )
          )
        )
      }

      test("a block val shadowing a lambda parameter is a different binding") {
        testParseBindings { (a: Int) =>
          val inner = {
            val a = 1
            a
          }
          inner + a
        } <==> Data.map(
          "node" -> Data("Lambda"),
          "params" -> Data.list(Data.map("binding" -> Data(0), "name" -> Data("a"))),
          "body" -> Data.map(
            "node" -> Data("Block"),
            "statements" -> Data.list(
              Data.map(
                "node" -> Data("ValDefinition"),
                "binding" -> Data(1),
                "name" -> Data("inner"),
                "type" -> Data("scala.Int"),
                "flags" -> Data.list(),
                "hasPosition" -> Data(true),
                "rhs" -> Data.map(
                  "node" -> Data("Block"),
                  "statements" -> Data.list(
                    Data.map(
                      "node" -> Data("ValDefinition"),
                      "binding" -> Data(2),
                      "name" -> Data("a"),
                      "type" -> Data("scala.Int"),
                      "flags" -> Data.list(),
                      "hasPosition" -> Data(true),
                      "rhs" -> Data.map("node" -> Data("Literal"), "value" -> Data("1"))
                    )
                  ),
                  "result" -> Data.map(
                    "node" -> Data("LocalReference"),
                    "binding" -> Data(2),
                    "name" -> Data("a"),
                    "external" -> Data(false)
                  )
                )
              )
            ),
            "result" -> Data.map(
              "node" -> Data("MethodCall"),
              "name" -> Data("+"),
              "receiver" -> Data.map(
                "node" -> Data("LocalReference"),
                "binding" -> Data(1),
                "name" -> Data("inner"),
                "external" -> Data(false)
              ),
              "args" -> Data.list(paramRef(0, "a"))
            )
          )
        )
      }
    }

    group("LocalBinding properties") {
      import DestructuredExprsFixtures.testLocalBindings

      test("a parameter of the method enclosing the macro call is an external binding") {
        def enclosing(param: String): Data = testLocalBindings((x: Int) => x + param.length)
        enclosing("abc") <==> Data.list(
          Data.map(
            "name" -> Data("param"),
            "type" -> Data("java.lang.String"),
            "flags" -> Data.list(Data("external")),
            "hasPosition" -> Data(true)
          )
        )
      }

      test("vals defined in the expression are not external and know their type") {
        testLocalBindings {
          val list = List(1, 2)
          var count = list.size
          count += 1
          count
        } <==> Data.list(
          Data.map(
            "name" -> Data("list"),
            "type" -> Data("scala.collection.immutable.List[scala.Int]"),
            "flags" -> Data.list(),
            "hasPosition" -> Data(true)
          ),
          Data.map(
            "name" -> Data("count"),
            "type" -> Data("scala.Int"),
            "flags" -> Data.list(Data("mutable")),
            "hasPosition" -> Data(true)
          )
        )
      }
    }

    group("findReferences") {
      import DestructuredExprsFixtures.testReferenceOrder

      test("finds references in local defs, nested lambdas, ifs and trys, in source order") {
        testReferenceOrder { (a: Int, b: Int) =>
          def local(i: Int): Int = i + a
          val f = (x: Int) => x + b
          if (a > 0) {
            try local(b)
            catch { case _: Throwable => f(a) }
          } else 0
        } <==> Data.map(
          "references" -> Data.list(
            Data.map("name" -> Data("a"), "line" -> Data(1)),
            Data.map("name" -> Data("b"), "line" -> Data(2)),
            Data.map("name" -> Data("a"), "line" -> Data(3)),
            Data.map("name" -> Data("b"), "line" -> Data(4)),
            Data.map("name" -> Data("f"), "line" -> Data(5)),
            Data.map("name" -> Data("a"), "line" -> Data(5))
          ),
          "paramsReferenced" -> Data.list(Data(true), Data(true)),
          "unusedParams" -> Data.list(),
          "noBindingsGiveNoReferences" -> Data(true)
        )
      }

      test("a lambda that uses none of its parameters reports all of them as unused") {
        // the parameters are deliberately unused
        @scala.annotation.nowarn("msg=unused explicit parameter")
        def result = testReferenceOrder((a: Int, b: String) => 42)
        result <==> Data.map(
          "references" -> Data.list(),
          "paramsReferenced" -> Data.list(Data(false), Data(false)),
          "unusedParams" -> Data.list(Data("a"), Data("b")),
          "noBindingsGiveNoReferences" -> Data(true)
        )
      }
    }

    group("node summaries: types, rebuilt trees, positions and pre-order traversal") {
      import DestructuredExprsFixtures.testNodeSummaries

      test("a lambda parameter reference has the widened parameter type, every node has a position") {
        testNodeSummaries((r: Resident) => r.home.street) <==> Data.list(
          summary("Lambda", "(r: hearth.examples.destructurededge.Resident) => streethomer", "scala.Function1[hearth.examples.destructurededge.Resident, hearth.examples.destructurededge.Street]"),
          summary("MethodCall", "streethomer", "hearth.examples.destructurededge.Street"),
          summary("MethodCall", "homer", "hearth.examples.destructurededge.Home"),
          summary("ParamRef", "r", "hearth.examples.destructurededge.Resident")
        )
      }

      test("a block's statements, vararg elements and result are visited in order, on their own lines") {
        testNodeSummaries {
          val h = new VarargHolder
          h.monomorphicOf(
            1,
            2
          )
        } <==> Data.list(
          summary("Block", "{ val h = <init>(); monomorphicOfh(1, 2) }", "scala.Int"),
          summary("ValDefinition", "val h = <init>()", "scala.Unit", line = 1),
          summary("MethodCall", "<init>()", "hearth.examples.destructurededge.VarargHolder", line = 1),
          summary("MethodCall", "monomorphicOfh(1, 2)", "scala.Int", line = 2),
          summary("LocalReference", "h", "hearth.examples.destructurededge.VarargHolder", line = 2),
          summary("Varargs", "1, 2", "scala.collection.immutable.Seq[scala.Int]", line = 3),
          summary("Literal", "1", "scala.Int", line = 3),
          summary("Literal", "2", "scala.Int", line = 4)
        )
      }

      test("collect stops at non-destructurable sub-expressions, findReferences does not") {
        import DestructuredExprsFixtures.{testCollectMethodCalls, testReferenceOrder}
        testCollectMethodCalls((b: Boolean, s: Street) => if (b) s.name else "") <==> Data.list()
        testReferenceOrder((b: Boolean, s: Street) => if (b) s.name else "") <==> Data.map(
          "references" -> Data.list(
            Data.map("name" -> Data("b"), "line" -> Data(0)),
            Data.map("name" -> Data("s"), "line" -> Data(0))
          ),
          "paramsReferenced" -> Data.list(Data(true), Data(true)),
          "unusedParams" -> Data.list(),
          "noBindingsGiveNoReferences" -> Data(true)
        )
      }
    }

    group("extractFieldPath") {
      import DestructuredExprsFixtures.testParseFieldPath

      def segment(name: String, source: String, result: String): Data = Data.map(
        "name" -> Data(name),
        "sourceType" -> Data(source),
        "resultType" -> Data(result),
        "methodName" -> Data(name)
      )

      test("three levels deep") {
        testParseFieldPath((r: Resident) => r.home.street.name) <==> Data.map(
          "fieldNames" -> Data.list(Data("home"), Data("street"), Data("name")),
          "depth" -> Data(3),
          "plainPrint" -> Data("_.home.street.name"),
          "rootType" -> Data("hearth.examples.destructurededge.Resident"),
          "leafType" -> Data("java.lang.String"),
          "segments" -> Data.list(
            segment("home", "hearth.examples.destructurededge.Resident", "hearth.examples.destructurededge.Home"),
            segment("street", "hearth.examples.destructurededge.Home", "hearth.examples.destructurededge.Street"),
            segment("name", "hearth.examples.destructurededge.Street", "java.lang.String")
          )
        )
      }

      test("a field of a generic case class has the applied type") {
        testParseFieldPath((b: Box[Int]) => b.value) <==> Data.map(
          "fieldNames" -> Data.list(Data("value")),
          "depth" -> Data(1),
          "plainPrint" -> Data("_.value"),
          "rootType" -> Data("hearth.examples.destructurededge.Box[scala.Int]"),
          "leafType" -> Data("scala.Int"),
          "segments" -> Data.list(segment("value", "hearth.examples.destructurededge.Box[scala.Int]", "scala.Int"))
        )
      }

      test("a field of a nested generic case class has the applied type") {
        testParseFieldPath((b: Box[Box[Street]]) => b.value.value.name) <==> Data.map(
          "fieldNames" -> Data.list(Data("value"), Data("value"), Data("name")),
          "depth" -> Data(3),
          "plainPrint" -> Data("_.value.value.name"),
          "rootType" -> Data(
            "hearth.examples.destructurededge.Box[hearth.examples.destructurededge.Box[hearth.examples.destructurededge.Street]]"
          ),
          "leafType" -> Data("java.lang.String"),
          "segments" -> Data.list(
            segment(
              "value",
              "hearth.examples.destructurededge.Box[hearth.examples.destructurededge.Box[hearth.examples.destructurededge.Street]]",
              "hearth.examples.destructurededge.Box[hearth.examples.destructurededge.Street]"
            ),
            segment(
              "value",
              "hearth.examples.destructurededge.Box[hearth.examples.destructurededge.Street]",
              "hearth.examples.destructurededge.Street"
            ),
            segment("name", "hearth.examples.destructurededge.Street", "java.lang.String")
          )
        )
      }

      test("a val of a regular class is a field step") {
        testParseFieldPath((t: TwoClauses) => t.b) <==> Data.map(
          "fieldNames" -> Data.list(Data("b")),
          "depth" -> Data(1),
          "plainPrint" -> Data("_.b"),
          "rootType" -> Data("hearth.examples.destructurededge.TwoClauses"),
          "leafType" -> Data("java.lang.String"),
          "segments" -> Data.list(segment("b", "hearth.examples.destructurededge.TwoClauses", "java.lang.String"))
        )
      }

      test("a method call with arguments is not a field step") {
        testParseFieldPath((s: Street) => s.name.substring(1)) <==> Data.map(
          "error" -> Data("Expected a field access (method with only an instance), got substringnames(1)")
        )
      }

      test("a tuple element accessor is a field step") {
        testParseFieldPath[(Street, Street), String]((pair: (Street, Street)) => pair._1.name) <==> Data.map(
          "fieldNames" -> Data.list(Data("_1"), Data("name")),
          "depth" -> Data(2),
          "plainPrint" -> Data("_._1.name"),
          "rootType" -> Data(
            "scala.Tuple2[hearth.examples.destructurededge.Street, hearth.examples.destructurededge.Street]"
          ),
          "leafType" -> Data("java.lang.String"),
          "segments" -> Data.list(
            segment(
              "_1",
              "scala.Tuple2[hearth.examples.destructurededge.Street, hearth.examples.destructurededge.Street]",
              "hearth.examples.destructurededge.Street"
            ),
            segment("name", "hearth.examples.destructurededge.Street", "java.lang.String")
          )
        )
      }

      test("a function value that is not a lambda literal is rejected") {
        val selector: Street => String = _.name
        val _ = selector
        testParseFieldPath(selector) <==> Data.map("error" -> Data("Expected a lambda expression, got selector"))
      }

      test("a chain that does not start at the lambda parameter is rejected") {
        val other = Street("other")
        val _ = other
        // the parameter is deliberately unused
        @scala.annotation.nowarn("msg=unused explicit parameter")
        def result = testParseFieldPath((s: Street) => other.name)
        result <==> Data.map("error" -> Data("Expected a field access chain on the lambda parameter, got other"))
      }
    }

    group("extractLambda") {
      import DestructuredExprsFixtures.testParseLambda

      test("rejects an expression that is not a lambda") {
        testParseLambda(42) <==> Data.map("error" -> Data("Expected a lambda expression, got 42"))
      }

      test("returns every parameter of a multi-parameter lambda") {
        testParseLambda((a: Int, b: String) => b.length + a) <==> Data.map(
          "params" -> Data.list(
            Data.map("name" -> Data("a"), "type" -> Data("scala.Int"), "declaredType" -> Data("scala.Int")),
            Data.map("name" -> Data("b"), "type" -> Data("java.lang.String"), "declaredType" -> Data("java.lang.String"))
          ),
          "body" -> Data.map(
            "nodeType" -> Data("MethodCall"),
            "plainPrint" -> Data("+lengthb()(a)"),
            "methodName" -> Data("+"),
            "appliedCount" -> Data(2)
          )
        )
      }
    }

    group("statements") {
      import DestructuredExprsFixtures.{testParseBindings, testStatements}

      test("objects, traits and single-name imports have positions and are rebuilt as Unit") {
        // the local definitions and imports are deliberately unused
        @scala.annotation.nowarn
        def result = testStatements {
          import scala.collection.Seq
          object LocalObject
          trait LocalTrait
          val value = 1
          value
        }
        result <==> Data.list(
          Data.map("plainPrint" -> Data("import collectionscala.Seq"), "hasPosition" -> Data(true), "rebuiltAsUnit" -> Data(true)),
          Data.map("plainPrint" -> Data("object LocalObject"), "hasPosition" -> Data(true), "rebuiltAsUnit" -> Data(true)),
          Data.map("plainPrint" -> Data("trait LocalTrait"), "hasPosition" -> Data(true), "rebuiltAsUnit" -> Data(true)),
          Data.map("plainPrint" -> Data("val value = 1"), "hasPosition" -> Data(true), "rebuiltAsUnit" -> Data(true))
        )
      }

      test("a single-name import has the name as its only selector") {
        // the import is deliberately unused
        @scala.annotation.nowarn
        def result = testParseBindings {
          import scala.collection.Seq
          new Street("x")
        }
        result <==> Data.map(
          "node" -> Data("Block"),
          "statements" -> Data.list(
            Data.map(
              "node" -> Data("Import"),
              "qualifier" -> Data.map(
                "node" -> Data("MethodCall"),
                "name" -> Data("collection"),
                "receiver" -> Data.map("node" -> Data("Other"), "plainPrint" -> Data("scala")),
                "args" -> Data.list()
              ),
              "selectors" -> Data.list(Data("Seq"))
            )
          ),
          "result" -> Data.map(
            "node" -> Data("MethodCall"),
            "name" -> Data("<init>"),
            "receiver" -> Data("<none>"),
            "args" -> Data.list(Data.map("node" -> Data("Literal"), "value" -> Data("\"x\"")))
          )
        )
      }
    }
  }
}
