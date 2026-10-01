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
          "params" -> Data.list(
            Data.map("name" -> Data("x"), "type" -> Data("scala.Int"), "declaredType" -> Data("scala.Int"))
          ),
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
            Data.map(
              "name" -> Data("p"),
              "type" -> Data("hearth.examples.parsed_exprs.Person"),
              "declaredType" -> Data("hearth.examples.parsed_exprs.Person")
            )
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

    group("optics marker paths") {
      import DestructuredExprsFixtures.testMarkerPath
      import examples.parsed_exprs.dsl.*

      // Monocle-Focus / quicklens-style optics use MARKER methods in the lambda path: `.each` (traverse a collection),
      // `.when[Sub]`/`.as[Sub]` (prism / subtype focus), `.some` (Option focus). These are plain `MethodCall`s in the
      // destructured tree (the DSL exposes them via implicit-class extension methods). A macro author can recognize
      // them purely from structure — the marker call's name, and for prisms the focused subtype recovered as the
      // marker's own type argument — regardless of how Scala 2 vs Scala 3 desugar the implicit-class plumbing. This is
      // the core of building a Monocle/quicklens-style API on top of Hearth. The fixture peels the plumbing and emits
      // a clean root -> leaf logical path; expectations below are identical on both platforms.

      test(".each traversal marker recognized between plain field selects") {
        // _.items.each : `items` is a plain field, `each` is a traversal marker.
        testMarkerPath((c: examples.parsed_exprs.Container) => c.items.each) <==> Data.map(
          "segments" -> Data.list(
            Data.map("name" -> Data("items"), "kind" -> Data("field"), "typeArgs" -> Data.list()),
            Data.map("name" -> Data("each"), "kind" -> Data("marker"), "typeArgs" -> Data.list())
          )
        )
      }

      test(".as[Sub] prism marker recovers the focused subtype as its type argument") {
        // _.animal.as[Dog].name : `animal` field, `as` prism marker carrying `Dog`, `name` field.
        testMarkerPath((h: examples.parsed_exprs.AnimalHolder) => h.animal.as[examples.parsed_exprs.Dog].name) <==> Data
          .map(
            "segments" -> Data.list(
              Data.map("name" -> Data("animal"), "kind" -> Data("field"), "typeArgs" -> Data.list()),
              Data.map(
                "name" -> Data("as"),
                "kind" -> Data("marker"),
                "typeArgs" -> Data.list(Data("hearth.examples.parsed_exprs.Dog"))
              ),
              Data.map("name" -> Data("name"), "kind" -> Data("field"), "typeArgs" -> Data.list())
            )
          )
      }

      test(".when[Sub] prism marker recovers the focused subtype as its type argument") {
        // _.animal.when[Cat] : `animal` field, `when` prism marker carrying `Cat`.
        testMarkerPath((h: examples.parsed_exprs.AnimalHolder) => h.animal.when[examples.parsed_exprs.Cat]) <==> Data
          .map(
            "segments" -> Data.list(
              Data.map("name" -> Data("animal"), "kind" -> Data("field"), "typeArgs" -> Data.list()),
              Data.map(
                "name" -> Data("when"),
                "kind" -> Data("marker"),
                "typeArgs" -> Data.list(Data("hearth.examples.parsed_exprs.Cat"))
              )
            )
          )
      }

      test(".some Option focus marker recognized before a plain field") {
        // _.nested.some.street : `nested` field, `some` Option-focus marker, `street` field.
        testMarkerPath((c: examples.parsed_exprs.Container) => c.nested.some.street) <==> Data.map(
          "segments" -> Data.list(
            Data.map("name" -> Data("nested"), "kind" -> Data("field"), "typeArgs" -> Data.list()),
            Data.map("name" -> Data("some"), "kind" -> Data("marker"), "typeArgs" -> Data.list()),
            Data.map("name" -> Data("street"), "kind" -> Data("field"), "typeArgs" -> Data.list())
          )
        )
      }

      test("pure field path has no markers") {
        testMarkerPath((p: examples.parsed_exprs.Person) => p.address.street) <==> Data.map(
          "segments" -> Data.list(
            Data.map("name" -> Data("address"), "kind" -> Data("field"), "typeArgs" -> Data.list()),
            Data.map("name" -> Data("street"), "kind" -> Data("field"), "typeArgs" -> Data.list())
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

    group("local bindings: val definitions, references, imports, local definitions") {
      import DestructuredExprsFixtures.testParseBindings
      import examples.parsed_exprs.grammar_dsl.*

      def paramRef(id: Int, name: String): Data =
        Data.map("node" -> Data("ParamRef"), "binding" -> Data(id), "name" -> Data(name))
      def localRef(id: Int, name: String, external: Boolean = false): Data =
        Data.map(
          "node" -> Data("LocalReference"),
          "binding" -> Data(id),
          "name" -> Data(name),
          "external" -> Data(external)
        )
      def literal(value: String): Data = Data.map("node" -> Data("Literal"), "value" -> Data(value))

      test("a block-shaped DSL: imports, vals and references linked by identity") {
        testParseBindings { (g: Dsl) =>
          import g.*
          val expr = nonTerminal[Int]
          val num = terminal("[0-9]+")
          expr ::= num
          expr
        } <==> Data.map(
          "node" -> Data("Lambda"),
          "params" -> Data.list(Data.map("binding" -> Data(0), "name" -> Data("g"))),
          "body" -> Data.map(
            "node" -> Data("Block"),
            "statements" -> Data.list(
              Data.map("node" -> Data("Import"), "qualifier" -> paramRef(0, "g"), "selectors" -> Data.list(Data("*"))),
              Data.map(
                "node" -> Data("ValDefinition"),
                "binding" -> Data(1),
                "name" -> Data("expr"),
                "type" -> Data("hearth.examples.parsed_exprs.grammar_dsl.Sym[scala.Int]"),
                "flags" -> Data.list(),
                "hasPosition" -> Data(true),
                "rhs" -> Data.map(
                  "node" -> Data("MethodCall"),
                  "name" -> Data("nonTerminal"),
                  "receiver" -> paramRef(0, "g"),
                  "args" -> Data.list()
                )
              ),
              Data.map(
                "node" -> Data("ValDefinition"),
                "binding" -> Data(2),
                "name" -> Data("num"),
                "type" -> Data("hearth.examples.parsed_exprs.grammar_dsl.Sym[java.lang.String]"),
                "flags" -> Data.list(),
                "hasPosition" -> Data(true),
                "rhs" -> Data.map(
                  "node" -> Data("MethodCall"),
                  "name" -> Data("terminal"),
                  "receiver" -> paramRef(0, "g"),
                  "args" -> Data.list(literal("\"[0-9]+\""))
                )
              ),
              // `expr ::= num` is `g.SymOps(expr).::=(num)`: `receiver` sees through the implicit class
              Data.map(
                "node" -> Data("MethodCall"),
                "name" -> Data("::="),
                "receiver" -> localRef(1, "expr"),
                "args" -> Data.list(localRef(2, "num"))
              )
            ),
            "result" -> localRef(1, "expr")
          )
        )
      }

      test("shadowed names are different bindings") {
        testParseBindings {
          val a = 1
          val b = {
            val a = 2
            a
          }
          a + b
        } <==> Data.map(
          "node" -> Data("Block"),
          "statements" -> Data.list(
            Data.map(
              "node" -> Data("ValDefinition"),
              "binding" -> Data(0),
              "name" -> Data("a"),
              "type" -> Data("scala.Int"),
              "flags" -> Data.list(),
              "hasPosition" -> Data(true),
              "rhs" -> literal("1")
            ),
            Data.map(
              "node" -> Data("ValDefinition"),
              "binding" -> Data(1),
              "name" -> Data("b"),
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
                    "rhs" -> literal("2")
                  )
                ),
                "result" -> localRef(2, "a")
              )
            )
          ),
          "result" -> Data.map(
            "node" -> Data("MethodCall"),
            "name" -> Data("+"),
            "receiver" -> localRef(0, "a"),
            "args" -> Data.list(localRef(1, "b"))
          )
        )
      }

      test("var, lazy val and implicit val flags") {
        testParseBindings {
          var counter = 0
          lazy val cached = counter + 1
          implicit val label: String = "x"
          counter = cached
          counter + label.length
        } <==> Data.map(
          "node" -> Data("Block"),
          "statements" -> Data.list(
            Data.map(
              "node" -> Data("ValDefinition"),
              "binding" -> Data(0),
              "name" -> Data("counter"),
              "type" -> Data("scala.Int"),
              "flags" -> Data.list(Data("mutable")),
              "hasPosition" -> Data(true),
              "rhs" -> literal("0")
            ),
            Data.map(
              "node" -> Data("ValDefinition"),
              "binding" -> Data(1),
              "name" -> Data("cached"),
              "type" -> Data("scala.Int"),
              "flags" -> Data.list(Data("lazy")),
              "hasPosition" -> Data(true),
              "rhs" -> Data.map(
                "node" -> Data("MethodCall"),
                "name" -> Data("+"),
                "receiver" -> localRef(0, "counter"),
                "args" -> Data.list(literal("1"))
              )
            ),
            Data.map(
              "node" -> Data("ValDefinition"),
              "binding" -> Data(2),
              "name" -> Data("label"),
              "type" -> Data("java.lang.String"),
              "flags" -> Data.list(Data("implicit")),
              "hasPosition" -> Data(true),
              "rhs" -> literal("\"x\"")
            ),
            // assignments are not destructured (yet)
            Data.map("node" -> Data("Other"), "plainPrint" -> Data("<non-destructurable: counter = cached>"))
          ),
          "result" -> Data.map(
            "node" -> Data("MethodCall"),
            "name" -> Data("+"),
            "receiver" -> localRef(0, "counter"),
            "args" -> Data.list(
              Data.map(
                "node" -> Data("MethodCall"),
                "name" -> Data("length"),
                "receiver" -> localRef(2, "label"),
                "args" -> Data.list()
              )
            )
          )
        )
      }

      test("references to locals defined outside of the expression are external") {
        val offset = 10
        val _ = offset // Scala 2 does not see the use inside the (already expanded) macro argument
        testParseBindings((x: Int) => x + offset) <==> Data.map(
          "node" -> Data("Lambda"),
          "params" -> Data.list(Data.map("binding" -> Data(0), "name" -> Data("x"))),
          "body" -> Data.map(
            "node" -> Data("MethodCall"),
            "name" -> Data("+"),
            "receiver" -> paramRef(0, "x"),
            "args" -> Data.list(localRef(1, "offset", external = true))
          )
        )
      }

      test("local defs, classes, objects and type aliases are LocalDefinitions") {
        // the local definitions are deliberately unused
        @scala.annotation.nowarn
        def result = testParseBindings {
          def helper(i: Int): Int = i
          class Local
          object LocalObject
          type Alias = Int
          val value: Alias = 1
          value
        }
        result <==> Data.map(
          "node" -> Data("Block"),
          "statements" -> Data.list(
            Data.map("node" -> Data("LocalDefinition"), "kind" -> Data("def"), "name" -> Data("helper")),
            Data.map("node" -> Data("LocalDefinition"), "kind" -> Data("class"), "name" -> Data("Local")),
            Data.map("node" -> Data("LocalDefinition"), "kind" -> Data("object"), "name" -> Data("LocalObject")),
            Data.map("node" -> Data("LocalDefinition"), "kind" -> Data("type"), "name" -> Data("Alias")),
            Data.map(
              "node" -> Data("ValDefinition"),
              "binding" -> Data(0),
              "name" -> Data("value"),
              "type" -> Data("scala.Int"),
              "flags" -> Data.list(),
              "hasPosition" -> Data(true),
              "rhs" -> literal("1")
            )
          ),
          "result" -> localRef(0, "value")
        )
      }
    }

    group("findReferences / Lambda.unusedParams") {
      import DestructuredExprsFixtures.testFindReferences

      test("finds references inside sub-trees that are not destructurable (pattern matches)") {
        // `b` is deliberately unused
        @scala.annotation.nowarn("msg=unused explicit parameter")
        def result = testFindReferences { (a: Int, b: Int, c: Int) =>
          val d = a
          (a, d) match {
            case (1, x) => x + c
            case _      => 0
          }
        }
        result <==> Data.map(
          "unusedParams" -> Data.list(Data("b")),
          "referenceCounts" -> Data.map(
            "a" -> Data(2),
            "b" -> Data(0),
            "c" -> Data(1),
            "d" -> Data(1)
          ),
          "allReferencesHavePositions" -> Data(true)
        )
      }

      test("matches by identity, not by name (a shadowing nested lambda parameter)") {
        // the outer `a` is deliberately unused
        @scala.annotation.nowarn("msg=unused explicit parameter")
        def result = testFindReferences { (a: Int) =>
          val f = (a: Int) => a + 1
          f(2)
        }
        result <==> Data.map(
          "unusedParams" -> Data.list(Data("a")),
          "referenceCounts" -> Data.map("a" -> Data(0), "f" -> Data(1)),
          "allReferencesHavePositions" -> Data(true)
        )
      }
    }

    group("skipContextualWrappers") {
      import DestructuredExprsFixtures.testSkipContextualWrappers

      test("skips a block that only binds an implicit value") {
        testSkipContextualWrappers {
          implicit val ctx: examples.parsed_exprs.Ctx = examples.parsed_exprs.Ctx.instance
          (p: examples.parsed_exprs.Person) => p.name + implicitly[examples.parsed_exprs.Ctx].toString
        } <==> Data.map(
          "parsed" -> Data.map("node" -> Data("Block")),
          "skipped" -> Data.map("node" -> Data("Lambda"), "contextual" -> Data(false), "params" -> Data.list(Data("p")))
        )
      }

      test("is a no-op for a plain lambda") {
        testSkipContextualWrappers((p: examples.parsed_exprs.Person) => p.name) <==> Data.map(
          "parsed" -> Data.map("node" -> Data("Lambda"), "contextual" -> Data(false), "params" -> Data.list(Data("p"))),
          "skipped" -> Data.map("node" -> Data("Lambda"), "contextual" -> Data(false), "params" -> Data.list(Data("p")))
        )
      }
    }

    group("MethodCall.receiver") {
      import DestructuredExprsFixtures.testReceiverChain
      import examples.parsed_exprs.dsl.*

      test("sees through implicit-class wrappers") {
        testReceiverChain((c: examples.parsed_exprs.Container) => c.items.each.length) <==> Data.list(
          Data("items"),
          Data("each"),
          Data("length")
        )
        testReceiverChain((h: examples.parsed_exprs.AnimalHolder) => h.animal.when[examples.parsed_exprs.Dog].name) <==>
          Data.list(Data("animal"), Data("when"), Data("name"))
      }
    }
  }
}
