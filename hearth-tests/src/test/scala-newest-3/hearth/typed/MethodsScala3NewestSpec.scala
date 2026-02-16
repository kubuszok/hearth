package hearth
package typed

import hearth.data.Data

/** Tests for Scala 3 newest features — only compiled on Scala 3.7+ (scala-newest-3 source set).
  *
  * Macro implementation is in [[MethodsFixturesImpl]]
  */
final class MethodsScala3NewestSpec extends MacroSuite {

  group("typed.Methods") {

    group("type Method") {

      group(
        "constructors: Method.{primaryConstructorOf[A], defaultConstructorOf[A], constructorsOf[A]}, returns preprocessed constructors"
      ) {
        import MethodsFixtures.testConstructorsExtraction

        test("for named tuple (Scala 3.7+)") {
          // NamedTuple is an opaque type alias wrapping a Tuple.
          // Hearth synthesizes a constructor from the NamedTuple type arguments:
          // field names from the first type arg, field types from the second.
          testConstructorsExtraction[(name: String, age: Int)] <==> Data.map(
            "primaryConstructor" -> Data("(name: java.lang.String, age: scala.Int)"),
            "defaultConstructor" -> Data("<no default constructor>"),
            "constructors" -> Data.list(Data("(name: java.lang.String, age: scala.Int)"))
          )
        }
      }

      group("constructor instantiation: construct a NamedTuple via the synthesized constructor") {
        import MethodsFixtures.testConstructNamedTuple

        test("for named tuple (Scala 3.7+)") {
          // Constructs (name: String, age: Int) with ("Alice", 42) via the synthesized constructor.
          // At runtime, NamedTuples are just regular tuples, so .toString gives "(Alice,42)".
          testConstructNamedTuple[(name: String, age: Int)] <==> Data("(Alice,42)")
        }
      }

      group("field extraction: extract named tuple fields by name") {
        import MethodsFixtures.testNamedTupleFieldExtraction

        test("for named tuple (Scala 3.7+)") {
          // Extracts fields by their named tuple names (not _1/_2) using
          // the constructor's parameters + Product.productElement(index).
          val nt: (name: String, age: Int) = ("Alice", 42)
          testNamedTupleFieldExtraction(nt) <==> Data("name=Alice, age=42")
        }
      }

      group("methods: Method.methodsOf[A], returns preprocessed methods for named tuples") {
        import MethodsFixtures.testMethodsExtraction

        test("for named tuple (Scala 3.7+)") {
          // NamedTuple's type symbol resolves to Tuple (sealed abstract trait),
          // so methods come from the Tuple trait and its parent Product.
          // This is similar to TupleXXL behavior — no _1/_2 accessors, but Tuple operations.
          val methods = testMethodsExtraction[(name: String, age: Int)](
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
          )
          val methodsMap = methods.asMap.get
          assert(methodsMap.nonEmpty, "NamedTuple should have methods")
          // Tuple-specific methods are present (not _1/_2 since those are on Tuple2, not Tuple)
          assert(methodsMap.contains("head"), "NamedTuple should have head method from Tuple")
          assert(methodsMap.contains("tail"), "NamedTuple should have tail method from Tuple")
          assert(methodsMap.contains("toList"), "NamedTuple should have toList method from Tuple")
          assert(methodsMap.contains("size"), "NamedTuple should have size method from Tuple")
          // Product methods are inherited (methods with parameters include signatures in key)
          assert(methodsMap.contains("productArity"), "NamedTuple should have productArity from Product")
          assert(
            methodsMap.contains("productElement(Int)"),
            "NamedTuple should have productElement(Int) from Product"
          )
          // Should NOT have _1/_2 (those belong to Tuple2, not Tuple)
          assert(!methodsMap.contains("_1"), "NamedTuple should not have _1 (belongs to Tuple2, not Tuple)")
          assert(!methodsMap.contains("_2"), "NamedTuple should not have _2 (belongs to Tuple2, not Tuple)")
        }
      }
    }
  }
}
