package hearth
package typed

import hearth.data.Data

/** Macro implementation is in [[MethodsFixturesImpl]] */
final class MethodsScala3Spec extends MacroSuite {

  group("typed.Methods") {

    group("type Method") {

      group(
        "constructors: Method.{primaryConstructorOf[A], defaultConstructorOf[A], constructorsOf[A]}, returns preprocessed constructors"
      ) {
        import MethodsFixtures.testConstructorsExtraction

        test("for small tuple (Tuple2)") {
          testConstructorsExtraction[(Int, String)] <==> Data.map(
            "primaryConstructor" -> Data("(_1: scala.Int, _2: java.lang.String)"),
            "defaultConstructor" -> Data("<no default constructor>"),
            "constructors" -> Data.list(Data("(_1: scala.Int, _2: java.lang.String)"))
          )
        }

        test("for TupleXXL (23+ elements)") {
          // 23-element tuple: macro reflection sees typeSymbol = scala.Tuple (the sealed abstract trait),
          // so constructors reflect Tuple's no-arg constructor, not *:'s constructor.
          testConstructorsExtraction[
            (
                Int,
                String,
                Boolean,
                Long,
                Float,
                Double,
                Char,
                Byte,
                Short,
                Int,
                String,
                Boolean,
                Long,
                Float,
                Double,
                Char,
                Byte,
                Short,
                Int,
                String,
                Boolean,
                Int,
                String
            )
          ] <==> Data.map(
            "primaryConstructor" -> Data("()"),
            "defaultConstructor" -> Data("()"),
            "constructors" -> Data.list(Data("()"))
          )
        }
      }

      group("methods: Method.methodsOf[A], returns preprocessed methods for tuples") {
        import MethodsFixtures.testMethodsExtraction

        test("for small tuple (Tuple2)") {
          val methods = testMethodsExtraction[(Int, String)](
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
          // Verify that methods were extracted (non-empty), and check expected accessor methods exist
          val methodsMap = methods.asMap.get
          assert(methodsMap.nonEmpty, "Tuple2 should have methods")
          assert(methodsMap.contains("_1"), "Tuple2 should have _1 accessor")
          assert(methodsMap.contains("_2"), "Tuple2 should have _2 accessor")
        }

        test("for TupleXXL (23+ elements)") {
          // Macro reflection sees typeSymbol = scala.Tuple for TupleXXL types,
          // so methods reflect Tuple's members rather than *:'s _1/_2 accessors.
          val methods = testMethodsExtraction[
            (
                Int,
                String,
                Boolean,
                Long,
                Float,
                Double,
                Char,
                Byte,
                Short,
                Int,
                String,
                Boolean,
                Long,
                Float,
                Double,
                Char,
                Byte,
                Short,
                Int,
                String,
                Boolean,
                Int,
                String
            )
          ](
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
          // Verify that methods were extracted for TupleXXL
          val methodsMap = methods.asMap.get
          assert(methodsMap.nonEmpty, "TupleXXL should have methods")
          // TupleXXL types expose Tuple's methods like productIterator, productElementName, etc.
          // but NOT _1/_2 (those belong to *: which is not what typeSymbol resolves to)
        }
      }
    }
  }
}
