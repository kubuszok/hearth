package hearth
package typed

import hearth.testdata.Data

class MethodsSpec extends MacroSuite {

  group("trait typed.Methods") {

    group("methods: Method.methodsOf[A], returns preprocessed methods") {
      import MethodsFixtures.testMethodsExtraction

      test("for java.lang.Object") {
        testMethodsExtraction[java.lang.Object]("getClass", "equals") <==> Data(
          Map(
            // actual methods of java.lang.Object
            "clone()" -> Data(
              Map(
                "invocation" -> Data("OnInstance"),
                "hasTypeParameters" -> Data(false),
                "position" -> Data("None"),
                "isVal" -> Data(false),
                "isVar" -> Data(false),
                "isLazy" -> Data(false),
                "isDef" -> Data(true),
                "isInherited" -> Data(false),
                "isImplicit" -> Data(false),
                "isAvailable(Everywhere)" -> Data(false),
                "arity" -> Data(0),
                "isNullary" -> Data(true),
                "isUnary" -> Data(false),
                "isBinary" -> Data(false),
                "isScalaGetter" -> Data(false),
                "isScalaSetter" -> Data(false),
                "isJavaGetter" -> Data(false),
                "isScalaAccessor" -> Data(false),
                "isJavaSetter" -> Data(false),
                "isJavaAccessor" -> Data(false),
                "isAccessor" -> Data(false)
              )
            ),
            // Excluded because on Scala 2 it's "equals(Object)", on Scala 3 it's "equals(Any)"
            /*
            "equals(Object)" -> Data(
              Map(
                "invocation" -> Data("OnInstance"),
                "hasTypeParameters" -> Data(false),
                "position" -> Data("None"),
                "isVal" -> Data(false),
                "isVar" -> Data(false),
                "isLazy" -> Data(false),
                "isDef" -> Data(true),
                "isInherited" -> Data(true),
                "isImplicit" -> Data(false),
                "isAvailable(Everywhere)" -> Data(true),
                "arity" -> Data(1),
                "isNullary" -> Data(false),
                "isUnary" -> Data(true),
                "isBinary" -> Data(false),
                "isScalaGetter" -> Data(false),
                "isScalaSetter" -> Data(false),
                "isJavaGetter" -> Data(false),
                "isScalaAccessor" -> Data(false),
                "isJavaSetter" -> Data(false),
                "isJavaAccessor" -> Data(false),
                "isAccessor" -> Data(false)
              )
            ),
             */
            "finalize()" -> Data(
              Map(
                "invocation" -> Data("OnInstance"),
                "hasTypeParameters" -> Data(false),
                "position" -> Data("None"),
                "isVal" -> Data(false),
                "isVar" -> Data(false),
                "isLazy" -> Data(false),
                "isDef" -> Data(true),
                "isInherited" -> Data(false),
                "isImplicit" -> Data(false),
                "isAvailable(Everywhere)" -> Data(false),
                "arity" -> Data(0),
                "isNullary" -> Data(true),
                "isUnary" -> Data(false),
                "isBinary" -> Data(false),
                "isScalaGetter" -> Data(false),
                "isScalaSetter" -> Data(false),
                "isJavaGetter" -> Data(false),
                "isScalaAccessor" -> Data(false),
                "isJavaSetter" -> Data(false),
                "isJavaAccessor" -> Data(false),
                "isAccessor" -> Data(false)
              )
            ),
            // Excluded because on Scala 2 it's "getClass", on Scala 3 it's "getClass()"
            /*
            "getClass()" -> Data(
              Map(
                "invocation" -> Data("OnInstance"),
                "hasTypeParameters" -> Data(false),
                "position" -> Data("None"),
                "isVal" -> Data(false),
                "isVar" -> Data(false),
                "isLazy" -> Data(false),
                "isDef" -> Data(true),
                "isInherited" -> Data(true),
                "isImplicit" -> Data(false),
                "isAvailable(Everywhere)" -> Data(true),
                "arity" -> Data(0),
                "isNullary" -> Data(true),
                "isUnary" -> Data(false),
                "isBinary" -> Data(false),
                "isScalaGetter" -> Data(false),
                "isScalaSetter" -> Data(false),
                "isJavaGetter" -> Data(false),
                "isScalaAccessor" -> Data(false),
                "isJavaSetter" -> Data(false),
                "isJavaAccessor" -> Data(false),
                "isAccessor" -> Data(false)
              )
            ),
             */
            "hashCode()" -> Data(
              Map(
                "invocation" -> Data("OnInstance"),
                "hasTypeParameters" -> Data(false),
                "position" -> Data("None"),
                "isVal" -> Data(false),
                "isVar" -> Data(false),
                "isLazy" -> Data(false),
                "isDef" -> Data(true),
                "isInherited" -> Data(false),
                "isImplicit" -> Data(false),
                "isAvailable(Everywhere)" -> Data(true),
                "arity" -> Data(0),
                "isNullary" -> Data(true),
                "isUnary" -> Data(false),
                "isBinary" -> Data(false),
                "isScalaGetter" -> Data(false),
                "isScalaSetter" -> Data(false),
                "isJavaGetter" -> Data(false),
                "isScalaAccessor" -> Data(false),
                "isJavaSetter" -> Data(false),
                "isJavaAccessor" -> Data(false),
                "isAccessor" -> Data(false)
              )
            ),
            "notify()" -> Data(
              Map(
                "invocation" -> Data("OnInstance"),
                "hasTypeParameters" -> Data(false),
                "position" -> Data("None"),
                "isVal" -> Data(false),
                "isVar" -> Data(false),
                "isLazy" -> Data(false),
                "isDef" -> Data(true),
                "isInherited" -> Data(false),
                "isImplicit" -> Data(false),
                "isAvailable(Everywhere)" -> Data(true),
                "arity" -> Data(0),
                "isNullary" -> Data(true),
                "isUnary" -> Data(false),
                "isBinary" -> Data(false),
                "isScalaGetter" -> Data(false),
                "isScalaSetter" -> Data(false),
                "isJavaGetter" -> Data(false),
                "isScalaAccessor" -> Data(false),
                "isJavaSetter" -> Data(false),
                "isJavaAccessor" -> Data(false),
                "isAccessor" -> Data(false)
              )
            ),
            "notifyAll()" -> Data(
              Map(
                "invocation" -> Data("OnInstance"),
                "hasTypeParameters" -> Data(false),
                "position" -> Data("None"),
                "isVal" -> Data(false),
                "isVar" -> Data(false),
                "isLazy" -> Data(false),
                "isDef" -> Data(true),
                "isInherited" -> Data(false),
                "isImplicit" -> Data(false),
                "isAvailable(Everywhere)" -> Data(true),
                "arity" -> Data(0),
                "isNullary" -> Data(true),
                "isUnary" -> Data(false),
                "isBinary" -> Data(false),
                "isScalaGetter" -> Data(false),
                "isScalaSetter" -> Data(false),
                "isJavaGetter" -> Data(false),
                "isScalaAccessor" -> Data(false),
                "isJavaSetter" -> Data(false),
                "isJavaAccessor" -> Data(false),
                "isAccessor" -> Data(false)
              )
            ),
            "toString()" -> Data(
              Map(
                "invocation" -> Data("OnInstance"),
                "hasTypeParameters" -> Data(false),
                "position" -> Data("None"),
                "isVal" -> Data(false),
                "isVar" -> Data(false),
                "isLazy" -> Data(false),
                "isDef" -> Data(true),
                "isInherited" -> Data(false),
                "isImplicit" -> Data(false),
                "isAvailable(Everywhere)" -> Data(true),
                "arity" -> Data(0),
                "isNullary" -> Data(true),
                "isUnary" -> Data(false),
                "isBinary" -> Data(false),
                "isScalaGetter" -> Data(false),
                "isScalaSetter" -> Data(false),
                "isJavaGetter" -> Data(false),
                "isScalaAccessor" -> Data(false),
                "isJavaSetter" -> Data(false),
                "isJavaAccessor" -> Data(false),
                "isAccessor" -> Data(false)
              )
            ),
            "wait" -> Data(
              Map(
                "()" -> Data(
                  Map(
                    "invocation" -> Data("OnInstance"),
                    "hasTypeParameters" -> Data(false),
                    "position" -> Data("None"),
                    "isVal" -> Data(false),
                    "isVar" -> Data(false),
                    "isLazy" -> Data(false),
                    "isDef" -> Data(true),
                    "isInherited" -> Data(false),
                    "isImplicit" -> Data(false),
                    "isAvailable(Everywhere)" -> Data(true),
                    "arity" -> Data(0),
                    "isNullary" -> Data(true),
                    "isUnary" -> Data(false),
                    "isBinary" -> Data(false),
                    "isScalaGetter" -> Data(false),
                    "isScalaSetter" -> Data(false),
                    "isJavaGetter" -> Data(false),
                    "isScalaAccessor" -> Data(false),
                    "isJavaSetter" -> Data(false),
                    "isJavaAccessor" -> Data(false),
                    "isAccessor" -> Data(false)
                  )
                ),
                "(Long)" -> Data(
                  Map(
                    "invocation" -> Data("OnInstance"),
                    "hasTypeParameters" -> Data(false),
                    "position" -> Data("None"),
                    "isVal" -> Data(false),
                    "isVar" -> Data(false),
                    "isLazy" -> Data(false),
                    "isDef" -> Data(true),
                    "isInherited" -> Data(false),
                    "isImplicit" -> Data(false),
                    "isAvailable(Everywhere)" -> Data(true),
                    "arity" -> Data(1),
                    "isNullary" -> Data(false),
                    "isUnary" -> Data(true),
                    "isBinary" -> Data(false),
                    "isScalaGetter" -> Data(false),
                    "isScalaSetter" -> Data(false),
                    "isJavaGetter" -> Data(false),
                    "isScalaAccessor" -> Data(false),
                    "isJavaSetter" -> Data(false),
                    "isJavaAccessor" -> Data(false),
                    "isAccessor" -> Data(false)
                  )
                ),
                "(Long, Int)" -> Data(
                  Map(
                    "invocation" -> Data("OnInstance"),
                    "hasTypeParameters" -> Data(false),
                    "position" -> Data("None"),
                    "isVal" -> Data(false),
                    "isVar" -> Data(false),
                    "isLazy" -> Data(false),
                    "isDef" -> Data(true),
                    "isInherited" -> Data(false),
                    "isImplicit" -> Data(false),
                    "isAvailable(Everywhere)" -> Data(true),
                    "arity" -> Data(2),
                    "isNullary" -> Data(false),
                    "isUnary" -> Data(false),
                    "isBinary" -> Data(true),
                    "isScalaGetter" -> Data(false),
                    "isScalaSetter" -> Data(false),
                    "isJavaGetter" -> Data(false),
                    "isScalaAccessor" -> Data(false),
                    "isJavaSetter" -> Data(false),
                    "isJavaAccessor" -> Data(false),
                    "isAccessor" -> Data(false)
                  )
                )
              )
            ),
            // quasi-methods generated by Scala for consistency
            "asInstanceOf" -> Data(
              Map(
                "invocation" -> Data("OnInstance"),
                "hasTypeParameters" -> Data(true),
                "position" -> Data("None"),
                "isVal" -> Data(false),
                "isVar" -> Data(false),
                "isLazy" -> Data(false),
                "isDef" -> Data(true),
                "isInherited" -> Data(true),
                "isImplicit" -> Data(false),
                "isAvailable(Everywhere)" -> Data(true),
                "arity" -> Data(0),
                "isNullary" -> Data(true),
                "isUnary" -> Data(false),
                "isBinary" -> Data(false),
                "isScalaGetter" -> Data(false),
                "isScalaSetter" -> Data(false),
                "isJavaGetter" -> Data(false),
                "isScalaAccessor" -> Data(false),
                "isJavaSetter" -> Data(false),
                "isJavaAccessor" -> Data(false),
                "isAccessor" -> Data(false)
              )
            ),
            "isInstanceOf" -> Data(
              Map(
                "invocation" -> Data("OnInstance"),
                "hasTypeParameters" -> Data(true),
                "position" -> Data("None"),
                "isVal" -> Data(false),
                "isVar" -> Data(false),
                "isLazy" -> Data(false),
                "isDef" -> Data(true),
                "isInherited" -> Data(true),
                "isImplicit" -> Data(false),
                "isAvailable(Everywhere)" -> Data(true),
                "arity" -> Data(0),
                "isNullary" -> Data(true),
                "isUnary" -> Data(false),
                "isBinary" -> Data(false),
                "isScalaGetter" -> Data(false),
                "isScalaSetter" -> Data(false),
                "isJavaGetter" -> Data(false),
                "isScalaAccessor" -> Data(false),
                "isJavaSetter" -> Data(false),
                "isJavaAccessor" -> Data(false),
                "isAccessor" -> Data(false)
              )
            ),
            "synchronized" -> Data(
              Map(
                "invocation" -> Data("OnInstance"),
                "hasTypeParameters" -> Data(true),
                "position" -> Data("None"),
                "isVal" -> Data(false),
                "isVar" -> Data(false),
                "isLazy" -> Data(false),
                "isDef" -> Data(true),
                "isInherited" -> Data(false),
                "isImplicit" -> Data(false),
                "isAvailable(Everywhere)" -> Data(true),
                "arity" -> Data(0),
                "isNullary" -> Data(true),
                "isUnary" -> Data(false),
                "isBinary" -> Data(false),
                "isScalaGetter" -> Data(false),
                "isScalaSetter" -> Data(false),
                "isJavaGetter" -> Data(false),
                "isScalaAccessor" -> Data(false),
                "isJavaSetter" -> Data(false),
                "isJavaAccessor" -> Data(false),
                "isAccessor" -> Data(false)
              )
            ),
            "==(Any)" -> Data(
              Map(
                "invocation" -> Data("OnInstance"),
                "hasTypeParameters" -> Data(false),
                "position" -> Data("None"),
                "isVal" -> Data(false),
                "isVar" -> Data(false),
                "isLazy" -> Data(false),
                "isDef" -> Data(true),
                "isInherited" -> Data(false),
                "isImplicit" -> Data(false),
                "isAvailable(Everywhere)" -> Data(true),
                "arity" -> Data(1),
                "isNullary" -> Data(false),
                "isUnary" -> Data(true),
                "isBinary" -> Data(false),
                "isScalaGetter" -> Data(false),
                "isScalaSetter" -> Data(false),
                "isJavaGetter" -> Data(false),
                "isScalaAccessor" -> Data(false),
                "isJavaSetter" -> Data(false),
                "isJavaAccessor" -> Data(false),
                "isAccessor" -> Data(false)
              )
            ),
            "!=(Any)" -> Data(
              Map(
                "invocation" -> Data("OnInstance"),
                "hasTypeParameters" -> Data(false),
                "position" -> Data("None"),
                "isVal" -> Data(false),
                "isVar" -> Data(false),
                "isLazy" -> Data(false),
                "isDef" -> Data(true),
                "isInherited" -> Data(false),
                "isImplicit" -> Data(false),
                "isAvailable(Everywhere)" -> Data(true),
                "arity" -> Data(1),
                "isNullary" -> Data(false),
                "isUnary" -> Data(true),
                "isBinary" -> Data(false),
                "isScalaGetter" -> Data(false),
                "isScalaSetter" -> Data(false),
                "isJavaGetter" -> Data(false),
                "isScalaAccessor" -> Data(false),
                "isJavaSetter" -> Data(false),
                "isJavaAccessor" -> Data(false),
                "isAccessor" -> Data(false)
              )
            ),
            "eq(Object)" -> Data(
              Map(
                "invocation" -> Data("OnInstance"),
                "hasTypeParameters" -> Data(false),
                "position" -> Data("None"),
                "isVal" -> Data(false),
                "isVar" -> Data(false),
                "isLazy" -> Data(false),
                "isDef" -> Data(true),
                "isInherited" -> Data(false),
                "isImplicit" -> Data(false),
                "isAvailable(Everywhere)" -> Data(true),
                "arity" -> Data(1),
                "isNullary" -> Data(false),
                "isUnary" -> Data(true),
                "isBinary" -> Data(false),
                "isScalaGetter" -> Data(false),
                "isScalaSetter" -> Data(false),
                "isJavaGetter" -> Data(false),
                "isScalaAccessor" -> Data(false),
                "isJavaSetter" -> Data(false),
                "isJavaAccessor" -> Data(false),
                "isAccessor" -> Data(false)
              )
            ),
            "ne(Object)" -> Data(
              Map(
                "invocation" -> Data("OnInstance"),
                "hasTypeParameters" -> Data(false),
                "position" -> Data("None"),
                "isVal" -> Data(false),
                "isVar" -> Data(false),
                "isLazy" -> Data(false),
                "isDef" -> Data(true),
                "isInherited" -> Data(false),
                "isImplicit" -> Data(false),
                "isAvailable(Everywhere)" -> Data(true),
                "arity" -> Data(1),
                "isNullary" -> Data(false),
                "isUnary" -> Data(true),
                "isBinary" -> Data(false),
                "isScalaGetter" -> Data(false),
                "isScalaSetter" -> Data(false),
                "isJavaGetter" -> Data(false),
                "isScalaAccessor" -> Data(false),
                "isJavaSetter" -> Data(false),
                "isJavaAccessor" -> Data(false),
                "isAccessor" -> Data(false)
              )
            ),
            // null-safe hash method
            "##" -> Data(
              Map(
                "invocation" -> Data("OnInstance"),
                "hasTypeParameters" -> Data(false),
                "position" -> Data("None"),
                "isVal" -> Data(false),
                "isVar" -> Data(false),
                "isLazy" -> Data(false),
                "isDef" -> Data(true),
                "isInherited" -> Data(false),
                "isImplicit" -> Data(false),
                "isAvailable(Everywhere)" -> Data(true),
                "arity" -> Data(0),
                "isNullary" -> Data(true),
                "isUnary" -> Data(false),
                "isBinary" -> Data(false),
                "isScalaGetter" -> Data(false),
                "isScalaSetter" -> Data(false),
                "isJavaGetter" -> Data(false),
                "isScalaAccessor" -> Data(false),
                "isJavaSetter" -> Data(false),
                "isJavaAccessor" -> Data(false),
                "isAccessor" -> Data(false)
              )
            )
          )
        )
      }
    }
  }
}
