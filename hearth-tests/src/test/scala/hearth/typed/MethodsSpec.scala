package hearth
package typed

import hearth.data.Data

/** Macro implementation is in [[MethodsFixturesImpl]] */
final class MethodsSpec extends MacroSuite {
  // Apparently: Scala 2 does not store position for for methods from previous compilation unit,
  // Scala 3 does store... something? Filename? It changes between versions, settings, etc :/
  private val isPositionTrimmed =
    MethodsFixtures
      .testMethodsExtraction[examples.methods.NoCompanionClass](
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
      .asMap
      .get
      .apply("method(Int)")
      .asMap
      .get
      .apply("position")
      .asString
      .get
      .endsWith("1:1)")
  private def methodsPosition(line: Int, column: Int): Data =
    if (LanguageVersion.byHearth.isScala2_13)
      Data("None")
    else if (isPositionTrimmed)
      Data("Some(hearth-tests/src/main/scala/hearth/examples/methods.scala:1:1)")
    else
      Data(s"Some(hearth-tests/src/main/scala/hearth/examples/methods.scala:$line:$column)")

  group("regression: intersection types") {

    // `instanceTpe.typeSymbol` is `NoSymbol` for an intersection `A & B` (an `AndType`), so reading members only from
    // it would drop the accessors declared by the parts (`name`, `age`).
    test("methods of `A & B` include the members of both parts") {
      MethodsFixtures.testExposesNameAndAge[examples.methods.ExampleHasName & examples.methods.ExampleHasAge] <==>
        Data(true)
    }
  }

  group("regression: JavaBean accessor capitalization") {

    // The JavaBeans convention requires an UPPER-CASE character after the `get`/`is`/`set` prefix. `getters`/`island`/
    // `settle` merely start with a prefix (lowercase char after it) and must NOT be classified as accessors - otherwise
    // `javaAccessorName` yields garbage ("ters"/"land"/"tle"). `getName`/`isActive`/`setValue` are genuine accessors.
    test("`get*`/`is*`/`set*` are accessors only with an upper-case char after the prefix") {
      MethodsFixtures.testJavaAccessorNames[examples.methods.ExampleBeanLikeNames] <==> Data.map(
        "getName" -> Data("name"),
        "getters" -> Data("<none>"),
        "isActive" -> Data("active"),
        "island" -> Data("<none>"),
        "setValue" -> Data("value"),
        "settle" -> Data("<none>")
      )
    }
  }

  group("typed.Methods") {

    group("type Method") {

      group(
        "constructors: Method.{primaryConstructorOf[A], defaultConstructorOf[A], constructorsOf[A]}, returns preprocessed constructors"
      ) {
        import MethodsFixtures.testConstructorsExtraction

        test("for class without companion") {
          testConstructorsExtraction[examples.methods.NoCompanionClass] <==> Data.map(
            "primaryConstructor" -> Data("()"),
            "defaultConstructor" -> Data("()"),
            "constructors" -> Data.list(Data("()"))
          )
        }

        test("for class with companion") {
          testConstructorsExtraction[examples.methods.WithCompanion] <==> Data.map(
            "primaryConstructor" -> Data("(arg: scala.Int)"),
            "defaultConstructor" -> Data("<no default constructor>"),
            "constructors" -> Data.list(Data("(arg: scala.Int)"))
          )
        }

        test("for type alias with fewer type parameters than the aliased class") {
          testConstructorsExtraction[examples.methods.PhantomParams.Partial[String]] <==> Data.map(
            "primaryConstructor" -> Data("(a: scala.Int, b: java.lang.String)"),
            "defaultConstructor" -> Data("<no default constructor>"),
            "constructors" -> Data.list(Data("(a: scala.Int, b: java.lang.String)"))
          )
        }

        test("for type alias with no type parameters of a generic class") {
          testConstructorsExtraction[examples.methods.PhantomParams.Fixed] <==> Data.map(
            "primaryConstructor" -> Data("(a: scala.Int, b: java.lang.String)"),
            "defaultConstructor" -> Data("<no default constructor>"),
            "constructors" -> Data.list(Data("(a: scala.Int, b: java.lang.String)"))
          )
        }

        test("for type alias reordering type parameters of the aliased class") {
          testConstructorsExtraction[examples.methods.GenericClassAliases.Swapped[Int, String]] <==> Data.map(
            "primaryConstructor" -> Data("(a: java.lang.String, b: scala.Int)"),
            "defaultConstructor" -> Data("<no default constructor>"),
            "constructors" -> Data.list(Data("(a: java.lang.String, b: scala.Int)"))
          )
        }

        test("for chains of type aliases renaming, reordering and fixing type parameters") {
          testConstructorsExtraction[examples.methods.PhantomParams.PartialOfPartial[String]] <==> Data.map(
            "primaryConstructor" -> Data("(a: scala.Int, b: java.lang.String)"),
            "defaultConstructor" -> Data("<no default constructor>"),
            "constructors" -> Data.list(Data("(a: scala.Int, b: java.lang.String)"))
          )
          testConstructorsExtraction[examples.methods.PhantomParams.ReorderedAgain[String, Nothing]] <==> Data.map(
            "primaryConstructor" -> Data("(a: scala.Int, b: java.lang.String)"),
            "defaultConstructor" -> Data("<no default constructor>"),
            "constructors" -> Data.list(Data("(a: scala.Int, b: java.lang.String)"))
          )
          testConstructorsExtraction[examples.methods.PhantomParams.FixedThroughChain] <==> Data.map(
            "primaryConstructor" -> Data("(a: scala.Int, b: java.lang.String)"),
            "defaultConstructor" -> Data("<no default constructor>"),
            "constructors" -> Data.list(Data("(a: scala.Int, b: java.lang.String)"))
          )
          testConstructorsExtraction[examples.methods.GenericClassAliases.SwappedTwice[Int, String]] <==> Data.map(
            "primaryConstructor" -> Data("(a: scala.Int, b: java.lang.String)"),
            "defaultConstructor" -> Data("<no default constructor>"),
            "constructors" -> Data.list(Data("(a: scala.Int, b: java.lang.String)"))
          )
          testConstructorsExtraction[examples.methods.GenericClassAliases.SwappedPartial[Int]] <==> Data.map(
            "primaryConstructor" -> Data("(a: java.lang.String, b: scala.Int)"),
            "defaultConstructor" -> Data("<no default constructor>"),
            "constructors" -> Data.list(Data("(a: java.lang.String, b: scala.Int)"))
          )
          testConstructorsExtraction[examples.methods.GenericClassAliases.SwappedTwiceSwapped[Int, String]] <==> Data
            .map(
              "primaryConstructor" -> Data("(a: java.lang.String, b: scala.Int)"),
              "defaultConstructor" -> Data("<no default constructor>"),
              "constructors" -> Data.list(Data("(a: java.lang.String, b: scala.Int)"))
            )
        }

        test("for companion object") {
          testConstructorsExtraction[examples.methods.WithCompanion.type] <==> Data.map(
            "primaryConstructor" -> Data("()"),
            "defaultConstructor" -> Data("()"),
            "constructors" -> Data.list(Data("()"))
          )
        }
      }

      group("methods: Method.methodsOf[A], returns preprocessed methods") {
        import MethodsFixtures.testMethodsExtraction

        test("for class without companion") {
          testMethodsExtraction[examples.methods.NoCompanionClass](
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
          ) <==> Data.map(
            "method(Int)" -> Data.map(
              "invocation" -> Data("OnInstance"),
              "hasTypeParameters" -> Data(false),
              "position" -> methodsPosition(20, 7),
              "annotations" -> Data.list(),
              "isConstructor" -> Data(false),
              "isVal" -> Data(false),
              "isVar" -> Data(false),
              "isLazy" -> Data(false),
              "isDef" -> Data(true),
              "isFinal" -> Data(false),
              "isAbstract" -> Data(false),
              "isOverride" -> Data(false),
              "isImplicit" -> Data(false),
              "isDeclared" -> Data(true),
              "isSynthetic" -> Data(false),
              "isInherited" -> Data(false),
              "isAvailable(Everywhere)" -> Data(true),
              "isAvailable(AtCallSite)" -> Data(true),
              "arity" -> Data(1),
              "isNullary" -> Data(false),
              "isUnary" -> Data(true),
              "isBinary" -> Data(false),
              "isConstructorArgument" -> Data(false),
              "isCaseField" -> Data(false),
              "isScalaGetter" -> Data(false),
              "isScalaSetter" -> Data(false),
              "isScalaAccessor" -> Data(false),
              "isJavaGetter" -> Data(false),
              "isJavaSetter" -> Data(false),
              "isJavaAccessor" -> Data(false),
              "isAccessor" -> Data(false),
              "scalaAccessorName" -> Data("<no scala accessor name>"),
              "javaAccessorName" -> Data("<no java accessor name>"),
              "accessorName" -> Data("<no accessor name>")
            ),
            "methodWithDefault(Int)" -> Data.map(
              "invocation" -> Data("OnInstance"),
              "hasTypeParameters" -> Data(false),
              "position" -> methodsPosition(22, 7),
              "annotations" -> Data.list(),
              "isConstructor" -> Data(false),
              "isVal" -> Data(false),
              "isVar" -> Data(false),
              "isLazy" -> Data(false),
              "isDef" -> Data(true),
              "isFinal" -> Data(false),
              "isAbstract" -> Data(false),
              "isOverride" -> Data(false),
              "isImplicit" -> Data(false),
              "isDeclared" -> Data(true),
              "isSynthetic" -> Data(false),
              "isInherited" -> Data(false),
              "isAvailable(Everywhere)" -> Data(true),
              "isAvailable(AtCallSite)" -> Data(true),
              "arity" -> Data(1),
              "isNullary" -> Data(false),
              "isUnary" -> Data(true),
              "isBinary" -> Data(false),
              "isConstructorArgument" -> Data(false),
              "isCaseField" -> Data(false),
              "isScalaGetter" -> Data(false),
              "isScalaSetter" -> Data(false),
              "isJavaGetter" -> Data(false),
              "isScalaAccessor" -> Data(false),
              "isJavaSetter" -> Data(false),
              "isJavaAccessor" -> Data(false),
              "isAccessor" -> Data(false),
              "scalaAccessorName" -> Data("<no scala accessor name>"),
              "javaAccessorName" -> Data("<no java accessor name>"),
              "accessorName" -> Data("<no accessor name>")
            ),
            "methodWithAnnotation(Int)" -> Data.map(
              "invocation" -> Data("OnInstance"),
              "hasTypeParameters" -> Data(false),
              "position" -> methodsPosition(25, 7),
              "annotations" -> Data.list(Data("new hearth.examples.methods.ExampleAnnotation()")),
              "isConstructor" -> Data(false),
              "isVal" -> Data(false),
              "isVar" -> Data(false),
              "isLazy" -> Data(false),
              "isDef" -> Data(true),
              "isFinal" -> Data(false),
              "isAbstract" -> Data(false),
              "isOverride" -> Data(false),
              "isImplicit" -> Data(false),
              "isDeclared" -> Data(true),
              "isSynthetic" -> Data(false),
              "isInherited" -> Data(false),
              "isAvailable(Everywhere)" -> Data(true),
              "isAvailable(AtCallSite)" -> Data(true),
              "arity" -> Data(1),
              "isNullary" -> Data(false),
              "isUnary" -> Data(true),
              "isBinary" -> Data(false),
              "isConstructorArgument" -> Data(false),
              "isCaseField" -> Data(false),
              "isScalaGetter" -> Data(false),
              "isScalaSetter" -> Data(false),
              "isScalaAccessor" -> Data(false),
              "isJavaGetter" -> Data(false),
              "isJavaSetter" -> Data(false),
              "isJavaAccessor" -> Data(false),
              "isAccessor" -> Data(false),
              "scalaAccessorName" -> Data("<no scala accessor name>"),
              "javaAccessorName" -> Data("<no java accessor name>"),
              "accessorName" -> Data("<no accessor name>")
            ),
            "methodWithAnnotatedParam(Int)" -> Data.map(
              "invocation" -> Data("OnInstance"),
              "hasTypeParameters" -> Data(false),
              "position" -> methodsPosition(27, 7),
              "annotations" -> Data.list(),
              "isConstructor" -> Data(false),
              "isVal" -> Data(false),
              "isVar" -> Data(false),
              "isLazy" -> Data(false),
              "isDef" -> Data(true),
              "isFinal" -> Data(false),
              "isAbstract" -> Data(false),
              "isOverride" -> Data(false),
              "isImplicit" -> Data(false),
              "isDeclared" -> Data(true),
              "isSynthetic" -> Data(false),
              "isInherited" -> Data(false),
              "isAvailable(Everywhere)" -> Data(true),
              "isAvailable(AtCallSite)" -> Data(true),
              "arity" -> Data(1),
              "isNullary" -> Data(false),
              "isUnary" -> Data(true),
              "isBinary" -> Data(false),
              "isConstructorArgument" -> Data(false),
              "isCaseField" -> Data(false),
              "isScalaGetter" -> Data(false),
              "isScalaSetter" -> Data(false),
              "isScalaAccessor" -> Data(false),
              "isJavaGetter" -> Data(false),
              "isJavaSetter" -> Data(false),
              "isJavaAccessor" -> Data(false),
              "isAccessor" -> Data(false),
              "scalaAccessorName" -> Data("<no scala accessor name>"),
              "javaAccessorName" -> Data("<no java accessor name>"),
              "accessorName" -> Data("<no accessor name>")
            ),
            "scalaValue" -> Data.map(
              "invocation" -> Data("OnInstance"),
              "hasTypeParameters" -> Data(false),
              "position" -> methodsPosition(29, 7),
              "annotations" -> Data.list(),
              "isConstructor" -> Data(false),
              "isVal" -> Data(true),
              "isVar" -> Data(false),
              "isLazy" -> Data(false),
              "isDef" -> Data(false),
              "isFinal" -> Data(false),
              "isAbstract" -> Data(false),
              "isOverride" -> Data(false),
              "isImplicit" -> Data(false),
              "isDeclared" -> Data(true),
              "isSynthetic" -> Data(false),
              "isInherited" -> Data(false),
              "isAvailable(Everywhere)" -> Data(true),
              "isAvailable(AtCallSite)" -> Data(true),
              "arity" -> Data(0),
              "isNullary" -> Data(true),
              "isUnary" -> Data(false),
              "isBinary" -> Data(false),
              "isConstructorArgument" -> Data(false),
              "isCaseField" -> Data(false),
              "isScalaGetter" -> Data(true),
              "isScalaSetter" -> Data(false),
              "isScalaAccessor" -> Data(true),
              "isJavaGetter" -> Data(false),
              "isJavaSetter" -> Data(false),
              "isJavaAccessor" -> Data(false),
              "isAccessor" -> Data(true),
              "scalaAccessorName" -> Data("scalaValue"),
              "javaAccessorName" -> Data("<no java accessor name>"),
              "accessorName" -> Data("scalaValue")
            ),
            "scalaVariable" -> Data.map(
              "invocation" -> Data("OnInstance"),
              "hasTypeParameters" -> Data(false),
              "position" -> methodsPosition(31, 7),
              "annotations" -> Data.list(),
              "isConstructor" -> Data(false),
              "isVal" -> Data(false),
              "isVar" -> Data(true),
              "isLazy" -> Data(false),
              "isDef" -> Data(false),
              "isFinal" -> Data(false),
              "isAbstract" -> Data(false),
              "isOverride" -> Data(false),
              "isImplicit" -> Data(false),
              "isDeclared" -> Data(true),
              "isSynthetic" -> Data(false),
              "isInherited" -> Data(false),
              "isAvailable(Everywhere)" -> Data(true),
              "isAvailable(AtCallSite)" -> Data(true),
              "arity" -> Data(0),
              "isNullary" -> Data(true),
              "isUnary" -> Data(false),
              "isBinary" -> Data(false),
              "isConstructorArgument" -> Data(false),
              "isCaseField" -> Data(false),
              "isScalaGetter" -> Data(true),
              "isScalaSetter" -> Data(false),
              "isScalaAccessor" -> Data(true),
              "isJavaGetter" -> Data(false),
              "isJavaSetter" -> Data(false),
              "isJavaAccessor" -> Data(false),
              "isAccessor" -> Data(true),
              "scalaAccessorName" -> Data("scalaVariable"),
              "javaAccessorName" -> Data("<no java accessor name>"),
              "accessorName" -> Data("scalaVariable")
            ),
            "scalaVariable_=(Int)" -> Data.map(
              "invocation" -> Data("OnInstance"),
              "hasTypeParameters" -> Data(false),
              "position" -> methodsPosition(31, 7),
              "annotations" -> Data.list(),
              "isConstructor" -> Data(false),
              "isVal" -> Data(false),
              "isVar" -> Data(true),
              "isLazy" -> Data(false),
              "isDef" -> Data(true),
              "isFinal" -> Data(false),
              "isAbstract" -> Data(false),
              "isOverride" -> Data(false),
              "isImplicit" -> Data(false),
              "isDeclared" -> Data(true),
              "isSynthetic" -> Data(false),
              "isInherited" -> Data(false),
              "isAvailable(Everywhere)" -> Data(true),
              "isAvailable(AtCallSite)" -> Data(true),
              "arity" -> Data(1),
              "isNullary" -> Data(false),
              "isUnary" -> Data(true),
              "isBinary" -> Data(false),
              "isConstructorArgument" -> Data(false),
              "isCaseField" -> Data(false),
              "isScalaGetter" -> Data(false),
              "isScalaSetter" -> Data(true),
              "isScalaAccessor" -> Data(true),
              "isJavaGetter" -> Data(false),
              "isJavaSetter" -> Data(false),
              "isJavaAccessor" -> Data(false),
              "isAccessor" -> Data(true),
              "scalaAccessorName" -> Data("scalaVariable"),
              "javaAccessorName" -> Data("<no java accessor name>"),
              "accessorName" -> Data("scalaVariable")
            ),
            "scalaLazyValue" -> Data.map(
              "invocation" -> Data("OnInstance"),
              "hasTypeParameters" -> Data(false),
              "position" -> methodsPosition(33, 12),
              "annotations" -> Data.list(),
              "isConstructor" -> Data(false),
              "isVal" -> Data(true),
              "isVar" -> Data(false),
              "isLazy" -> Data(true),
              "isDef" -> Data(false),
              "isFinal" -> Data(false),
              "isAbstract" -> Data(false),
              "isOverride" -> Data(false),
              "isImplicit" -> Data(false),
              "isDeclared" -> Data(true),
              "isSynthetic" -> Data(false),
              "isInherited" -> Data(false),
              "isAvailable(Everywhere)" -> Data(true),
              "isAvailable(AtCallSite)" -> Data(true),
              "arity" -> Data(0),
              "isNullary" -> Data(true),
              "isUnary" -> Data(false),
              "isBinary" -> Data(false),
              "isConstructorArgument" -> Data(false),
              "isCaseField" -> Data(false),
              "isScalaGetter" -> Data(true),
              "isScalaSetter" -> Data(false),
              "isScalaAccessor" -> Data(true),
              "isJavaGetter" -> Data(false),
              "isJavaSetter" -> Data(false),
              "isJavaAccessor" -> Data(false),
              "isAccessor" -> Data(true),
              "scalaAccessorName" -> Data("scalaLazyValue"),
              "javaAccessorName" -> Data("<no java accessor name>"),
              "accessorName" -> Data("scalaLazyValue")
            ),
            "inheritedAbstractMethod(Int)" -> Data.map(
              "invocation" -> Data("OnInstance"),
              "hasTypeParameters" -> Data(false),
              "position" -> methodsPosition(35, 16),
              "annotations" -> Data.list(),
              "isConstructor" -> Data(false),
              "isVal" -> Data(false),
              "isVar" -> Data(false),
              "isLazy" -> Data(false),
              "isDef" -> Data(true),
              "isFinal" -> Data(false),
              "isAbstract" -> Data(false),
              "isOverride" -> Data(true),
              "isImplicit" -> Data(false),
              "isDeclared" -> Data(true),
              "isSynthetic" -> Data(false),
              "isInherited" -> Data(false),
              "isAvailable(Everywhere)" -> Data(true),
              "isAvailable(AtCallSite)" -> Data(true),
              "arity" -> Data(1),
              "isNullary" -> Data(false),
              "isUnary" -> Data(true),
              "isBinary" -> Data(false),
              "isConstructorArgument" -> Data(false),
              "isCaseField" -> Data(false),
              "isScalaGetter" -> Data(false),
              "isScalaSetter" -> Data(false),
              "isScalaAccessor" -> Data(false),
              "isJavaGetter" -> Data(false),
              "isJavaSetter" -> Data(false),
              "isJavaAccessor" -> Data(false),
              "isAccessor" -> Data(false),
              "scalaAccessorName" -> Data("<no scala accessor name>"),
              "javaAccessorName" -> Data("<no java accessor name>"),
              "accessorName" -> Data("<no accessor name>")
            ),
            "inheritedFinalMethod(Int)" -> Data.map(
              "invocation" -> Data("OnInstance"),
              "hasTypeParameters" -> Data(false),
              "position" -> methodsPosition(14, 13),
              "annotations" -> Data.list(),
              "isConstructor" -> Data(false),
              "isVal" -> Data(false),
              "isVar" -> Data(false),
              "isLazy" -> Data(false),
              "isDef" -> Data(true),
              "isFinal" -> Data(true),
              "isAbstract" -> Data(false),
              "isOverride" -> Data(false),
              "isImplicit" -> Data(false),
              "isDeclared" -> Data(false),
              "isSynthetic" -> Data(false),
              "isInherited" -> Data(true),
              "isAvailable(Everywhere)" -> Data(true),
              "isAvailable(AtCallSite)" -> Data(true),
              "arity" -> Data(1),
              "isNullary" -> Data(false),
              "isUnary" -> Data(true),
              "isBinary" -> Data(false),
              "isConstructorArgument" -> Data(false),
              "isCaseField" -> Data(false),
              "isScalaGetter" -> Data(false),
              "isScalaSetter" -> Data(false),
              "isScalaAccessor" -> Data(false),
              "isJavaGetter" -> Data(false),
              "isJavaSetter" -> Data(false),
              "isJavaAccessor" -> Data(false),
              "isAccessor" -> Data(false),
              "scalaAccessorName" -> Data("<no scala accessor name>"),
              "javaAccessorName" -> Data("<no java accessor name>"),
              "accessorName" -> Data("<no accessor name>")
            )
          )
        }

        test("for class with companion") {
          testMethodsExtraction[examples.methods.WithCompanion](
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
            "##",
            "writeReplace" // Scala 3-only, private def for serialization only
          ) <==> Data.map(
            "arg" -> Data.map(
              "invocation" -> Data("OnInstance"),
              "hasTypeParameters" -> Data(false),
              "position" -> methodsPosition(38, 27),
              "annotations" -> Data.list(),
              "isConstructor" -> Data(false),
              "isVal" -> Data(true),
              "isVar" -> Data(false),
              "isLazy" -> Data(false),
              "isDef" -> Data(false),
              "isFinal" -> Data(false),
              "isAbstract" -> Data(false),
              "isOverride" -> Data(false),
              "isImplicit" -> Data(false),
              "isDeclared" -> Data(true),
              "isSynthetic" -> Data(false),
              "isInherited" -> Data(false),
              "isAvailable(Everywhere)" -> Data(false),
              "isAvailable(AtCallSite)" -> Data(false),
              "arity" -> Data(0),
              "isNullary" -> Data(true),
              "isUnary" -> Data(false),
              "isBinary" -> Data(false),
              "isConstructorArgument" -> Data(true),
              "isCaseField" -> Data(false),
              "isScalaGetter" -> Data(true),
              "isScalaSetter" -> Data(false),
              "isScalaAccessor" -> Data(true),
              "isJavaGetter" -> Data(false),
              "isJavaSetter" -> Data(false),
              "isJavaAccessor" -> Data(false),
              "isAccessor" -> Data(true),
              "scalaAccessorName" -> Data("arg"),
              "javaAccessorName" -> Data("<no java accessor name>"),
              "accessorName" -> Data("arg")
            ),
            "method(Int)" -> Data.map(
              "invocation" -> Data("OnInstance"),
              "hasTypeParameters" -> Data(false),
              "position" -> methodsPosition(40, 7),
              "annotations" -> Data.list(),
              "isConstructor" -> Data(false),
              "isVal" -> Data(false),
              "isVar" -> Data(false),
              "isLazy" -> Data(false),
              "isDef" -> Data(true),
              "isFinal" -> Data(false),
              "isAbstract" -> Data(false),
              "isOverride" -> Data(false),
              "isImplicit" -> Data(false),
              "isDeclared" -> Data(true),
              "isSynthetic" -> Data(false),
              "isInherited" -> Data(false),
              "isAvailable(Everywhere)" -> Data(true),
              "isAvailable(AtCallSite)" -> Data(true),
              "arity" -> Data(1),
              "isNullary" -> Data(false),
              "isUnary" -> Data(true),
              "isBinary" -> Data(false),
              "isConstructorArgument" -> Data(false),
              "isCaseField" -> Data(false),
              "isScalaGetter" -> Data(false),
              "isScalaSetter" -> Data(false),
              "isScalaAccessor" -> Data(false),
              "isJavaGetter" -> Data(false),
              "isJavaSetter" -> Data(false),
              "isJavaAccessor" -> Data(false),
              "isAccessor" -> Data(false),
              "scalaAccessorName" -> Data("<no scala accessor name>"),
              "javaAccessorName" -> Data("<no java accessor name>"),
              "accessorName" -> Data("<no accessor name>")
            ),
            "apply(Int)" -> Data.map(
              "invocation" -> Data(
                if (LanguageVersion.byHearth.isScala2_13) "OnModule(WithCompanion)"
                else "OnModule(Ident(WithCompanion))"
              ),
              "hasTypeParameters" -> Data(false),
              "position" -> methodsPosition(44, 7),
              "annotations" -> Data.list(),
              "isConstructor" -> Data(false),
              "isVal" -> Data(false),
              "isVar" -> Data(false),
              "isLazy" -> Data(false),
              "isDef" -> Data(true),
              "isFinal" -> Data(false),
              "isAbstract" -> Data(false),
              "isOverride" -> Data(false),
              "isImplicit" -> Data(false),
              "isDeclared" -> Data(true),
              "isSynthetic" -> Data(false),
              "isInherited" -> Data(false),
              "isAvailable(Everywhere)" -> Data(true),
              "isAvailable(AtCallSite)" -> Data(true),
              "arity" -> Data(1),
              "isNullary" -> Data(false),
              "isUnary" -> Data(true),
              "isBinary" -> Data(false),
              "isConstructorArgument" -> Data(false),
              "isCaseField" -> Data(false),
              "isScalaGetter" -> Data(false),
              "isScalaSetter" -> Data(false),
              "isScalaAccessor" -> Data(false),
              "isJavaGetter" -> Data(false),
              "isJavaSetter" -> Data(false),
              "isJavaAccessor" -> Data(false),
              "isAccessor" -> Data(false),
              "scalaAccessorName" -> Data("<no scala accessor name>"),
              "javaAccessorName" -> Data("<no java accessor name>"),
              "accessorName" -> Data("<no accessor name>")
            ),
            "call(Int, Int)" -> Data.map(
              "invocation" -> Data(
                if (LanguageVersion.byHearth.isScala2_13) "OnModule(WithCompanion)"
                else "OnModule(Ident(WithCompanion))"
              ),
              "hasTypeParameters" -> Data(false),
              "position" -> methodsPosition(46, 7),
              "annotations" -> Data.list(),
              "isConstructor" -> Data(false),
              "isVal" -> Data(false),
              "isVar" -> Data(false),
              "isLazy" -> Data(false),
              "isDef" -> Data(true),
              "isFinal" -> Data(false),
              "isAbstract" -> Data(false),
              "isOverride" -> Data(false),
              "isImplicit" -> Data(false),
              "isDeclared" -> Data(true),
              "isSynthetic" -> Data(false),
              "isInherited" -> Data(false),
              "isAvailable(Everywhere)" -> Data(true),
              "isAvailable(AtCallSite)" -> Data(true),
              "arity" -> Data(2),
              "isNullary" -> Data(false),
              "isUnary" -> Data(false),
              "isBinary" -> Data(true),
              "isConstructorArgument" -> Data(false),
              "isCaseField" -> Data(false),
              "isScalaGetter" -> Data(false),
              "isScalaSetter" -> Data(false),
              "isScalaAccessor" -> Data(false),
              "isJavaGetter" -> Data(false),
              "isJavaSetter" -> Data(false),
              "isJavaAccessor" -> Data(false),
              "isAccessor" -> Data(false),
              "scalaAccessorName" -> Data("<no scala accessor name>"),
              "javaAccessorName" -> Data("<no java accessor name>"),
              "accessorName" -> Data("<no accessor name>")
            )
          )
        }

        test("for companion object") {
          testMethodsExtraction[examples.methods.WithCompanion.type](
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
            "##",
            "writeReplace" // Scala 3-only, private def for serialization only
          ) <==> Data.map(
            "apply(Int)" -> Data.map(
              "invocation" -> Data("OnInstance"),
              "hasTypeParameters" -> Data(false),
              "position" -> methodsPosition(44, 7),
              "annotations" -> Data.list(),
              "isConstructor" -> Data(false),
              "isVal" -> Data(false),
              "isVar" -> Data(false),
              "isLazy" -> Data(false),
              "isDef" -> Data(true),
              "isFinal" -> Data(false),
              "isAbstract" -> Data(false),
              "isOverride" -> Data(false),
              "isImplicit" -> Data(false),
              "isDeclared" -> Data(true),
              "isSynthetic" -> Data(false),
              "isInherited" -> Data(false),
              "isAvailable(Everywhere)" -> Data(true),
              "isAvailable(AtCallSite)" -> Data(true),
              "arity" -> Data(1),
              "isNullary" -> Data(false),
              "isUnary" -> Data(true),
              "isBinary" -> Data(false),
              "isConstructorArgument" -> Data(false),
              "isCaseField" -> Data(false),
              "isScalaGetter" -> Data(false),
              "isScalaSetter" -> Data(false),
              "isScalaAccessor" -> Data(false),
              "isJavaGetter" -> Data(false),
              "isJavaSetter" -> Data(false),
              "isJavaAccessor" -> Data(false),
              "isAccessor" -> Data(false),
              "scalaAccessorName" -> Data("<no scala accessor name>"),
              "javaAccessorName" -> Data("<no java accessor name>"),
              "accessorName" -> Data("<no accessor name>")
            ),
            "call(Int, Int)" -> Data.map(
              "invocation" -> Data("OnInstance"),
              "hasTypeParameters" -> Data(false),
              "position" -> methodsPosition(46, 7),
              "annotations" -> Data.list(),
              "isConstructor" -> Data(false),
              "isVal" -> Data(false),
              "isVar" -> Data(false),
              "isLazy" -> Data(false),
              "isDef" -> Data(true),
              "isFinal" -> Data(false),
              "isAbstract" -> Data(false),
              "isOverride" -> Data(false),
              "isImplicit" -> Data(false),
              "isDeclared" -> Data(true),
              "isSynthetic" -> Data(false),
              "isInherited" -> Data(false),
              "isAvailable(Everywhere)" -> Data(true),
              "isAvailable(AtCallSite)" -> Data(true),
              "arity" -> Data(2),
              "isNullary" -> Data(false),
              "isUnary" -> Data(false),
              "isBinary" -> Data(true),
              "isConstructorArgument" -> Data(false),
              "isCaseField" -> Data(false),
              "isScalaGetter" -> Data(false),
              "isScalaSetter" -> Data(false),
              "isScalaAccessor" -> Data(false),
              "isJavaGetter" -> Data(false),
              "isJavaSetter" -> Data(false),
              "isJavaAccessor" -> Data(false),
              "isAccessor" -> Data(false),
              "scalaAccessorName" -> Data("<no scala accessor name>"),
              "javaAccessorName" -> Data("<no java accessor name>"),
              "accessorName" -> Data("<no accessor name>")
            )
          )
        }

        test("for scope visibility from the outside") {
          testMethodsExtraction[examples.methods.ScopeVisibility](
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
            "##",
            "writeReplace" // Scala 3-only, private def for serialization only
          ) <==> Data.map(
            "privateCtorArg" -> Data.map(
              "invocation" -> Data("OnInstance"),
              "hasTypeParameters" -> Data(false),
              "position" -> methodsPosition(50, 32),
              "annotations" -> Data.list(),
              "isConstructor" -> Data(false),
              "isVal" -> Data(true),
              "isVar" -> Data(false),
              "isLazy" -> Data(false),
              "isDef" -> Data(false),
              "isFinal" -> Data(false),
              "isAbstract" -> Data(false),
              "isOverride" -> Data(false),
              "isImplicit" -> Data(false),
              "isDeclared" -> Data(true),
              "isSynthetic" -> Data(false),
              "isInherited" -> Data(false),
              "isAvailable(Everywhere)" -> Data(false),
              "isAvailable(AtCallSite)" -> Data(false),
              "arity" -> Data(0),
              "isNullary" -> Data(true),
              "isUnary" -> Data(false),
              "isBinary" -> Data(false),
              "isConstructorArgument" -> Data(true),
              "isCaseField" -> Data(false),
              "isScalaGetter" -> Data(true),
              "isScalaSetter" -> Data(false),
              "isScalaAccessor" -> Data(true),
              "isJavaGetter" -> Data(false),
              "isJavaSetter" -> Data(false),
              "isJavaAccessor" -> Data(false),
              "isAccessor" -> Data(true),
              "scalaAccessorName" -> Data("privateCtorArg"),
              "javaAccessorName" -> Data("<no java accessor name>"),
              "accessorName" -> Data("privateCtorArg")
            ),
            "publicCtorArg" -> Data.map(
              "invocation" -> Data("OnInstance"),
              "hasTypeParameters" -> Data(false),
              "position" -> methodsPosition(50, 57),
              "annotations" -> Data.list(),
              "isConstructor" -> Data(false),
              "isVal" -> Data(true),
              "isVar" -> Data(false),
              "isLazy" -> Data(false),
              "isDef" -> Data(false),
              "isFinal" -> Data(false),
              "isAbstract" -> Data(false),
              "isOverride" -> Data(false),
              "isImplicit" -> Data(false),
              "isDeclared" -> Data(true),
              "isSynthetic" -> Data(false),
              "isInherited" -> Data(false),
              "isAvailable(Everywhere)" -> Data(true),
              "isAvailable(AtCallSite)" -> Data(true),
              "arity" -> Data(0),
              "isNullary" -> Data(true),
              "isUnary" -> Data(false),
              "isBinary" -> Data(false),
              "isConstructorArgument" -> Data(true),
              "isCaseField" -> Data(false),
              "isScalaGetter" -> Data(true),
              "isScalaSetter" -> Data(false),
              "isScalaAccessor" -> Data(true),
              "isJavaGetter" -> Data(false),
              "isJavaSetter" -> Data(false),
              "isJavaAccessor" -> Data(false),
              "isAccessor" -> Data(true),
              "scalaAccessorName" -> Data("publicCtorArg"),
              "javaAccessorName" -> Data("<no java accessor name>"),
              "accessorName" -> Data("publicCtorArg")
            ),
            "publicMethod" -> Data.map(
              "invocation" -> Data("OnInstance"),
              "hasTypeParameters" -> Data(false),
              "position" -> methodsPosition(52, 7),
              "annotations" -> Data.list(),
              "isConstructor" -> Data(false),
              "isVal" -> Data(false),
              "isVar" -> Data(false),
              "isLazy" -> Data(false),
              "isDef" -> Data(true),
              "isFinal" -> Data(false),
              "isAbstract" -> Data(false),
              "isOverride" -> Data(false),
              "isImplicit" -> Data(false),
              "isDeclared" -> Data(true),
              "isSynthetic" -> Data(false),
              "isInherited" -> Data(false),
              "isAvailable(Everywhere)" -> Data(true),
              "isAvailable(AtCallSite)" -> Data(true),
              "arity" -> Data(0),
              "isNullary" -> Data(true),
              "isUnary" -> Data(false),
              "isBinary" -> Data(false),
              "isConstructorArgument" -> Data(false),
              "isCaseField" -> Data(false),
              "isScalaGetter" -> Data(false),
              "isScalaSetter" -> Data(false),
              "isJavaGetter" -> Data(false),
              "isScalaAccessor" -> Data(false),
              "isJavaSetter" -> Data(false),
              "isJavaAccessor" -> Data(false),
              "isAccessor" -> Data(false),
              "scalaAccessorName" -> Data("<no scala accessor name>"),
              "javaAccessorName" -> Data("<no java accessor name>"),
              "accessorName" -> Data("<no accessor name>")
            ),
            "privateMethod" -> Data.map(
              "invocation" -> Data("OnInstance"),
              "hasTypeParameters" -> Data(false),
              "position" -> methodsPosition(54, 15),
              "annotations" -> Data.list(),
              "isConstructor" -> Data(false),
              "isVal" -> Data(false),
              "isVar" -> Data(false),
              "isLazy" -> Data(false),
              "isDef" -> Data(true),
              "isFinal" -> Data(false),
              "isAbstract" -> Data(false),
              "isOverride" -> Data(false),
              "isImplicit" -> Data(false),
              "isDeclared" -> Data(true),
              "isSynthetic" -> Data(false),
              "isInherited" -> Data(false),
              "isAvailable(Everywhere)" -> Data(false),
              "isAvailable(AtCallSite)" -> Data(false),
              "arity" -> Data(0),
              "isNullary" -> Data(true),
              "isUnary" -> Data(false),
              "isBinary" -> Data(false),
              "isConstructorArgument" -> Data(false),
              "isCaseField" -> Data(false),
              "isScalaGetter" -> Data(false),
              "isScalaSetter" -> Data(false),
              "isJavaGetter" -> Data(false),
              "isScalaAccessor" -> Data(false),
              "isJavaSetter" -> Data(false),
              "isJavaAccessor" -> Data(false),
              "isAccessor" -> Data(false),
              "scalaAccessorName" -> Data("<no scala accessor name>"),
              "javaAccessorName" -> Data("<no java accessor name>"),
              "accessorName" -> Data("<no accessor name>")
            ),
            "protectedMethod" -> Data.map(
              "invocation" -> Data("OnInstance"),
              "hasTypeParameters" -> Data(false),
              "position" -> methodsPosition(56, 17),
              "annotations" -> Data.list(),
              "isConstructor" -> Data(false),
              "isVal" -> Data(false),
              "isVar" -> Data(false),
              "isLazy" -> Data(false),
              "isDef" -> Data(true),
              "isFinal" -> Data(false),
              "isAbstract" -> Data(false),
              "isOverride" -> Data(false),
              "isImplicit" -> Data(false),
              "isDeclared" -> Data(true),
              "isSynthetic" -> Data(false),
              "isInherited" -> Data(false),
              "isAvailable(Everywhere)" -> Data(false),
              "isAvailable(AtCallSite)" -> Data(false),
              "arity" -> Data(0),
              "isNullary" -> Data(true),
              "isUnary" -> Data(false),
              "isBinary" -> Data(false),
              "isConstructorArgument" -> Data(false),
              "isCaseField" -> Data(false),
              "isScalaGetter" -> Data(false),
              "isScalaSetter" -> Data(false),
              "isJavaGetter" -> Data(false),
              "isScalaAccessor" -> Data(false),
              "isJavaSetter" -> Data(false),
              "isJavaAccessor" -> Data(false),
              "isAccessor" -> Data(false),
              "scalaAccessorName" -> Data("<no scala accessor name>"),
              "javaAccessorName" -> Data("<no java accessor name>"),
              "accessorName" -> Data("<no accessor name>")
            ),
            "privateThisMethod" -> Data.map(
              "invocation" -> Data("OnInstance"),
              "hasTypeParameters" -> Data(false),
              "position" -> methodsPosition(58, 21),
              "annotations" -> Data.list(),
              "isConstructor" -> Data(false),
              "isVal" -> Data(false),
              "isVar" -> Data(false),
              "isLazy" -> Data(false),
              "isDef" -> Data(true),
              "isFinal" -> Data(false),
              "isAbstract" -> Data(false),
              "isOverride" -> Data(false),
              "isImplicit" -> Data(false),
              "isDeclared" -> Data(true),
              "isSynthetic" -> Data(false),
              "isInherited" -> Data(false),
              "isAvailable(Everywhere)" -> Data(false),
              "isAvailable(AtCallSite)" -> Data(false),
              "arity" -> Data(0),
              "isNullary" -> Data(true),
              "isUnary" -> Data(false),
              "isBinary" -> Data(false),
              "isConstructorArgument" -> Data(false),
              "isCaseField" -> Data(false),
              "isScalaGetter" -> Data(false),
              "isScalaSetter" -> Data(false),
              "isJavaGetter" -> Data(false),
              "isScalaAccessor" -> Data(false),
              "isJavaSetter" -> Data(false),
              "isJavaAccessor" -> Data(false),
              "isAccessor" -> Data(false),
              "scalaAccessorName" -> Data("<no scala accessor name>"),
              "javaAccessorName" -> Data("<no java accessor name>"),
              "accessorName" -> Data("<no accessor name>")
            ),
            "privateHearthMethod" -> Data.map(
              "invocation" -> Data("OnInstance"),
              "hasTypeParameters" -> Data(false),
              "position" -> methodsPosition(60, 23),
              "annotations" -> Data.list(),
              "isConstructor" -> Data(false),
              "isVal" -> Data(false),
              "isVar" -> Data(false),
              "isLazy" -> Data(false),
              "isDef" -> Data(true),
              "isFinal" -> Data(false),
              "isAbstract" -> Data(false),
              "isOverride" -> Data(false),
              "isImplicit" -> Data(false),
              "isDeclared" -> Data(true),
              "isSynthetic" -> Data(false),
              "isInherited" -> Data(false),
              "isAvailable(Everywhere)" -> Data(false),
              "isAvailable(AtCallSite)" -> Data(true),
              "arity" -> Data(0),
              "isNullary" -> Data(true),
              "isUnary" -> Data(false),
              "isBinary" -> Data(false),
              "isConstructorArgument" -> Data(false),
              "isCaseField" -> Data(false),
              "isScalaGetter" -> Data(false),
              "isScalaSetter" -> Data(false),
              "isJavaGetter" -> Data(false),
              "isScalaAccessor" -> Data(false),
              "isJavaSetter" -> Data(false),
              "isJavaAccessor" -> Data(false),
              "isAccessor" -> Data(false),
              "scalaAccessorName" -> Data("<no scala accessor name>"),
              "javaAccessorName" -> Data("<no java accessor name>"),
              "accessorName" -> Data("<no accessor name>")
            ),
            "privateHearthExamplesMethod" -> Data.map(
              "invocation" -> Data("OnInstance"),
              "hasTypeParameters" -> Data(false),
              "position" -> methodsPosition(62, 25),
              "annotations" -> Data.list(),
              "isConstructor" -> Data(false),
              "isVal" -> Data(false),
              "isVar" -> Data(false),
              "isLazy" -> Data(false),
              "isDef" -> Data(true),
              "isFinal" -> Data(false),
              "isAbstract" -> Data(false),
              "isOverride" -> Data(false),
              "isImplicit" -> Data(false),
              "isDeclared" -> Data(true),
              "isSynthetic" -> Data(false),
              "isInherited" -> Data(false),
              "isAvailable(Everywhere)" -> Data(false),
              "isAvailable(AtCallSite)" -> Data(false),
              "arity" -> Data(0),
              "isNullary" -> Data(true),
              "isUnary" -> Data(false),
              "isBinary" -> Data(false),
              "isConstructorArgument" -> Data(false),
              "isCaseField" -> Data(false),
              "isScalaGetter" -> Data(false),
              "isScalaSetter" -> Data(false),
              "isJavaGetter" -> Data(false),
              "isScalaAccessor" -> Data(false),
              "isJavaSetter" -> Data(false),
              "isJavaAccessor" -> Data(false),
              "isAccessor" -> Data(false),
              "scalaAccessorName" -> Data("<no scala accessor name>"),
              "javaAccessorName" -> Data("<no java accessor name>"),
              "accessorName" -> Data("<no accessor name>")
            )
          )
        }

        test("for scope visibility from the inside") {
          class ScopeVisibilityExtending extends examples.methods.ScopeVisibility(0, 0) {

            def expandMacroInsideAClass: Data = testMethodsExtraction[examples.methods.ScopeVisibility](
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
              "##",
              "writeReplace" // Scala 3-only, private def for serialization only
            )
          }

          (new ScopeVisibilityExtending).expandMacroInsideAClass <==> Data.map(
            "privateCtorArg" -> Data.map(
              "invocation" -> Data("OnInstance"),
              "hasTypeParameters" -> Data(false),
              "position" -> methodsPosition(50, 32),
              "annotations" -> Data.list(),
              "isConstructor" -> Data(false),
              "isVal" -> Data(true),
              "isVar" -> Data(false),
              "isLazy" -> Data(false),
              "isDef" -> Data(false),
              "isFinal" -> Data(false),
              "isAbstract" -> Data(false),
              "isOverride" -> Data(false),
              "isImplicit" -> Data(false),
              "isDeclared" -> Data(true),
              "isSynthetic" -> Data(false),
              "isInherited" -> Data(false),
              "isAvailable(Everywhere)" -> Data(false),
              "isAvailable(AtCallSite)" -> Data(false),
              "arity" -> Data(0),
              "isNullary" -> Data(true),
              "isUnary" -> Data(false),
              "isBinary" -> Data(false),
              "isConstructorArgument" -> Data(true),
              "isCaseField" -> Data(false),
              "isScalaGetter" -> Data(true),
              "isScalaSetter" -> Data(false),
              "isScalaAccessor" -> Data(true),
              "isJavaGetter" -> Data(false),
              "isJavaSetter" -> Data(false),
              "isJavaAccessor" -> Data(false),
              "isAccessor" -> Data(true),
              "scalaAccessorName" -> Data("privateCtorArg"),
              "javaAccessorName" -> Data("<no java accessor name>"),
              "accessorName" -> Data("privateCtorArg")
            ),
            "publicCtorArg" -> Data.map(
              "invocation" -> Data("OnInstance"),
              "hasTypeParameters" -> Data(false),
              "position" -> methodsPosition(50, 57),
              "annotations" -> Data.list(),
              "isConstructor" -> Data(false),
              "isVal" -> Data(true),
              "isVar" -> Data(false),
              "isLazy" -> Data(false),
              "isDef" -> Data(false),
              "isFinal" -> Data(false),
              "isAbstract" -> Data(false),
              "isOverride" -> Data(false),
              "isImplicit" -> Data(false),
              "isDeclared" -> Data(true),
              "isSynthetic" -> Data(false),
              "isInherited" -> Data(false),
              "isAvailable(Everywhere)" -> Data(true),
              "isAvailable(AtCallSite)" -> Data(true),
              "arity" -> Data(0),
              "isNullary" -> Data(true),
              "isUnary" -> Data(false),
              "isBinary" -> Data(false),
              "isConstructorArgument" -> Data(true),
              "isCaseField" -> Data(false),
              "isScalaGetter" -> Data(true),
              "isScalaSetter" -> Data(false),
              "isScalaAccessor" -> Data(true),
              "isJavaGetter" -> Data(false),
              "isJavaSetter" -> Data(false),
              "isJavaAccessor" -> Data(false),
              "isAccessor" -> Data(true),
              "scalaAccessorName" -> Data("publicCtorArg"),
              "javaAccessorName" -> Data("<no java accessor name>"),
              "accessorName" -> Data("publicCtorArg")
            ),
            "publicMethod" -> Data.map(
              "invocation" -> Data("OnInstance"),
              "hasTypeParameters" -> Data(false),
              "position" -> methodsPosition(52, 7),
              "annotations" -> Data.list(),
              "isConstructor" -> Data(false),
              "isVal" -> Data(false),
              "isVar" -> Data(false),
              "isLazy" -> Data(false),
              "isDef" -> Data(true),
              "isFinal" -> Data(false),
              "isAbstract" -> Data(false),
              "isOverride" -> Data(false),
              "isImplicit" -> Data(false),
              "isDeclared" -> Data(true),
              "isSynthetic" -> Data(false),
              "isInherited" -> Data(false),
              "isAvailable(Everywhere)" -> Data(true),
              "isAvailable(AtCallSite)" -> Data(true),
              "arity" -> Data(0),
              "isNullary" -> Data(true),
              "isUnary" -> Data(false),
              "isBinary" -> Data(false),
              "isConstructorArgument" -> Data(false),
              "isCaseField" -> Data(false),
              "isScalaGetter" -> Data(false),
              "isScalaSetter" -> Data(false),
              "isJavaGetter" -> Data(false),
              "isScalaAccessor" -> Data(false),
              "isJavaSetter" -> Data(false),
              "isJavaAccessor" -> Data(false),
              "isAccessor" -> Data(false),
              "scalaAccessorName" -> Data("<no scala accessor name>"),
              "javaAccessorName" -> Data("<no java accessor name>"),
              "accessorName" -> Data("<no accessor name>")
            ),
            "privateMethod" -> Data.map(
              "invocation" -> Data("OnInstance"),
              "hasTypeParameters" -> Data(false),
              "position" -> methodsPosition(54, 15),
              "annotations" -> Data.list(),
              "isConstructor" -> Data(false),
              "isVal" -> Data(false),
              "isVar" -> Data(false),
              "isLazy" -> Data(false),
              "isDef" -> Data(true),
              "isFinal" -> Data(false),
              "isAbstract" -> Data(false),
              "isOverride" -> Data(false),
              "isImplicit" -> Data(false),
              "isDeclared" -> Data(true),
              "isSynthetic" -> Data(false),
              "isInherited" -> Data(false),
              "isAvailable(Everywhere)" -> Data(false),
              "isAvailable(AtCallSite)" -> Data(false),
              "arity" -> Data(0),
              "isNullary" -> Data(true),
              "isUnary" -> Data(false),
              "isBinary" -> Data(false),
              "isConstructorArgument" -> Data(false),
              "isCaseField" -> Data(false),
              "isScalaGetter" -> Data(false),
              "isScalaSetter" -> Data(false),
              "isJavaGetter" -> Data(false),
              "isScalaAccessor" -> Data(false),
              "isJavaSetter" -> Data(false),
              "isJavaAccessor" -> Data(false),
              "isAccessor" -> Data(false),
              "scalaAccessorName" -> Data("<no scala accessor name>"),
              "javaAccessorName" -> Data("<no java accessor name>"),
              "accessorName" -> Data("<no accessor name>")
            ),
            "protectedMethod" -> Data.map(
              "invocation" -> Data("OnInstance"),
              "hasTypeParameters" -> Data(false),
              "position" -> methodsPosition(56, 17),
              "annotations" -> Data.list(),
              "isConstructor" -> Data(false),
              "isVal" -> Data(false),
              "isVar" -> Data(false),
              "isLazy" -> Data(false),
              "isDef" -> Data(true),
              "isFinal" -> Data(false),
              "isAbstract" -> Data(false),
              "isOverride" -> Data(false),
              "isImplicit" -> Data(false),
              "isDeclared" -> Data(true),
              "isSynthetic" -> Data(false),
              "isInherited" -> Data(false),
              "isAvailable(Everywhere)" -> Data(false),
              "isAvailable(AtCallSite)" -> Data(false),
              "arity" -> Data(0),
              "isNullary" -> Data(true),
              "isUnary" -> Data(false),
              "isBinary" -> Data(false),
              "isConstructorArgument" -> Data(false),
              "isCaseField" -> Data(false),
              "isScalaGetter" -> Data(false),
              "isScalaSetter" -> Data(false),
              "isJavaGetter" -> Data(false),
              "isScalaAccessor" -> Data(false),
              "isJavaSetter" -> Data(false),
              "isJavaAccessor" -> Data(false),
              "isAccessor" -> Data(false),
              "scalaAccessorName" -> Data("<no scala accessor name>"),
              "javaAccessorName" -> Data("<no java accessor name>"),
              "accessorName" -> Data("<no accessor name>")
            ),
            "privateThisMethod" -> Data.map(
              "invocation" -> Data("OnInstance"),
              "hasTypeParameters" -> Data(false),
              "position" -> methodsPosition(58, 21),
              "annotations" -> Data.list(),
              "isConstructor" -> Data(false),
              "isVal" -> Data(false),
              "isVar" -> Data(false),
              "isLazy" -> Data(false),
              "isDef" -> Data(true),
              "isFinal" -> Data(false),
              "isAbstract" -> Data(false),
              "isOverride" -> Data(false),
              "isImplicit" -> Data(false),
              "isDeclared" -> Data(true),
              "isSynthetic" -> Data(false),
              "isInherited" -> Data(false),
              "isAvailable(Everywhere)" -> Data(false),
              "isAvailable(AtCallSite)" -> Data(false),
              "arity" -> Data(0),
              "isNullary" -> Data(true),
              "isUnary" -> Data(false),
              "isBinary" -> Data(false),
              "isConstructorArgument" -> Data(false),
              "isCaseField" -> Data(false),
              "isScalaGetter" -> Data(false),
              "isScalaSetter" -> Data(false),
              "isJavaGetter" -> Data(false),
              "isScalaAccessor" -> Data(false),
              "isJavaSetter" -> Data(false),
              "isJavaAccessor" -> Data(false),
              "isAccessor" -> Data(false),
              "scalaAccessorName" -> Data("<no scala accessor name>"),
              "javaAccessorName" -> Data("<no java accessor name>"),
              "accessorName" -> Data("<no accessor name>")
            ),
            "privateHearthMethod" -> Data.map(
              "invocation" -> Data("OnInstance"),
              "hasTypeParameters" -> Data(false),
              "position" -> methodsPosition(60, 23),
              "annotations" -> Data.list(),
              "isConstructor" -> Data(false),
              "isVal" -> Data(false),
              "isVar" -> Data(false),
              "isLazy" -> Data(false),
              "isDef" -> Data(true),
              "isFinal" -> Data(false),
              "isAbstract" -> Data(false),
              "isOverride" -> Data(false),
              "isImplicit" -> Data(false),
              "isDeclared" -> Data(true),
              "isSynthetic" -> Data(false),
              "isInherited" -> Data(false),
              "isAvailable(Everywhere)" -> Data(false),
              "isAvailable(AtCallSite)" -> Data(true),
              "arity" -> Data(0),
              "isNullary" -> Data(true),
              "isUnary" -> Data(false),
              "isBinary" -> Data(false),
              "isConstructorArgument" -> Data(false),
              "isCaseField" -> Data(false),
              "isScalaGetter" -> Data(false),
              "isScalaSetter" -> Data(false),
              "isJavaGetter" -> Data(false),
              "isScalaAccessor" -> Data(false),
              "isJavaSetter" -> Data(false),
              "isJavaAccessor" -> Data(false),
              "isAccessor" -> Data(false),
              "scalaAccessorName" -> Data("<no scala accessor name>"),
              "javaAccessorName" -> Data("<no java accessor name>"),
              "accessorName" -> Data("<no accessor name>")
            ),
            "privateHearthExamplesMethod" -> Data.map(
              "invocation" -> Data("OnInstance"),
              "hasTypeParameters" -> Data(false),
              "position" -> methodsPosition(62, 25),
              "annotations" -> Data.list(),
              "isConstructor" -> Data(false),
              "isVal" -> Data(false),
              "isVar" -> Data(false),
              "isLazy" -> Data(false),
              "isDef" -> Data(true),
              "isFinal" -> Data(false),
              "isAbstract" -> Data(false),
              "isOverride" -> Data(false),
              "isImplicit" -> Data(false),
              "isDeclared" -> Data(true),
              "isSynthetic" -> Data(false),
              "isInherited" -> Data(false),
              "isAvailable(Everywhere)" -> Data(false),
              "isAvailable(AtCallSite)" -> Data(false),
              "arity" -> Data(0),
              "isNullary" -> Data(true),
              "isUnary" -> Data(false),
              "isBinary" -> Data(false),
              "isConstructorArgument" -> Data(false),
              "isCaseField" -> Data(false),
              "isScalaGetter" -> Data(false),
              "isScalaSetter" -> Data(false),
              "isJavaGetter" -> Data(false),
              "isScalaAccessor" -> Data(false),
              "isJavaSetter" -> Data(false),
              "isJavaAccessor" -> Data(false),
              "isAccessor" -> Data(false),
              "scalaAccessorName" -> Data("<no scala accessor name>"),
              "javaAccessorName" -> Data("<no java accessor name>"),
              "accessorName" -> Data("<no accessor name>")
            )
          )
        }
      }

      group("isImplicit: method-level and parameter-level implicit detection") {
        import MethodsFixtures.testMethodsExtraction
        import MethodsFixtures.testParameterProperties

        test("method-level isImplicit for class with implicit members") {
          testMethodsExtraction[examples.methods.WithImplicits](
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
          ) <==> Data.map(
            "implicitValue" -> Data.map(
              "invocation" -> Data("OnInstance"),
              "hasTypeParameters" -> Data(false),
              "position" -> methodsPosition(82, 3),
              "annotations" -> Data.list(),
              "isConstructor" -> Data(false),
              "isVal" -> Data(true),
              "isVar" -> Data(false),
              "isLazy" -> Data(false),
              "isDef" -> Data(false),
              "isFinal" -> Data(false),
              "isAbstract" -> Data(false),
              "isOverride" -> Data(false),
              "isImplicit" -> Data(true),
              "isDeclared" -> Data(true),
              "isSynthetic" -> Data(false),
              "isInherited" -> Data(false),
              "isAvailable(Everywhere)" -> Data(true),
              "isAvailable(AtCallSite)" -> Data(true),
              "arity" -> Data(0),
              "isNullary" -> Data(true),
              "isUnary" -> Data(false),
              "isBinary" -> Data(false),
              "isConstructorArgument" -> Data(false),
              "isCaseField" -> Data(false),
              "isScalaGetter" -> Data(true),
              "isScalaSetter" -> Data(false),
              "isScalaAccessor" -> Data(true),
              "isJavaGetter" -> Data(false),
              "isJavaSetter" -> Data(false),
              "isJavaAccessor" -> Data(false),
              "isAccessor" -> Data(true),
              "scalaAccessorName" -> Data("implicitValue"),
              "javaAccessorName" -> Data("<no java accessor name>"),
              "accessorName" -> Data("implicitValue")
            ),
            "implicitConversion(Int)" -> Data.map(
              "invocation" -> Data("OnInstance"),
              "hasTypeParameters" -> Data(false),
              "position" -> methodsPosition(83, 3),
              "annotations" -> Data.list(),
              "isConstructor" -> Data(false),
              "isVal" -> Data(false),
              "isVar" -> Data(false),
              "isLazy" -> Data(false),
              "isDef" -> Data(true),
              "isFinal" -> Data(false),
              "isAbstract" -> Data(false),
              "isOverride" -> Data(false),
              "isImplicit" -> Data(true),
              "isDeclared" -> Data(true),
              "isSynthetic" -> Data(false),
              "isInherited" -> Data(false),
              "isAvailable(Everywhere)" -> Data(true),
              "isAvailable(AtCallSite)" -> Data(true),
              "arity" -> Data(1),
              "isNullary" -> Data(false),
              "isUnary" -> Data(true),
              "isBinary" -> Data(false),
              "isConstructorArgument" -> Data(false),
              "isCaseField" -> Data(false),
              "isScalaGetter" -> Data(false),
              "isScalaSetter" -> Data(false),
              "isScalaAccessor" -> Data(false),
              "isJavaGetter" -> Data(false),
              "isJavaSetter" -> Data(false),
              "isJavaAccessor" -> Data(false),
              "isAccessor" -> Data(false),
              "scalaAccessorName" -> Data("<no scala accessor name>"),
              "javaAccessorName" -> Data("<no java accessor name>"),
              "accessorName" -> Data("<no accessor name>")
            ),
            "methodWithImplicitParam(Int)" -> Data.map(
              "invocation" -> Data("OnInstance"),
              "hasTypeParameters" -> Data(false),
              "position" -> methodsPosition(84, 3),
              "annotations" -> Data.list(),
              "isConstructor" -> Data(false),
              "isVal" -> Data(false),
              "isVar" -> Data(false),
              "isLazy" -> Data(false),
              "isDef" -> Data(true),
              "isFinal" -> Data(false),
              "isAbstract" -> Data(false),
              "isOverride" -> Data(false),
              "isImplicit" -> Data(false),
              "isDeclared" -> Data(true),
              "isSynthetic" -> Data(false),
              "isInherited" -> Data(false),
              "isAvailable(Everywhere)" -> Data(true),
              "isAvailable(AtCallSite)" -> Data(true),
              "arity" -> Data(1),
              "isNullary" -> Data(false),
              "isUnary" -> Data(true),
              "isBinary" -> Data(false),
              "isConstructorArgument" -> Data(false),
              "isCaseField" -> Data(false),
              "isScalaGetter" -> Data(false),
              "isScalaSetter" -> Data(false),
              "isScalaAccessor" -> Data(false),
              "isJavaGetter" -> Data(false),
              "isJavaSetter" -> Data(false),
              "isJavaAccessor" -> Data(false),
              "isAccessor" -> Data(false),
              "scalaAccessorName" -> Data("<no scala accessor name>"),
              "javaAccessorName" -> Data("<no java accessor name>"),
              "accessorName" -> Data("<no accessor name>")
            )
          )
        }

        test("parameter-level isImplicit for implicit parameters") {
          testParameterProperties[examples.methods.WithImplicits]("methodWithImplicitParam") <==> Data.map(
            "x" -> Data.map(
              "isImplicit" -> Data(true),
              "hasDefault" -> Data(false),
              "isByName" -> Data(false),
              "isVararg" -> Data(false),
              "byNameUnderlying" -> Data("<none>")
            )
          )
        }

        test("parameter-level isImplicit for implicit def parameter") {
          testParameterProperties[examples.methods.WithImplicits]("implicitConversion") <==> Data.map(
            "x" -> Data.map(
              "isImplicit" -> Data(false),
              "hasDefault" -> Data(false),
              "isByName" -> Data(false),
              "isVararg" -> Data(false),
              "byNameUnderlying" -> Data("<none>")
            )
          )
        }

        test("parameter-level isImplicit for normal method parameters") {
          testParameterProperties[examples.methods.NoCompanionClass]("method") <==> Data.map(
            "arg" -> Data.map(
              "isImplicit" -> Data(false),
              "hasDefault" -> Data(false),
              "isByName" -> Data(false),
              "isVararg" -> Data(false),
              "byNameUnderlying" -> Data("<none>")
            )
          )
        }
      }

      group("isVararg: vararg (repeated) parameter detection and calls") {
        import MethodsFixtures.{testCallVarargIntMethod, testConstructVarargCtor, testParameterProperties}

        test("parameter-level isVararg for vararg method parameter") {
          testParameterProperties[examples.methods.WithVarargs]("varargMethod") <==> Data.map(
            "xs" -> Data.map(
              "isImplicit" -> Data(false),
              "hasDefault" -> Data(false),
              "isByName" -> Data(false),
              "isVararg" -> Data(true),
              "byNameUnderlying" -> Data("<none>")
            )
          )
        }

        test("parameter-level isVararg is false for normal parameter") {
          testParameterProperties[examples.methods.WithVarargs]("normalMethod") <==> Data.map(
            "x" -> Data.map(
              "isImplicit" -> Data(false),
              "hasDefault" -> Data(false),
              "isByName" -> Data(false),
              "isVararg" -> Data(false),
              "byNameUnderlying" -> Data("<none>")
            )
          )
        }

        test("parameter-level isVararg is false for by-name parameter") {
          testParameterProperties[examples.methods.WithVarargs]("byNameMethod") <==> Data.map(
            "x" -> Data.map(
              "isImplicit" -> Data(false),
              "hasDefault" -> Data(false),
              "isByName" -> Data(true),
              "isVararg" -> Data(false),
              "byNameUnderlying" -> Data("scala.Int")
            )
          )
        }

        test("parameter-level isVararg for vararg constructor parameter") {
          MethodsFixtures.testConstructorsExtraction[examples.methods.WithVarargsCtor] <==> Data.map(
            "primaryConstructor" -> Data("(xs: scala.collection.immutable.Seq[java.lang.String])"),
            "defaultConstructor" -> Data("<no default constructor>"),
            "constructors" -> Data.list(
              Data("(xs: scala.collection.immutable.Seq[java.lang.String])")
            )
          )
        }

        test("vararg method rendered as A* in plainPrint") {
          val data = MethodsFixtures.testMethodPrettyPrint[examples.methods.WithVarargs]("varargMethod")
          val map = data.asList.get.head.asMap.get
          val plain = map("plainPrint").asString.get
          val stripped = map("prettyPrintStripped").asString.get
          assert(plain.contains("(xs: scala.Int*)"), s"plainPrint should render vararg as scala.Int*, got: $plain")
          assert(stripped == plain, s"stripped: $stripped\nplain:    $plain")
        }

        test("calling a vararg method with multiple values via Method API") {
          testCallVarargIntMethod[examples.methods.WithVarargs](new examples.methods.WithVarargs)("varargMethod")(
            1,
            2,
            3
          ) ==> 6
        }

        test("calling a vararg method with no values via Method API") {
          testCallVarargIntMethod[examples.methods.WithVarargs](new examples.methods.WithVarargs)(
            "varargMethod"
          )() ==> 0
        }

        test("constructing a class with vararg constructor via Method API") {
          testConstructVarargCtor[examples.methods.WithVarargsCtor]("a", "b", "c") <==> Data(
            "WithVarargsCtor(a,b,c)"
          )
        }
      }

      // A `var` constructor parameter produces a getter and a setter (name_=); only the getter may be flagged as a
      // constructor argument / case field — the setter must not be.
      group("regression: setters are not constructor arguments nor case fields") {
        import MethodsFixtures.testMethodProperties

        test("plain class with var constructor parameter: getter is the constructor argument") {
          testMethodProperties[examples.methods.WithVarCtorParam]("name") <==> Data.map(
            "name" -> Data.map(
              "invocation" -> Data("OnInstance"),
              "hasTypeParameters" -> Data(false),
              "position" -> methodsPosition(111, 33),
              "annotations" -> Data.list(),
              "isConstructor" -> Data(false),
              "isVal" -> Data(false),
              "isVar" -> Data(true),
              "isLazy" -> Data(false),
              "isDef" -> Data(false),
              "isFinal" -> Data(false),
              "isAbstract" -> Data(false),
              "isOverride" -> Data(false),
              "isImplicit" -> Data(false),
              "isDeclared" -> Data(true),
              "isSynthetic" -> Data(false),
              "isInherited" -> Data(false),
              "isAvailable(Everywhere)" -> Data(true),
              "isAvailable(AtCallSite)" -> Data(true),
              "arity" -> Data(0),
              "isNullary" -> Data(true),
              "isUnary" -> Data(false),
              "isBinary" -> Data(false),
              "isConstructorArgument" -> Data(true),
              "isCaseField" -> Data(false),
              "isScalaGetter" -> Data(true),
              "isScalaSetter" -> Data(false),
              "isScalaAccessor" -> Data(true),
              "isJavaGetter" -> Data(false),
              "isJavaSetter" -> Data(false),
              "isJavaAccessor" -> Data(false),
              "isAccessor" -> Data(true),
              "scalaAccessorName" -> Data("name"),
              "javaAccessorName" -> Data("<no java accessor name>"),
              "accessorName" -> Data("name")
            )
          )
        }

        test("plain class with var constructor parameter: setter is not a constructor argument") {
          testMethodProperties[examples.methods.WithVarCtorParam]("name_=") <==> Data.map(
            "name_=(String)" -> Data.map(
              "invocation" -> Data("OnInstance"),
              "hasTypeParameters" -> Data(false),
              "position" -> methodsPosition(111, 33),
              "annotations" -> Data.list(),
              "isConstructor" -> Data(false),
              "isVal" -> Data(false),
              "isVar" -> Data(true),
              "isLazy" -> Data(false),
              "isDef" -> Data(true),
              "isFinal" -> Data(false),
              "isAbstract" -> Data(false),
              "isOverride" -> Data(false),
              "isImplicit" -> Data(false),
              "isDeclared" -> Data(true),
              "isSynthetic" -> Data(false),
              "isInherited" -> Data(false),
              "isAvailable(Everywhere)" -> Data(true),
              "isAvailable(AtCallSite)" -> Data(true),
              "arity" -> Data(1),
              "isNullary" -> Data(false),
              "isUnary" -> Data(true),
              "isBinary" -> Data(false),
              "isConstructorArgument" -> Data(false),
              "isCaseField" -> Data(false),
              "isScalaGetter" -> Data(false),
              "isScalaSetter" -> Data(true),
              "isScalaAccessor" -> Data(true),
              "isJavaGetter" -> Data(false),
              "isJavaSetter" -> Data(false),
              "isJavaAccessor" -> Data(false),
              "isAccessor" -> Data(true),
              "scalaAccessorName" -> Data("name"),
              "javaAccessorName" -> Data("<no java accessor name>"),
              "accessorName" -> Data("name")
            )
          )
        }

        test("case class with var constructor parameter: getter is a case field") {
          testMethodProperties[examples.methods.CaseClassWithVarCtorParam]("name") <==> Data.map(
            "name" -> Data.map(
              "invocation" -> Data("OnInstance"),
              "hasTypeParameters" -> Data(false),
              "position" -> methodsPosition(113, 32),
              "annotations" -> Data.list(),
              "isConstructor" -> Data(false),
              "isVal" -> Data(false),
              "isVar" -> Data(true),
              "isLazy" -> Data(false),
              "isDef" -> Data(false),
              "isFinal" -> Data(false),
              "isAbstract" -> Data(false),
              "isOverride" -> Data(false),
              "isImplicit" -> Data(false),
              "isDeclared" -> Data(true),
              "isSynthetic" -> Data(false),
              "isInherited" -> Data(false),
              "isAvailable(Everywhere)" -> Data(true),
              "isAvailable(AtCallSite)" -> Data(true),
              "arity" -> Data(0),
              "isNullary" -> Data(true),
              "isUnary" -> Data(false),
              "isBinary" -> Data(false),
              "isConstructorArgument" -> Data(true),
              "isCaseField" -> Data(true),
              "isScalaGetter" -> Data(true),
              "isScalaSetter" -> Data(false),
              "isScalaAccessor" -> Data(true),
              "isJavaGetter" -> Data(false),
              "isJavaSetter" -> Data(false),
              "isJavaAccessor" -> Data(false),
              "isAccessor" -> Data(true),
              "scalaAccessorName" -> Data("name"),
              "javaAccessorName" -> Data("<no java accessor name>"),
              "accessorName" -> Data("name")
            )
          )
        }

        test("case class with var constructor parameter: setter is not a case field") {
          testMethodProperties[examples.methods.CaseClassWithVarCtorParam]("name_=") <==> Data.map(
            "name_=(String)" -> Data.map(
              "invocation" -> Data("OnInstance"),
              "hasTypeParameters" -> Data(false),
              "position" -> methodsPosition(113, 32),
              "annotations" -> Data.list(),
              "isConstructor" -> Data(false),
              "isVal" -> Data(false),
              "isVar" -> Data(true),
              "isLazy" -> Data(false),
              "isDef" -> Data(true),
              "isFinal" -> Data(false),
              "isAbstract" -> Data(false),
              "isOverride" -> Data(false),
              "isImplicit" -> Data(false),
              "isDeclared" -> Data(true),
              "isSynthetic" -> Data(false),
              "isInherited" -> Data(false),
              "isAvailable(Everywhere)" -> Data(true),
              "isAvailable(AtCallSite)" -> Data(true),
              "arity" -> Data(1),
              "isNullary" -> Data(false),
              "isUnary" -> Data(true),
              "isBinary" -> Data(false),
              "isConstructorArgument" -> Data(false),
              "isCaseField" -> Data(false),
              "isScalaGetter" -> Data(false),
              "isScalaSetter" -> Data(true),
              "isScalaAccessor" -> Data(true),
              "isJavaGetter" -> Data(false),
              "isJavaSetter" -> Data(false),
              "isJavaAccessor" -> Data(false),
              "isAccessor" -> Data(true),
              "scalaAccessorName" -> Data("name"),
              "javaAccessorName" -> Data("<no java accessor name>"),
              "accessorName" -> Data("name")
            )
          )
        }
      }

      // Visibility declared on a var (private[examples]) must propagate from the getter/val to the synthesized setter
      // (counter_=), on both platforms.
      group("regression: setter inherits the visibility of its var") {
        import MethodsFixtures.{testMethodProperties, testMethodVisibility}

        // scala.collection.immutable.:: declares `private[scala] var next` — for the precompiled stdlib class on
        // Scala 3 the synthesized setter symbol does not carry the visibility flags itself, so they must be looked up
        // on the field.
        test("private[scala] var next of scala.collection.immutable.:: restricts the setter too") {
          testMethodVisibility[scala.collection.immutable.::[Int]]("next") <==> Data.map(
            "next" -> Data.map(
              "isAvailable(Everywhere)" -> Data(false),
              "isAvailable(AtCallSite)" -> Data(false)
            )
          )
          testMethodVisibility[scala.collection.immutable.::[Int]]("next_=") <==> Data.map(
            "next_=" -> Data.map(
              "isAvailable(Everywhere)" -> Data(false),
              "isAvailable(AtCallSite)" -> Data(false)
            )
          )
        }

        test("restricted var getter is not available outside its scope") {
          testMethodProperties[examples.methods.WithRestrictedVar]("counter") <==> Data.map(
            "counter" -> Data.map(
              "invocation" -> Data("OnInstance"),
              "hasTypeParameters" -> Data(false),
              "position" -> methodsPosition(116, 24),
              "annotations" -> Data.list(),
              "isConstructor" -> Data(false),
              "isVal" -> Data(false),
              "isVar" -> Data(true),
              "isLazy" -> Data(false),
              "isDef" -> Data(false),
              "isFinal" -> Data(false),
              "isAbstract" -> Data(false),
              "isOverride" -> Data(false),
              "isImplicit" -> Data(false),
              "isDeclared" -> Data(true),
              "isSynthetic" -> Data(false),
              "isInherited" -> Data(false),
              "isAvailable(Everywhere)" -> Data(false),
              "isAvailable(AtCallSite)" -> Data(false),
              "arity" -> Data(0),
              "isNullary" -> Data(true),
              "isUnary" -> Data(false),
              "isBinary" -> Data(false),
              "isConstructorArgument" -> Data(false),
              "isCaseField" -> Data(false),
              "isScalaGetter" -> Data(true),
              "isScalaSetter" -> Data(false),
              "isScalaAccessor" -> Data(true),
              "isJavaGetter" -> Data(false),
              "isJavaSetter" -> Data(false),
              "isJavaAccessor" -> Data(false),
              "isAccessor" -> Data(true),
              "scalaAccessorName" -> Data("counter"),
              "javaAccessorName" -> Data("<no java accessor name>"),
              "accessorName" -> Data("counter")
            )
          )
        }

        test("restricted var setter is not available outside its scope either") {
          testMethodProperties[examples.methods.WithRestrictedVar]("counter_=") <==> Data.map(
            "counter_=(Int)" -> Data.map(
              "invocation" -> Data("OnInstance"),
              "hasTypeParameters" -> Data(false),
              "position" -> methodsPosition(116, 24),
              "annotations" -> Data.list(),
              "isConstructor" -> Data(false),
              "isVal" -> Data(false),
              "isVar" -> Data(true),
              "isLazy" -> Data(false),
              "isDef" -> Data(true),
              "isFinal" -> Data(false),
              "isAbstract" -> Data(false),
              "isOverride" -> Data(false),
              "isImplicit" -> Data(false),
              "isDeclared" -> Data(true),
              "isSynthetic" -> Data(false),
              "isInherited" -> Data(false),
              "isAvailable(Everywhere)" -> Data(false),
              "isAvailable(AtCallSite)" -> Data(false),
              "arity" -> Data(1),
              "isNullary" -> Data(false),
              "isUnary" -> Data(true),
              "isBinary" -> Data(false),
              "isConstructorArgument" -> Data(false),
              "isCaseField" -> Data(false),
              "isScalaGetter" -> Data(false),
              "isScalaSetter" -> Data(true),
              "isScalaAccessor" -> Data(true),
              "isJavaGetter" -> Data(false),
              "isJavaSetter" -> Data(false),
              "isJavaAccessor" -> Data(false),
              "isAccessor" -> Data(true),
              "scalaAccessorName" -> Data("counter"),
              "javaAccessorName" -> Data("<no java accessor name>"),
              "accessorName" -> Data("counter")
            )
          )
        }
      }

      group("calling methods, return Expr with a called method result") {
        import MethodsFixtures.testCallNoInstanceIntMethod
        import MethodsFixtures.testCallInstanceIntMethod

        test("for no-instance methods (companion methods see for companion class)") {
          testCallNoInstanceIntMethod[examples.methods.WithCompanion]("call")(
            1,
            2
          ) ==> examples.methods.WithCompanion.call(1, 2)
        }

        test("for instance methods") {
          testCallInstanceIntMethod[examples.methods.WithCompanion](new examples.methods.WithCompanion(1))("method")(
            2
          ) ==> (new examples.methods.WithCompanion(1)).method(2)
        }

        test("for companion methods (for companion object directly)") {
          testCallInstanceIntMethod[examples.methods.WithCompanion.type](examples.methods.WithCompanion)("call")(
            1,
            2
          ) ==> examples.methods.WithCompanion.call(1, 2)
        }

        test("for methods with default values") {
          testCallInstanceIntMethod[examples.methods.NoCompanionClass](new examples.methods.NoCompanionClass)(
            "methodWithDefault"
          )(
            1
          ) ==> (new examples.methods.NoCompanionClass).methodWithDefault()
        }
      }

      group("default value splicing") {
        import MethodsFixtures.testConstructWithDefaults
        import MethodsFixtures.testCallNoInstanceMethodWithDefaults
        import MethodsFixtures.testCallInstanceMethodWithDefaults

        test("constructor with default values") {
          testConstructWithDefaults[examples.methods.WithConstructorDefaults](5) <==> Data(
            "WithConstructorDefaults(5, 10)"
          )
        }

        test("case class constructor with default values") {
          testConstructWithDefaults[examples.methods.CaseWithDefaults](5) <==> Data(
            "CaseWithDefaults(5,20)"
          )
        }

        test("companion apply with default values") {
          testCallNoInstanceMethodWithDefaults[examples.methods.WithCompanionDefaults]("apply")(5) <==> Data(
            "WithCompanionDefaults(5, 15)"
          )
        }

        test("instance method with default values (no args provided)") {
          testCallInstanceMethodWithDefaults[examples.methods.NoCompanionClass](
            new examples.methods.NoCompanionClass
          )("methodWithDefault")() <==> Data(
            (new examples.methods.NoCompanionClass).methodWithDefault().toString
          )
        }

        test("case class instance method with default values") {
          testCallInstanceMethodWithDefaults[examples.methods.CaseWithDefaults](
            examples.methods.CaseWithDefaults(5, 20)
          )("methodWithDefault")() <==> Data(
            examples.methods.CaseWithDefaults(5, 20).methodWithDefault().toString
          )
        }
      }
    }

    group("method ordering") {

      test("constructor arguments should appear before declared methods") {
        import MethodsFixtures.testMethodOrdering

        val result = testMethodOrdering[examples.methods.WithCompanion]
        val names = result.asList.get.map(_.asString.get)
        // "arg" is the constructor argument, "method" is declared
        val argIdx = names.indexOf("arg")
        val methodIdx = names.indexOf("method")
        assert(argIdx >= 0, s"Constructor arg 'arg' should be in methods: $names")
        assert(methodIdx >= 0, s"Declared method 'method' should be in methods: $names")
        assert(
          argIdx < methodIdx,
          s"Constructor arg 'arg' (idx=$argIdx) should appear before 'method' (idx=$methodIdx): $names"
        )
      }

      test("multiple constructor arguments should preserve declaration order") {
        import MethodsFixtures.testMethodOrdering

        val result = testMethodOrdering[examples.methods.ScopeVisibility]
        val names = result.asList.get.map(_.asString.get)
        val privateCtorIdx = names.indexOf("privateCtorArg")
        val publicCtorIdx = names.indexOf("publicCtorArg")
        assert(privateCtorIdx >= 0, s"'privateCtorArg' should be in methods: $names")
        assert(publicCtorIdx >= 0, s"'publicCtorArg' should be in methods: $names")
        assert(
          privateCtorIdx < publicCtorIdx,
          s"'privateCtorArg' (idx=$privateCtorIdx) should appear before 'publicCtorArg' (idx=$publicCtorIdx): $names"
        )
        val publicMethodIdx = names.indexOf("publicMethod")
        assert(
          publicCtorIdx < publicMethodIdx,
          s"Constructor args should appear before declared methods: $names"
        )
      }
    }
  }

  group("expectations and builder chain") {

    group("nullary methods") {

      test("nullaryNoParamList") {
        MethodsFixtures.testMethodExpectations[examples.methods.NullaryMethods]("nullaryNoParamList") <==>
          Data.list(
            Data.map(
              "name" -> Data("nullaryNoParamList"),
              "expectations" -> Data.list(Data("NeedsInstance")),
              "knownReturning" -> Data("scala.Int"),
              "toString" -> Data("hearth.examples.methods.NullaryMethods: def nullaryNoParamList: scala.Int")
            )
          )
      }

      test("nullaryEmptyParamList") {
        MethodsFixtures.testMethodExpectations[examples.methods.NullaryMethods]("nullaryEmptyParamList") <==>
          Data.list(
            Data.map(
              "name" -> Data("nullaryEmptyParamList"),
              "expectations" -> Data.list(Data("NeedsInstance"), Data("NeedsValues()")),
              "knownReturning" -> Data("scala.Int"),
              "toString" -> Data("hearth.examples.methods.NullaryMethods: def nullaryEmptyParamList(): scala.Int")
            )
          )
      }
    }

    group("non-nullary methods") {

      test("singleParamList") {
        MethodsFixtures.testMethodExpectations[examples.methods.MultiParamListMethods]("singleParamList") <==>
          Data.list(
            Data.map(
              "name" -> Data("singleParamList"),
              "expectations" -> Data.list(
                Data("NeedsInstance"),
                Data("NeedsValues(arg1: scala.Int, arg2: java.lang.String)")
              ),
              "knownReturning" -> Data("scala.Boolean"),
              "toString" -> Data(
                "hearth.examples.methods.MultiParamListMethods: def singleParamList(arg1: scala.Int, arg2: java.lang.String): scala.Boolean"
              )
            )
          )
      }

      test("multiParamList") {
        MethodsFixtures.testMethodExpectations[examples.methods.MultiParamListMethods]("multiParamList") <==>
          Data.list(
            Data.map(
              "name" -> Data("multiParamList"),
              "expectations" -> Data.list(
                Data("NeedsInstance"),
                Data("NeedsValues(arg1: scala.Int)(arg2: java.lang.String)")
              ),
              "knownReturning" -> Data("scala.Boolean"),
              "toString" -> Data(
                "hearth.examples.methods.MultiParamListMethods: def multiParamList(arg1: scala.Int)(arg2: java.lang.String): scala.Boolean"
              )
            )
          )
      }
    }

    group("type-parametric methods") {

      test("parametric1") {
        MethodsFixtures.testMethodExpectations[examples.methods.Parametric]("parametric1") <==>
          Data.list(
            Data.map(
              "name" -> Data("parametric1"),
              "expectations" -> Data.list(
                Data("NeedsInstance"),
                Data("NeedsTypes[A]"),
                Data("NeedsValues(a: A)")
              ),
              "knownReturning" -> Data("<none>"),
              "toString" -> Data(
                "hearth.examples.methods.Parametric: def parametric1[A]" +
                  "(a: " + "A" +
                  "): scala.collection.immutable.List[" +
                  "A" + "]"
              )
            )
          )
      }
    }

    group("generic class methods") {

      test("method on GenericClass[Int, String] has resolved parameter types") {
        MethodsFixtures.testMethodExpectations[examples.methods.GenericClass[Int, String]]("method") <==>
          Data.list(
            Data.map(
              "name" -> Data("method"),
              "expectations" -> Data.list(
                Data("NeedsInstance"),
                Data("NeedsValues(x: scala.Int, y: java.lang.String)")
              ),
              "knownReturning" -> Data("java.lang.String"),
              "toString" -> Data(
                "hearth.examples.methods.GenericClass[scala.Int, java.lang.String]: def method(x: scala.Int, y: java.lang.String): java.lang.String"
              )
            )
          )
      }
    }

    group("constructors") {

      test("SimpleConstructor") {
        MethodsFixtures.testConstructorExpectations[examples.methods.SimpleConstructor] <==>
          Data.list(
            Data.map(
              "expectations" -> Data.list(Data("NeedsValues(a: scala.Int, b: java.lang.String)")),
              "knownReturning" -> Data("hearth.examples.methods.SimpleConstructor"),
              "toString" -> Data("new hearth.examples.methods.SimpleConstructor(a: scala.Int, b: java.lang.String)")
            )
          )
      }
    }

    group("path-dependent return type") {

      test("pathDepResult with resolved type alias") {
        val innerTypeName = "hearth.examples.methods.PathDepReturn#Inner"
        MethodsFixtures.testMethodExpectations[examples.methods.PathDepReturn]("pathDepResult") <==>
          Data.list(
            Data.map(
              "name" -> Data("pathDepResult"),
              "expectations" -> Data.list(
                Data("NeedsInstance"),
                Data(s"NeedsValues(arg: $innerTypeName)")
              ),
              "knownReturning" -> Data("java.lang.String"),
              "toString" -> Data(
                s"hearth.examples.methods.PathDepReturn: def pathDepResult(arg: $innerTypeName): java.lang.String"
              )
            )
          )
      }
    }

    group("path-dependent arguments") {

      test("pathDep1(arg)(arg2: arg.Inner) - resolved type alias") {
        val wrapperType = "hearth.examples.methods.PathDepArgs#Wrapper"
        MethodsFixtures.testMethodExpectations[examples.methods.PathDepArgs]("pathDep1") <==>
          Data.list(
            Data.map(
              "name" -> Data("pathDep1"),
              "expectations" -> Data.list(
                Data("NeedsInstance"),
                Data(s"NeedsValues(arg: $wrapperType)(arg2: scala.Int)")
              ),
              "knownReturning" -> Data("java.lang.String"),
              "toString" -> Data(
                s"hearth.examples.methods.PathDepArgs: def pathDep1(arg: $wrapperType)(arg2: scala.Int): java.lang.String"
              )
            )
          )
      }

      test("pathDep2(arg)(arg2)(arg3) - all params in one NeedsValues") {
        val wrapperType = "hearth.examples.methods.PathDepArgs#Wrapper"
        MethodsFixtures.testMethodExpectations[examples.methods.PathDepArgs]("pathDep2") <==>
          Data.list(
            Data.map(
              "name" -> Data("pathDep2"),
              "expectations" -> Data.list(
                Data("NeedsInstance"),
                Data(s"NeedsValues(arg: $wrapperType)(arg2: scala.Int)(arg3: java.lang.String)")
              ),
              "knownReturning" -> Data("scala.Boolean"),
              "toString" -> Data(
                s"hearth.examples.methods.PathDepArgs: def pathDep2(arg: $wrapperType)(arg2: scala.Int)(arg3: java.lang.String): scala.Boolean"
              )
            )
          )
      }

      test("pathDep3(arg)(arg2)(arg3)(arg4) - four param lists") {
        val w1Type =
          "hearth.examples.methods.PathDepArgs2#W1"
        val w2Type =
          "hearth.examples.methods.PathDepArgs2#W2"
        MethodsFixtures.testMethodExpectations[examples.methods.PathDepArgs2]("pathDep3") <==>
          Data.list(
            Data.map(
              "name" -> Data("pathDep3"),
              "expectations" -> Data.list(
                Data("NeedsInstance"),
                Data(s"NeedsValues(arg: $w1Type)(arg2: scala.Int)(arg3: $w2Type)(arg4: java.lang.String)")
              ),
              "knownReturning" -> Data("scala.Boolean"),
              "toString" -> Data(
                s"hearth.examples.methods.PathDepArgs2: def pathDep3(arg: $w1Type)(arg2: scala.Int)(arg3: $w2Type)(arg4: java.lang.String): scala.Boolean"
              )
            )
          )
      }
    }

    group("generic constructors") {

      test("GenericCtor[Int] constructor") {
        MethodsFixtures.testConstructorExpectations[examples.methods.GenericCtor[Int]] <==>
          Data.list(
            Data.map(
              "expectations" -> Data.list(Data("NeedsValues(value: scala.Int)")),
              "knownReturning" -> Data("hearth.examples.methods.GenericCtor[scala.Int]"),
              "toString" -> Data("new hearth.examples.methods.GenericCtor[scala.Int](value: scala.Int)")
            )
          )
      }
    }

    group("more type-parametric methods") {

      test("parametric2[A](a: A): A - type param in return type means knownReturning is None") {
        MethodsFixtures.testMethodExpectations[examples.methods.Parametric]("parametric2") <==>
          Data.list(
            Data.map(
              "name" -> Data("parametric2"),
              "expectations" -> Data.list(Data("NeedsInstance"), Data("NeedsTypes[A]"), Data("NeedsValues(a: A)")),
              "knownReturning" -> Data("<none>"),
              "toString" -> Data(
                "hearth.examples.methods.Parametric: def parametric2[A]" +
                  "(a: " + "A" +
                  "): " + "A"
              )
            )
          )
      }

      test("parametricBounded[A <: Comparable[A]]") {
        MethodsFixtures.testMethodExpectations[examples.methods.Parametric]("parametricBounded") <==>
          Data.list(
            Data.map(
              "name" -> Data("parametricBounded"),
              "expectations" -> Data.list(
                Data("NeedsInstance"),
                Data("NeedsTypes[A <: java.lang.Comparable[A]]"),
                Data("NeedsValues(a: A)")
              ),
              "knownReturning" -> Data("<none>"),
              "toString" -> Data(
                "hearth.examples.methods.Parametric: def parametricBounded[A <: java.lang.Comparable[A]]" +
                  "(a: " + "A" +
                  "): " + "A"
              )
            )
          )
      }
    }

    group("default constructors") {

      test("DefaultConstructor with default values") {
        MethodsFixtures.testConstructorExpectations[examples.methods.DefaultConstructor] <==>
          Data.list(
            Data.map(
              "expectations" -> Data.list(Data("NeedsValues(a: scala.Int, b: java.lang.String)")),
              "knownReturning" -> Data("hearth.examples.methods.DefaultConstructor"),
              "toString" -> Data(
                "new hearth.examples.methods.DefaultConstructor(a: scala.Int = <default>, b: java.lang.String = <default>)"
              )
            )
          )
      }
    }

    group("nullary instance methods on generic class") {

      test("swap on GenericClass[Int, String] returns GenericClass[String, Int]") {
        MethodsFixtures.testMethodExpectations[examples.methods.GenericClass[Int, String]]("swap") <==>
          Data.list(
            Data.map(
              "name" -> Data("swap"),
              "expectations" -> Data.list(Data("NeedsInstance")),
              "knownReturning" -> Data("hearth.examples.methods.GenericClass[java.lang.String, scala.Int]"),
              "toString" -> Data(
                "hearth.examples.methods.GenericClass[scala.Int, java.lang.String]: def swap: hearth.examples.methods.GenericClass[java.lang.String, scala.Int]"
              )
            )
          )
      }
    }

    group("higher-kinded type params") {

      test("higherKinded[F[_]](a: F[String]): F[String]") {
        MethodsFixtures.testMethodExpectations[examples.methods.HigherKinded]("higherKinded") <==>
          Data.list(
            Data.map(
              "name" -> Data("higherKinded"),
              "expectations" -> Data.list(
                Data("NeedsInstance"),
                Data("NeedsTypes[F]"),
                Data("NeedsValues(a: F[java.lang.String])")
              ),
              "knownReturning" -> Data("<none>"),
              "toString" -> Data(
                "hearth.examples.methods.HigherKinded: def higherKinded[F](a: F[java.lang.String]): F[java.lang.String]"
              )
            )
          )
      }
    }

    group("implicit params") {

      test("withImplicit(a: Int)(implicit b: String): String") {
        val knownRet = "java.lang.String"
        MethodsFixtures.testMethodExpectations[examples.methods.WithImplicitParam]("withImplicit") <==>
          Data.list(
            Data.map(
              "name" -> Data("withImplicit"),
              "expectations" -> Data.list(
                Data("NeedsInstance"),
                Data("NeedsValues(a: scala.Int)(b: java.lang.String)")
              ),
              "knownReturning" -> Data(knownRet),
              "toString" -> Data(
                "hearth.examples.methods.WithImplicitParam: def withImplicit(a: scala.Int)(b: java.lang.String): java.lang.String"
              )
            )
          )
      }

      // An implicit clause that FOLLOWS a type-parameter clause must not be dropped (`NeedsValues()`), or there is no
      // way to supply the `Sync[F]`. It is surfaced with the method's type parameter abstract.
      test("resource[F[_]](implicit ev: Sync[F]): F[Int] — implicit clause after a type-param clause is kept") {
        MethodsFixtures.testMethodExpectations[examples.methods.HigherKindedImplicit]("resource") <==>
          Data.list(
            Data.map(
              "name" -> Data("resource"),
              "expectations" -> Data.list(
                Data("NeedsInstance"),
                Data("NeedsTypes[F]"),
                Data("NeedsValues(ev: hearth.examples.methods.Sync[F])")
              ),
              "knownReturning" -> Data("<none>"),
              "toString" -> Data(
                "hearth.examples.methods.HigherKindedImplicit: def resource[F](ev: hearth.examples.methods.Sync[F]): F[scala.Int]"
              )
            )
          )
      }

      test(
        "make[F[_]](config: String)(implicit ev: Sync[F]): F[Int] — value + implicit clause after a type-param clause"
      ) {
        MethodsFixtures.testMethodExpectations[examples.methods.HigherKindedImplicit]("make") <==>
          Data.list(
            Data.map(
              "name" -> Data("make"),
              "expectations" -> Data.list(
                Data("NeedsInstance"),
                Data("NeedsTypes[F]"),
                Data("NeedsValues(config: java.lang.String)(ev: hearth.examples.methods.Sync[F])")
              ),
              "knownReturning" -> Data("<none>"),
              "toString" -> Data(
                "hearth.examples.methods.HigherKindedImplicit: def make[F](config: java.lang.String)(ev: hearth.examples.methods.Sync[F]): F[scala.Int]"
              )
            )
          )
      }
    }

    group("method calling via fold") {

      // Applying `T := Int` in `onTypes` substitutes into the value/implicit clauses that follow.
      test("fold substitutes applied type arguments into a following value/implicit clause") {
        MethodsFixtures.testFoldSubstitutesTypeArgs <==> Data.map(
          "params" -> Data.list(
            Data("t: scala.Int"),
            Data("ord: scala.math.Ordering[scala.Int]")
          ),
          "result" -> Data("built")
        )
      }

      test("nullary — nullaryNoParamList returns 42") {
        MethodsFixtures.testCallInstanceViaFold(new examples.methods.NullaryMethods)("nullaryNoParamList")() <==>
          Data("42")
      }

      test("nullary — nullaryEmptyParamList() returns 42") {
        MethodsFixtures.testCallInstanceViaFold(new examples.methods.NullaryMethods)("nullaryEmptyParamList")() <==>
          Data("42")
      }

      test("singleParamList(1, test) returns true") {
        MethodsFixtures.testCallInstanceViaFold(new examples.methods.MultiParamListMethods)("singleParamList")(1) <==>
          Data("true")
      }

      test("multiParamList(1)(test) returns true") {
        MethodsFixtures.testCallInstanceViaFold(new examples.methods.MultiParamListMethods)("multiParamList")(1) <==>
          Data("true")
      }

      test("construct SimpleConstructor(1, test) via fold") {
        val result = MethodsFixtures.testCallConstructorViaFold[examples.methods.SimpleConstructor](1)
        val str = result.asString.get
        assert(!str.startsWith("FAILED"), s"Constructor should succeed: $str")
        assert(str.contains("SimpleConstructor"), s"Result should be a SimpleConstructor: $str")
      }

      test("construct PhantomParams through a type alias with mismatched type parameters via fold") {
        MethodsFixtures.testCallConstructorViaFold[examples.methods.PhantomParams.Partial[String]](1) <==>
          Data("PhantomParams(1, test)")
        MethodsFixtures.testCallConstructorViaFold[examples.methods.PhantomParams.Fixed](1) <==>
          Data("PhantomParams(1, test)")
        MethodsFixtures.testCallConstructorViaFold[examples.methods.PhantomParams.PartialOfPartial[String]](1) <==>
          Data("PhantomParams(1, test)")
        MethodsFixtures.testCallConstructorViaFold[examples.methods.PhantomParams.FixedThroughChain](1) <==>
          Data("PhantomParams(1, test)")
      }

      test("construct DefaultConstructor(1) with defaults via fold") {
        val result = MethodsFixtures.testCallConstructorViaFold[examples.methods.DefaultConstructor](1)
        val str = result.asString.get
        assert(!str.startsWith("FAILED"), s"Constructor should succeed: $str")
        assert(str.contains("DefaultConstructor"), s"Result should be a DefaultConstructor: $str")
      }

      test("construct GenericCtor[Int](42) via fold") {
        val result = MethodsFixtures.testCallConstructorViaFold[examples.methods.GenericCtor[Int]](42)
        val str = result.asString.get
        assert(!str.startsWith("FAILED"), s"Constructor should succeed: $str")
        assert(str.contains("GenericCtor"), s"Result should be a GenericCtor: $str")
      }
    }

    group("method calling via foldF") {

      test("nullary — nullaryNoParamList returns 42") {
        MethodsFixtures.testCallInstanceViaFoldF(new examples.methods.NullaryMethods)("nullaryNoParamList")() <==>
          Data("42")
      }

      test("singleParamList(1, test) returns true") {
        MethodsFixtures.testCallInstanceViaFoldF(new examples.methods.MultiParamListMethods)("singleParamList")(
          1
        ) <==> Data("true")
      }

      test("multiParamList(1)(test) returns true") {
        MethodsFixtures.testCallInstanceViaFoldF(new examples.methods.MultiParamListMethods)("multiParamList")(
          1
        ) <==> Data("true")
      }

      test("missing required Int argument short-circuits the whole foldF to None") {
        MethodsFixtures.testCallInstanceViaFoldF(new examples.methods.MultiParamListMethods)(
          "singleParamList"
        )() <==> Data("<not applicable>")
      }

      test("wrong application — providing no arguments at all fails with a requirement error") {
        MethodsFixtures.testCallInstanceViaFoldMissingArgs(new examples.methods.MultiParamListMethods)(
          "singleParamList"
        ) <==> Data(
          "REQUIREMENT FAILED: Expected that hearth.examples.methods.MultiParamListMethods's singleParamList parameter `arg1` would be provided or have a default value.\n" +
            "Ensure that all arguments are provided or have a default value."
        )
      }
    }

    group("prettyPrint (ANSI coloring)") {

      test("prettyPrint stripped of ANSI equals plainPrint — nullary") {
        val data =
          MethodsFixtures.testMethodPrettyPrint[examples.methods.NullaryMethods]("nullaryNoParamList")
        val map = data.asList.get.head.asMap.get
        val plain = map("plainPrint").asString.get
        val stripped = map("prettyPrintStripped").asString.get
        assert(stripped == plain, s"stripped: $stripped\nplain:    $plain")
      }

      test("prettyPrint stripped of ANSI equals plainPrint — multi param list") {
        val data =
          MethodsFixtures.testMethodPrettyPrint[examples.methods.MultiParamListMethods]("multiParamList")
        val map = data.asList.get.head.asMap.get
        val plain = map("plainPrint").asString.get
        val stripped = map("prettyPrintStripped").asString.get
        assert(stripped == plain, s"stripped: $stripped\nplain:    $plain")
      }

      test("prettyPrint stripped of ANSI equals plainPrint — type parametric with bounds") {
        val data =
          MethodsFixtures.testMethodPrettyPrint[examples.methods.Parametric]("parametricBounded")
        val map = data.asList.get.head.asMap.get
        val plain = map("plainPrint").asString.get
        val stripped = map("prettyPrintStripped").asString.get
        assert(stripped == plain, s"stripped: $stripped\nplain:    $plain")
      }

      test("prettyPrint stripped of ANSI equals plainPrint — higher kinded") {
        val data =
          MethodsFixtures.testMethodPrettyPrint[examples.methods.HigherKinded]("higherKinded")
        val map = data.asList.get.head.asMap.get
        val plain = map("plainPrint").asString.get
        val stripped = map("prettyPrintStripped").asString.get
        assert(stripped == plain, s"stripped: $stripped\nplain:    $plain")
      }

      test("prettyPrint contains ANSI escape codes") {
        val data =
          MethodsFixtures.testMethodPrettyPrint[examples.methods.NullaryMethods]("nullaryNoParamList")
        val map = data.asList.get.head.asMap.get
        val prettyPrint = map("prettyPrint").asString.get
        val plainPrint = map("plainPrint").asString.get
        assert(prettyPrint != plainPrint, "prettyPrint should contain ANSI codes and differ from plainPrint")
      }
    }

    // Method.toString renders the *applied state* once the builder chain has been partially consumed: the bare
    // signature when nothing has been applied, "(on <instance>)" after an instance step, and "(applied (...))"
    // after value arguments. These branches are not exercised by the expectations tests (which toString an
    // un-applied method).
    group("toString of a partially-applied builder chain") {

      test("instance method: signature, then 'on <instance>', then 'applied (...)'") {
        val map = MethodsFixtures
          .testMethodAppliedToString(new examples.methods.MultiParamListMethods)("singleParamList")
          .asMap
          .get
        val before = map("beforeApplication").asString.get
        val afterInstance = map("afterInstance").asString.get
        val afterValues = map("afterInstanceAndValues").asString.get

        // Un-applied: bare signature, no applied-state suffix.
        before ==>
          "hearth.examples.methods.MultiParamListMethods: def singleParamList(arg1: scala.Int, arg2: java.lang.String): scala.Boolean"
        // After supplying the instance the chain records an Instance step rendered as "on ...".
        assert(
          afterInstance.contains("(on ") && afterInstance.startsWith(before),
          s"afterInstance should keep the signature and append the instance step, got: $afterInstance"
        )
        // After supplying value arguments the chain records a Values step rendered as "applied (...)".
        assert(
          afterValues.contains("applied (") && afterValues.contains("arg1 = ") && afterValues.contains("arg2 = "),
          s"afterValues should render the applied value arguments, got: $afterValues"
        )
      }

      test("constructor: signature, then 'applied (...)' for the value arguments") {
        val map = MethodsFixtures.testConstructorAppliedToString[examples.methods.SimpleConstructor].asMap.get
        val before = map("beforeApplication").asString.get
        val afterValues = map("afterValues").asString.get

        before ==>
          "new hearth.examples.methods.SimpleConstructor(a: scala.Int, b: java.lang.String)"
        assert(
          afterValues.contains("applied (") && afterValues.contains("a = ") && afterValues.contains("b = "),
          s"afterValues should render the applied constructor arguments, got: $afterValues"
        )
      }
    }
  }

  group("methods: annotation type matching and destructuring") {
    import MethodsFixtures.testAnnotationDestructuring

    test("type annotation: ExampleAnnotation2(1) destructured to extract constructor arg") {
      val result = testAnnotationDestructuring[examples.methods.NoCompanionClass]
      val list = result.asList.get
      val typeAnns = list.filter(_.asMap.get("source").asString.get == "type")
      val ann2 = typeAnns.find(_.asMap.get("annotationType").asString.get == "ExampleAnnotation2")
      assert(ann2.isDefined, s"should find ExampleAnnotation2 on type, got: $typeAnns")
      val args = ann2.get.asMap.get("destructuredArgs").asString.get
      assert(args == "1", s"should destructure constructor arg to '1', got: '$args'")
    }

    test("method annotation: ExampleAnnotation() destructured as no-arg constructor") {
      val result = testAnnotationDestructuring[examples.methods.NoCompanionClass]
      val list = result.asList.get
      val methodAnns = list.filter(_.asMap.get("source").asString.get.startsWith("method:"))
      val annOnMethod = methodAnns.find(_.asMap.get("source").asString.get == "method:methodWithAnnotation")
      assert(annOnMethod.isDefined, "should find annotation on methodWithAnnotation")
      assert(annOnMethod.get.asMap.get("annotationType").asString.get == "ExampleAnnotation")
      val args = annOnMethod.get.asMap.get("destructuredArgs").asString.get
      assert(args == "", s"no-arg annotation should destructure to empty args, got: '$args'")
    }

    test("type-position annotations on case-class field types are exposed") {
      import MethodsFixtures.testTypeAnnotations

      testTypeAnnotations[examples.methods.WithTypeAnnotations] <==> Data.map(
        "own" -> Data.list(),
        "fields" -> Data.list(
          Data.map("name" -> Data("name"), "typeAnnotations" -> Data.list(Data("ExampleAnnotation"))),
          Data.map("name" -> Data("age"), "typeAnnotations" -> Data.list()),
          Data.map(
            "name" -> Data("nickname"),
            "typeAnnotations" -> Data.list(Data("ExampleAnnotation"), Data("ExampleAnnotation2"))
          )
        )
      )
    }

    test("type-position annotations survive the caseFields / knownReturning path") {
      import MethodsFixtures.testTypeAnnotationsViaKnownReturning

      testTypeAnnotationsViaKnownReturning[examples.methods.WithTypeAnnotations] <==> Data.list(
        Data.map("name" -> Data("name"), "typeAnnotations" -> Data.list(Data("ExampleAnnotation"))),
        Data.map("name" -> Data("age"), "typeAnnotations" -> Data.list()),
        Data.map(
          "name" -> Data("nickname"),
          "typeAnnotations" -> Data.list(Data("ExampleAnnotation"), Data("ExampleAnnotation2"))
        )
      )
    }

  }

  group("methods: parameter annotations") {
    import MethodsFixtures.{testConstructorParameterAnnotations, testParameterAnnotations}

    test("method parameter annotations are typed and destructurable") {
      testParameterAnnotations[examples.methods.NoCompanionClass]("methodWithAnnotatedParam") <==> Data.list(
        Data.map(
          "name" -> Data("arg"),
          "annotations" -> Data.list(
            Data.map(
              "isExampleAnnotation" -> Data(true),
              "isExampleAnnotation2" -> Data(false),
              "destructuredArgs" -> Data("")
            )
          )
        )
      )
    }

    test("case class constructor parameter annotations are typed and destructurable") {
      testConstructorParameterAnnotations[examples.methods.WithAnnotatedParams] <==> Data.list(
        Data.map(
          "name" -> Data("a"),
          "annotations" -> Data.list(
            Data.map(
              "isExampleAnnotation" -> Data(true),
              "isExampleAnnotation2" -> Data(false),
              "destructuredArgs" -> Data("")
            )
          )
        ),
        Data.map(
          "name" -> Data("b"),
          "annotations" -> Data.list(
            Data.map(
              "isExampleAnnotation" -> Data(false),
              "isExampleAnnotation2" -> Data(true),
              "destructuredArgs" -> Data("1")
            )
          )
        ),
        Data.map(
          "name" -> Data("c"),
          "annotations" -> Data.list()
        )
      )
    }
  }

  group("methods: annotationsOfType and constructorArguments") {
    import MethodsFixtures.{testAnnotationsOfType, testAnnotationValueDecoded, testFieldNamesFromAnnotations}

    test("filters type and method annotations by type, decoding constructor args (present and absent cases)") {
      testAnnotationsOfType[examples.methods.NoCompanionClass]("methodWithAnnotation") <==> Data.map(
        "type" -> Data.map(
          "hasExampleAnnotation" -> Data(false),
          "hasExampleAnnotation2" -> Data(true),
          "exampleAnnotation2Args" -> Data.list(Data.list(Data("1"))),
          "parentAnnotations" -> Data(0),
          "childAnnotations" -> Data(0)
        ),
        "method" -> Data.map(
          "hasExampleAnnotation" -> Data(true),
          "hasExampleAnnotation2" -> Data(false),
          "exampleAnnotation2Args" -> Data.list(),
          "parentAnnotations" -> Data(0),
          "childAnnotations" -> Data(0),
          "parameters" -> Data.list(
            Data.map(
              "name" -> Data("arg"),
              "hasExampleAnnotation" -> Data(false),
              "exampleAnnotation2Args" -> Data.list(),
              "parentAnnotations" -> Data(0),
              "childAnnotations" -> Data(0)
            )
          )
        ),
        "constructor" -> Data.list()
      )
    }

    test("filters method parameter annotations by type") {
      testAnnotationsOfType[examples.methods.NoCompanionClass]("methodWithAnnotatedParam") <==> Data.map(
        "type" -> Data.map(
          "hasExampleAnnotation" -> Data(false),
          "hasExampleAnnotation2" -> Data(true),
          "exampleAnnotation2Args" -> Data.list(Data.list(Data("1"))),
          "parentAnnotations" -> Data(0),
          "childAnnotations" -> Data(0)
        ),
        "method" -> Data.map(
          "hasExampleAnnotation" -> Data(false),
          "hasExampleAnnotation2" -> Data(false),
          "exampleAnnotation2Args" -> Data.list(),
          "parentAnnotations" -> Data(0),
          "childAnnotations" -> Data(0),
          "parameters" -> Data.list(
            Data.map(
              "name" -> Data("arg"),
              "hasExampleAnnotation" -> Data(true),
              "exampleAnnotation2Args" -> Data.list(),
              "parentAnnotations" -> Data(0),
              "childAnnotations" -> Data(0)
            )
          )
        ),
        "constructor" -> Data.list()
      )
    }

    test("filters case class constructor parameter annotations by type, decoding constructor args") {
      testAnnotationsOfType[examples.methods.WithAnnotatedParams]("") <==> Data.map(
        "type" -> Data.map(
          "hasExampleAnnotation" -> Data(false),
          "hasExampleAnnotation2" -> Data(false),
          "exampleAnnotation2Args" -> Data.list(),
          "parentAnnotations" -> Data(0),
          "childAnnotations" -> Data(0)
        ),
        "method" -> Data("<no method requested>"),
        "constructor" -> Data.list(
          Data.map(
            "name" -> Data("a"),
            "hasExampleAnnotation" -> Data(true),
            "exampleAnnotation2Args" -> Data.list(),
            "parentAnnotations" -> Data(0),
            "childAnnotations" -> Data(0)
          ),
          Data.map(
            "name" -> Data("b"),
            "hasExampleAnnotation" -> Data(false),
            "exampleAnnotation2Args" -> Data.list(Data.list(Data("1"))),
            "parentAnnotations" -> Data(0),
            "childAnnotations" -> Data(0)
          ),
          Data.map(
            "name" -> Data("c"),
            "hasExampleAnnotation" -> Data(false),
            "exampleAnnotation2Args" -> Data.list(),
            "parentAnnotations" -> Data(0),
            "childAnnotations" -> Data(0)
          )
        )
      )
    }

    test("<:< filtering matches annotation subtypes by their base type (type, method and parameter)") {
      testAnnotationsOfType[examples.methods.WithChildAnnotation]("annotatedMethod") <==> Data.map(
        "type" -> Data.map(
          "hasExampleAnnotation" -> Data(false),
          "hasExampleAnnotation2" -> Data(false),
          "exampleAnnotation2Args" -> Data.list(),
          "parentAnnotations" -> Data(1),
          "childAnnotations" -> Data(1)
        ),
        "method" -> Data.map(
          "hasExampleAnnotation" -> Data(false),
          "hasExampleAnnotation2" -> Data(false),
          "exampleAnnotation2Args" -> Data.list(),
          "parentAnnotations" -> Data(1),
          "childAnnotations" -> Data(1),
          "parameters" -> Data.list(
            Data.map(
              "name" -> Data("arg"),
              "hasExampleAnnotation" -> Data(false),
              "exampleAnnotation2Args" -> Data.list(),
              "parentAnnotations" -> Data(1),
              "childAnnotations" -> Data(1)
            )
          )
        ),
        "constructor" -> Data.list()
      )
    }

    test("<:< filtering does not match a base annotation when its subtype is requested") {
      testAnnotationsOfType[examples.methods.WithChildAnnotation]("annotatedMethod2") <==> Data.map(
        "type" -> Data.map(
          "hasExampleAnnotation" -> Data(false),
          "hasExampleAnnotation2" -> Data(false),
          "exampleAnnotation2Args" -> Data.list(),
          "parentAnnotations" -> Data(1),
          "childAnnotations" -> Data(1)
        ),
        "method" -> Data.map(
          "hasExampleAnnotation" -> Data(false),
          "hasExampleAnnotation2" -> Data(true),
          "exampleAnnotation2Args" -> Data.list(Data.list(Data("42"))),
          "parentAnnotations" -> Data(1),
          "childAnnotations" -> Data(0),
          "parameters" -> Data.list(
            Data.map(
              "name" -> Data("arg"),
              "hasExampleAnnotation" -> Data(false),
              "exampleAnnotation2Args" -> Data.list(),
              "parentAnnotations" -> Data(0),
              "childAnnotations" -> Data(0)
            )
          )
        ),
        "constructor" -> Data.list()
      )
    }

    test("typed annotation expression from annotationsOfType decodes to values at macro time") {
      testAnnotationValueDecoded[examples.methods.NoCompanionClass] <==> Data.map(
        "wholeAnnotation" -> Data("1"),
        "firstArgument" -> Data("1")
      )
    }

    test(
      "@fieldName(\"first_name\") on case class ctor param decodes String/Boolean/Double literals"
    ) {
      testFieldNamesFromAnnotations[examples.methods.Person] <==> Data.list(
        Data.map(
          "name" -> Data("firstName"),
          "fieldNames" -> Data.list(Data("first_name")),
          "fieldFlags" -> Data.list()
        ),
        Data.map(
          "name" -> Data("age"),
          "fieldNames" -> Data.list(Data("age")),
          "fieldFlags" -> Data.list(Data.list(Data("true"), Data("1.5")))
        )
      )
    }
  }

  group("methods: splicing annotation exprs into macro output") {
    import MethodsFixtures.testSplicedAnnotationValue

    test("type annotation expr spliced into generated code evaluates at runtime") {
      testSplicedAnnotationValue[examples.methods.NoCompanionClass]("") <==> Data.map(
        "typeValue" -> Data(1),
        "methodValue" -> Data(-1),
        "parameterValue" -> Data(-1)
      )
    }

    test("method annotation expr spliced into generated code evaluates at runtime") {
      testSplicedAnnotationValue[examples.methods.WithChildAnnotation]("annotatedMethod2") <==> Data.map(
        "typeValue" -> Data(-1),
        "methodValue" -> Data(42),
        "parameterValue" -> Data(-1)
      )
    }

    test("constructor parameter annotation expr spliced into generated code evaluates at runtime") {
      testSplicedAnnotationValue[examples.methods.WithAnnotatedParams]("") <==> Data.map(
        "typeValue" -> Data(-1),
        "methodValue" -> Data(-1),
        "parameterValue" -> Data(1)
      )
    }
  }

  group("methods: fold on abstract trait method") {
    import MethodsFixtures.testCallInstanceViaFold

    test("calling abstract method on trait-typed instance via fold") {
      val instance: examples.methods.TraitWithAbstractMethod = new examples.methods.TraitWithAbstractMethodImpl
      testCallInstanceViaFold[examples.methods.TraitWithAbstractMethod](instance)("compute")(42) <==> Data(
        "result:42"
      )
    }

    test("calling abstract method via AnonymousInstance.mustOverride + fold") {
      val instance: examples.methods.SimpleAlg = new examples.methods.SimpleAlgImpl
      MethodsFixtures.testFoldAnonymousInstanceMethod[examples.methods.SimpleAlg](
        instance,
        "getUser"
      ) <==> Data("user:42")
    }
  }

  group("methods: default values on generic case class") {
    import MethodsFixtures.testDefaultValueOnGenericMethod

    test("copy method on GenericWithDefaults[Int] resolves defaults with correct types") {
      val result =
        testDefaultValueOnGenericMethod(examples.methods.GenericWithDefaults(42, "hello"), "copy")
      val list = result.asList.get
      assert(list.size == 2, s"copy should have 2 params, got ${list.size}")
      val valueParam = list(0).asMap.get
      val labelParam = list(1).asMap.get
      assert(valueParam("name").asString.get == "value")
      assert(valueParam("default").asString.get.nonEmpty, "value should have a default (from copy)")
      assert(!valueParam.contains("error"), s"value default should not error: ${valueParam.get("error")}")
      assert(labelParam("name").asString.get == "label")
      assert(!labelParam.contains("error"), s"label default should not error: ${labelParam.get("error")}")
    }
  }

  group("methods: return type of a generic method is substituted once its type parameters are applied") {
    import examples.genericmethods.*
    import MethodsFixtures.{
      testGenericMethodChain,
      testGenericMethodChainApplyingConstructorOf,
      testGenericMethodChainApplyingCtor1Of
    }

    // `knownReturning` / `Returned` / the built expression's type are rendered as "=:= R" when they are =:= the
    // expected (substituted) return type `R`.
    def step(name: String, knownReturning: String, expectations: String*): Data = Data.map(
      "step" -> Data(name),
      "knownReturning" -> Data(knownReturning),
      "expectations" -> Data.list(expectations.map(Data(_))*)
    )
    val result: Data = Data.map(
      "Result.Returned" -> Data("=:= R"),
      "Returned <:< String" -> Data(false),
      "built" -> Data("built: =:= R")
    )
    val P = "hearth.examples.genericmethods"
    val serviceBefore = s"NeedsValues(config: $P.Config)(evidence$$1: $P.SyncLike[F])"
    val serviceAfter = s"NeedsValues(config: $P.Config)(evidence$$1: $P.SyncLike[$P.SomeF])"
    val serviceSteps = Data.list(
      step("OnInstance", "<none>", "NeedsInstance", "NeedsTypes[F]", serviceBefore),
      step("ApplyTypes", "<none>", "NeedsInstance", "NeedsTypes[F]", serviceBefore),
      // known right after ApplyTypes, before the value clauses are applied
      step("ApplyValues", "=:= R", "NeedsInstance", "NeedsTypes[F]", serviceAfter),
      step("Result", "=:= R", "NeedsInstance", "NeedsTypes[F]", serviceAfter),
      result
    )

    test("type clause only: def resource[F[_]]: Res[F, Widget]") {
      testGenericMethodChainApplyingConstructorOf[Widget.type, Res[SomeF, Widget], SomeF[Any]](Widget)(
        "resource"
      ) <==> Data.list(
        step("OnInstance", "<none>", "NeedsInstance", "NeedsTypes[F]"),
        step("ApplyTypes", "<none>", "NeedsInstance", "NeedsTypes[F]"),
        step("Result", "=:= R", "NeedsInstance", "NeedsTypes[F]"),
        result
      )
    }

    test("Result.Returned is never Nothing (which would conform to anything): def resource[F[_]]: Option[Plain]") {
      testGenericMethodChainApplyingConstructorOf[Plain.type, Option[Plain], SomeF[Any]](Plain)("resource") <==>
        Data.list(
          step("OnInstance", "<none>", "NeedsInstance", "NeedsTypes[F]"),
          step("ApplyTypes", "<none>", "NeedsInstance", "NeedsTypes[F]"),
          step("Result", "=:= R", "NeedsInstance", "NeedsTypes[F]"),
          result
        )
    }

    test("type, value and implicit clauses: def resource[F[_]: SyncLike](config: Config): Res[F, Service]") {
      testGenericMethodChainApplyingConstructorOf[Service.type, Res[SomeF, Service], SomeF[Any]](Service)(
        "resource"
      ) <==> serviceSteps
    }

    test("type argument given as Type.Ctor1.of[SomeF].asUntyped (an eta-expanded type lambda on Scala 3)") {
      testGenericMethodChainApplyingCtor1Of[Service.type, Res[SomeF, Service]](Service)("resource") <==> serviceSteps
    }

    test("class and method type parameters both substituted: Holder[Int].pair[B]: (Int, B)") {
      testGenericMethodChain[Holder[Int], (Int, String), String](new Holder[Int])("pair") <==> Data.list(
        step("OnInstance", "<none>", "NeedsInstance", "NeedsTypes[B]"),
        step("ApplyTypes", "<none>", "NeedsInstance", "NeedsTypes[B]"),
        step("Result", "=:= R", "NeedsInstance", "NeedsTypes[B]"),
        result
      )
    }
  }

  group("methods: documented shapes and edge cases") {
    import examples.methodsedge.*
    val P = "hearth.examples.methodsedge"

    def flags(overrides: (String, Data)*): Data = Data.map(
      (List[(String, Data)](
        "isConstructor" -> Data(false),
        "isPrivate" -> Data(false),
        "isProtected" -> Data(false),
        "privateWithin" -> Data("<none>"),
        "protectedWithin" -> Data("<none>"),
        "isAvailable(Everywhere)" -> Data(true),
        "isAvailable(AtCallSite)" -> Data(true),
        "isAvailable(Anywhere)" -> Data(true),
        "isFinal" -> Data(false),
        "isAbstract" -> Data(false),
        "isOverride" -> Data(false),
        "isImplicit" -> Data(false),
        "isVal" -> Data(false),
        "isVar" -> Data(false),
        "isLazy" -> Data(false),
        "isDef" -> Data(true),
        "isDeclared" -> Data(true),
        "isInherited" -> Data(false),
        "isSynthetic" -> Data(false)
      ).toMap ++ overrides).toList*
    )

    def accessors(overrides: (String, Data)*): Data = Data.map(
      (List[(String, Data)](
        "arity" -> Data(0),
        "isNullary" -> Data(true),
        "isUnary" -> Data(false),
        "isBinary" -> Data(false),
        "isNAry(3)" -> Data(false),
        "isScalaGetter" -> Data(false),
        "isScalaSetter" -> Data(false),
        "isScalaAccessor" -> Data(false),
        "scalaAccessorName" -> Data("<none>"),
        "isJavaGetter" -> Data(false),
        "isJavaSetter" -> Data(false),
        "isJavaAccessor" -> Data(false),
        "javaAccessorName" -> Data("<none>"),
        "isAccessor" -> Data(false),
        "accessorName" -> Data("<none>")
      ).toMap ++ overrides).toList*
    )
    val unary = List("arity" -> Data(1), "isNullary" -> Data(false), "isUnary" -> Data(true))

    def param(name: String, index: Int, tpe: String, overrides: (String, Data)*): Data = Data.map(
      (List[(String, Data)](
        "name" -> Data(name),
        "param.name" -> Data(name),
        "index" -> Data(index),
        "tpe" -> Data(tpe),
        "hasDefault" -> Data(false),
        "defaultValue" -> Data("<none>"),
        "isByName" -> Data(false),
        "byNameUnderlying" -> Data("<none>"),
        "isVararg" -> Data(false),
        "isImplicit" -> Data(false)
      ).toMap ++ overrides).toList*
    )

    def step(name: String, parameters: String, totalParameters: String, knownReturning: String, toString: String) =
      Data.map(
        "step" -> Data(name),
        "parameters" -> Data(parameters),
        "totalParameters" -> Data(totalParameters),
        "knownReturning" -> Data(knownReturning),
        "toString" -> Data(toString)
      )

    group("plainPrint/prettyPrint render every documented modifier") {
      import MethodsFixtures.testMethodSignatures

      test("final, private, protected, private[scope], protected[scope] and implicit") {
        val M = s"$P.Modifiers"
        testMethodSignatures[Modifiers]("finalDef") <==> Data.list(Data(s"$M: final def finalDef: scala.Int"))
        testMethodSignatures[Modifiers]("privateDef") <==> Data.list(Data(s"$M: private def privateDef: scala.Int"))
        testMethodSignatures[Modifiers]("protectedDef") <==>
          Data.list(Data(s"$M: protected def protectedDef: scala.Int"))
        testMethodSignatures[Modifiers]("packagePrivateDef") <==>
          Data.list(Data(s"$M: private[methodsedge] def packagePrivateDef: scala.Int"))
        testMethodSignatures[Modifiers]("packageProtectedDef") <==>
          Data.list(Data(s"$M: protected[methodsedge] def packageProtectedDef: scala.Int"))
        testMethodSignatures[Modifiers]("implicitDef") <==>
          Data.list(Data(s"$M: implicit def implicitDef: java.lang.String"))
        testMethodSignatures[Modifiers]("abstractDef") <==>
          Data.list(Data(s"$M: def abstractDef(a: scala.Int): scala.Int"))
      }

      test("override, final override and abstract override") {
        testMethodSignatures[ModifiersImpl]("abstractDef") <==>
          Data.list(Data(s"$P.ModifiersImpl: override def abstractDef(a: scala.Int): scala.Int"))
        testMethodSignatures[ModifiersImpl]("concreteDef") <==>
          Data.list(Data(s"$P.ModifiersImpl: final override def concreteDef: scala.Int"))
        testMethodSignatures[StackableOverride]("stack") <==>
          Data.list(Data(s"$P.StackableOverride: abstract override def stack(a: scala.Int): scala.Int"))
      }

      test("lower and lower-and-upper type bounds") {
        testMethodSignatures[Bounds]("lower") <==> Data.list(Data(s"$P.Bounds: def lower[B >: scala.Int](b: B): B"))
        testMethodSignatures[Bounds]("both") <==>
          Data.list(Data(s"$P.Bounds: def both[C >: java.lang.String <: java.lang.CharSequence](c: C): C"))
      }

      test("overloaded methods are all listed, in declaration order") {
        testMethodSignatures[Overloads]("over") <==> Data.list(
          Data(s"$P.Overloads: def over(a: scala.Int): scala.Int"),
          Data(s"$P.Overloads: def over(a: java.lang.String): java.lang.String"),
          Data(s"$P.Overloads: def over(a: scala.Int, b: scala.Int): scala.Int")
        )
      }

      test("symbolic names are rendered and looked up by their source name") {
        testMethodSignatures[Symbolic]("+") <==> Data.list(Data(s"$P.Symbolic: def +(other: scala.Int): scala.Int"))
        testMethodSignatures[Symbolic]("unary_-") <==> Data.list(Data(s"$P.Symbolic: def unary_-: scala.Int"))
      }

      test("defaults on a later parameter list, by-name and implicit parameter lists") {
        testMethodSignatures[Shapes]("laterDefaults") <==> Data.list(
          Data(
            s"$P.Shapes: def laterDefaults(a: scala.Int)(b: scala.Int = <default>, c: scala.Int = <default>): scala.Int"
          )
        )
        testMethodSignatures[Shapes]("byName") <==> Data.list(
          Data(s"$P.Shapes: def byName(a: => scala.Int): scala.Int")
        )
        testMethodSignatures[Shapes]("curried") <==>
          Data.list(Data(s"$P.Shapes: def curried(a: scala.Int)(b: scala.Int)(c: scala.Int): scala.Int"))
      }

      test("val, var and lazy val members are rendered with their keyword") {
        testMethodSignatures[Accessors]("plainVal") <==> Data.list(Data(s"$P.Accessors: val plainVal: scala.Int"))
        testMethodSignatures[Accessors]("plainVar") <==> Data.list(Data(s"$P.Accessors: var plainVar: scala.Int"))
        testMethodSignatures[Accessors]("plainLazy") <==>
          Data.list(Data(s"$P.Accessors: lazy val plainLazy: scala.Int"))
        testMethodSignatures[Accessors]("plainDef") <==> Data.list(Data(s"$P.Accessors: def plainDef: scala.Int"))
      }

      test("a method inherited from a generic parent has the parent's type arguments applied") {
        testMethodSignatures[IntChild]("get") <==> Data.list(Data(s"$P.IntChild: def get(a: scala.Int): scala.Int"))
        testMethodSignatures[Aliases.GenParentOfString]("get") <==>
          Data.list(Data(s"$P.GenParent[java.lang.String]: def get(a: java.lang.String): java.lang.String"))
      }

      test("constructors: secondary constructors, private constructor and defaults of a generic class") {
        import MethodsFixtures.testConstructorSignatures
        testConstructorSignatures[MultiCtor] <==> Data.list(
          Data(s"new $P.MultiCtor()"),
          Data(s"new $P.MultiCtor(a: scala.Int)"),
          Data(s"new $P.MultiCtor(a: scala.Int, b: java.lang.String)")
        )
        testConstructorSignatures[PrivateCtor] <==> Data.list(Data(s"new $P.PrivateCtor(a: scala.Int)"))
        testConstructorSignatures[GenDefaults[Int]] <==> Data.list(
          Data(
            s"new $P.GenDefaults[scala.Int](a: scala.Int, xs: scala.collection.immutable.List[scala.Int] = <default>)"
          )
        )
      }
    }

    group("constructors: primaryConstructor, defaultConstructor and constructors") {
      import MethodsFixtures.{testConstructorFlags, testConstructorsExtraction}

      test("for a class with secondary constructors, defaultConstructor is the nullary one") {
        testConstructorsExtraction[MultiCtor] <==> Data.map(
          "primaryConstructor" -> Data("(a: scala.Int, b: java.lang.String)"),
          "defaultConstructor" -> Data("()"),
          "constructors" -> Data.list(Data("(a: scala.Int, b: java.lang.String)"), Data("(a: scala.Int)"), Data("()"))
        )
      }

      test("for a trait there is no constructor") {
        testConstructorsExtraction[NoCtorTrait] <==> Data.map(
          "primaryConstructor" -> Data("<no primary constructor>"),
          "defaultConstructor" -> Data("<no default constructor>"),
          "constructors" -> Data.list()
        )
      }

      test("a private constructor is listed but not available") {
        testConstructorFlags[PrivateCtor] <==> Data.list(
          Data.map(
            "plainPrint" -> Data(s"new $P.PrivateCtor(a: scala.Int)"),
            "variant" -> Data("ApplyValues"),
            "flags" -> flags(
              "isConstructor" -> Data(true),
              "isPrivate" -> Data(true),
              "isAvailable(Everywhere)" -> Data(false),
              "isAvailable(AtCallSite)" -> Data(false)
            )
          )
        )
      }
    }

    group("modifier, kind and visibility predicates") {
      import MethodsFixtures.testMethodFlags

      test("isFinal, isAbstract and isImplicit") {
        testMethodFlags[Modifiers]("finalDef") <==> Data.list(flags("isFinal" -> Data(true)))
        testMethodFlags[Modifiers]("abstractDef") <==> Data.list(flags("isAbstract" -> Data(true)))
        testMethodFlags[Modifiers]("implicitDef") <==> Data.list(flags("isImplicit" -> Data(true)))
        testMethodFlags[AbstractMembers]("abstractDef") <==> Data.list(flags("isAbstract" -> Data(true)))
      }

      test("isOverride for override, final override and abstract override") {
        testMethodFlags[ModifiersImpl]("abstractDef") <==> Data.list(flags("isOverride" -> Data(true)))
        testMethodFlags[ModifiersImpl]("concreteDef") <==>
          Data.list(flags("isOverride" -> Data(true), "isFinal" -> Data(true)))
        testMethodFlags[StackableOverride]("stack") <==>
          Data.list(flags("isOverride" -> Data(true), "isAbstract" -> Data(true)))
      }

      test("isPrivate/isProtected are strict, privateWithin/protectedWithin hold the qualifier") {
        val notAvailable = List("isAvailable(Everywhere)" -> Data(false), "isAvailable(AtCallSite)" -> Data(false))
        testMethodFlags[Modifiers]("privateDef") <==> Data.list(
          flags((List("isPrivate" -> Data(true)) ++ notAvailable)*)
        )
        testMethodFlags[Modifiers]("protectedDef") <==>
          Data.list(flags((List("isProtected" -> Data(true)) ++ notAvailable)*))
        testMethodFlags[Modifiers]("packagePrivateDef") <==>
          Data.list(flags((List("privateWithin" -> Data("methodsedge")) ++ notAvailable)*))
        testMethodFlags[Modifiers]("packageProtectedDef") <==>
          Data.list(flags((List("protectedWithin" -> Data("methodsedge")) ++ notAvailable)*))
      }

      test("isDeclared/isInherited for a member inherited from a parent") {
        testMethodFlags[ModifiersImpl]("finalDef") <==>
          Data.list(flags("isFinal" -> Data(true), "isDeclared" -> Data(false), "isInherited" -> Data(true)))
        testMethodFlags[IntChild]("get") <==>
          Data.list(flags("isDeclared" -> Data(false), "isInherited" -> Data(true)))
      }

      test("isVal, isVar, isLazy and isDef for each kind of member") {
        testMethodFlags[Accessors]("plainVal") <==> Data.list(flags("isVal" -> Data(true), "isDef" -> Data(false)))
        testMethodFlags[Accessors]("plainVar") <==> Data.list(flags("isVar" -> Data(true), "isDef" -> Data(false)))
        testMethodFlags[Accessors]("plainLazy") <==>
          Data.list(flags("isVal" -> Data(true), "isLazy" -> Data(true), "isDef" -> Data(false)))
        testMethodFlags[Accessors]("plainDef") <==> Data.list(flags())
      }

      test("an abstract val is a val and abstract") {
        testMethodFlags[AbstractMembers]("abstractVal") <==>
          Data.list(flags("isVal" -> Data(true), "isAbstract" -> Data(true), "isDef" -> Data(false)))
      }
    }

    group("arity and accessor predicates") {
      import MethodsFixtures.testMethodAccessorsAndArity

      test("JavaBean getters: getX returning non-Unit and isX returning Boolean") {
        testMethodAccessorsAndArity[ScalaBean]("getFoo") <==> Data.list(
          accessors(
            "isJavaGetter" -> Data(true),
            "isJavaAccessor" -> Data(true),
            "javaAccessorName" -> Data("foo"),
            "isAccessor" -> Data(true),
            "accessorName" -> Data("foo")
          )
        )
        testMethodAccessorsAndArity[ScalaBean]("isBar") <==> Data.list(
          accessors(
            "isJavaGetter" -> Data(true),
            "isJavaAccessor" -> Data(true),
            "javaAccessorName" -> Data("bar"),
            "isAccessor" -> Data(true),
            "accessorName" -> Data("bar")
          )
        )
      }

      test("not JavaBean getters: isX not returning Boolean, getX returning Unit or taking arguments") {
        testMethodAccessorsAndArity[ScalaBean]("isBaz") <==> Data.list(accessors())
        testMethodAccessorsAndArity[ScalaBean]("getUnit") <==> Data.list(accessors())
        testMethodAccessorsAndArity[ScalaBean]("getWithArg") <==> Data.list(accessors(unary*))
      }

      test("JavaBean setters: unary setX returning Unit; not when returning a value or taking two arguments") {
        testMethodAccessorsAndArity[ScalaBean]("setFoo") <==> Data.list(
          accessors(
            (unary ++ List(
              "isJavaSetter" -> Data(true),
              "isJavaAccessor" -> Data(true),
              "javaAccessorName" -> Data("foo"),
              "isAccessor" -> Data(true),
              "accessorName" -> Data("foo")
            ))*
          )
        )
        testMethodAccessorsAndArity[ScalaBean]("setReturning") <==> Data.list(accessors(unary*))
        testMethodAccessorsAndArity[ScalaBean]("setTwo") <==> Data.list(
          accessors("arity" -> Data(2), "isNullary" -> Data(false), "isBinary" -> Data(true))
        )
      }

      test("Scala getters and setters of val, var and lazy val; a def is not an accessor") {
        def getter(name: String) = accessors(
          "isScalaGetter" -> Data(true),
          "isScalaAccessor" -> Data(true),
          "scalaAccessorName" -> Data(name),
          "isAccessor" -> Data(true),
          "accessorName" -> Data(name)
        )
        testMethodAccessorsAndArity[Accessors]("plainVal") <==> Data.list(getter("plainVal"))
        testMethodAccessorsAndArity[Accessors]("plainVar") <==> Data.list(getter("plainVar"))
        testMethodAccessorsAndArity[Accessors]("plainLazy") <==> Data.list(getter("plainLazy"))
        testMethodAccessorsAndArity[Accessors]("plainVar_=") <==> Data.list(
          accessors(
            (unary ++ List(
              "isScalaSetter" -> Data(true),
              "isScalaAccessor" -> Data(true),
              "scalaAccessorName" -> Data("plainVar"),
              "isAccessor" -> Data(true),
              "accessorName" -> Data("plainVar")
            ))*
          )
        )
        testMethodAccessorsAndArity[Accessors]("plainDef") <==> Data.list(accessors())
      }

      test("arity counts the parameters of every parameter list, including the implicit one") {
        val threeArgs = accessors("arity" -> Data(3), "isNullary" -> Data(false), "isNAry(3)" -> Data(true))
        testMethodAccessorsAndArity[Shapes]("threeArgs") <==> Data.list(threeArgs)
        testMethodAccessorsAndArity[Shapes]("curried") <==> Data.list(threeArgs)
      }
    }

    group("parameters") {
      import MethodsFixtures.{testConstructorParameters, testMethodParameters}

      test("defaults on a later parameter list, by-name and implicit parameters") {
        testMethodParameters[Shapes]("byName") <==> Data.list(
          Data.list(
            Data.list(
              param("a", 0, "<by-name>", "isByName" -> Data(true), "byNameUnderlying" -> Data("scala.Int"))
            )
          )
        )
        testMethodParameters[Shapes]("curried") <==> Data.list(
          Data.list(
            Data.list(param("a", 0, "scala.Int")),
            Data.list(param("b", 1, "scala.Int")),
            Data.list(param("c", 2, "scala.Int", "isImplicit" -> Data(true)))
          )
        )
        testMethodParameters[Shapes]("laterDefaults") <==> Data.list(
          Data.list(
            Data.list(param("a", 0, "scala.Int")),
            Data.list(
              param(
                "b",
                1,
                "scala.Int",
                "hasDefault" -> Data(true),
                "defaultValue" -> Data("returning scala.Int, expecting NeedsInstance -> NeedsValues(a)")
              ),
              param(
                "c",
                2,
                "scala.Int",
                "hasDefault" -> Data(true),
                "defaultValue" -> Data("returning scala.Int, expecting NeedsInstance -> NeedsValues(a)")
              )
            )
          )
        )
      }

      test("defaultValue of a generic class has the class type arguments applied") {
        testConstructorParameters[GenDefaults[Int]] <==> Data.list(
          Data.list(
            param("a", 0, "scala.Int"),
            param(
              "xs",
              1,
              "scala.collection.immutable.List[scala.Int]",
              "hasDefault" -> Data(true),
              "defaultValue" -> Data("returning scala.collection.immutable.List[scala.Int], expecting ")
            )
          )
        )
        testMethodParameters[GenMethodDefaults[String]]("withDefault") <==> Data.list(
          Data.list(
            Data.list(
              param("x", 0, "scala.Int"),
              param(
                "y",
                1,
                "java.lang.String",
                "hasDefault" -> Data(true),
                "defaultValue" -> Data("returning java.lang.String, expecting NeedsInstance")
              )
            )
          )
        )
      }
    }

    group("builder chain: steps, parameters, totalParameters, knownReturning and toString") {
      import MethodsFixtures.{testChainWalk, testConstructorChainWalk}

      test("defaults on a later parameter list are filled in when the arguments are skipped") {
        val sig =
          s"$P.Shapes: def laterDefaults(a: scala.Int)(b: scala.Int = <default>, c: scala.Int = <default>): scala.Int"
        testChainWalk(new Shapes)("laterDefaults")(1) <==> Data.map(
          "steps" -> Data.list(
            step("OnInstance", "<none>", "(a)(b, c)", "scala.Int", sig),
            step("ApplyValues", "(a)(b, c)", "(a)(b, c)", "scala.Int", s"$sig (on <instance>)"),
            step("Result", "<none>", "(a)(b, c)", "scala.Int", s"$sig (on <instance>, applied (a = 1))")
          ),
          "result" -> Data("6")
        )
      }

      test("by-name and implicit parameters are passed by name like any other argument") {
        testChainWalk(new Shapes)("byName")(5).asMap.get("result") <==> Data("5")
        testChainWalk(new Shapes)("curried")(1, 2, 3).asMap.get("result") <==> Data("6")
      }

      test("a nullary companion method listed on the class is a Result right away") {
        testChainWalk(new Registry)("count")() <==> Data.map(
          "steps" -> Data.list(step("Result", "<none>", "<none>", "scala.Int", s"$P.Registry: def count: scala.Int")),
          "result" -> Data("3")
        )
        testChainWalk(new Registry)("make")(21) <==> Data.map(
          "steps" -> Data.list(
            step("ApplyValues", "(a)", "(a)", "scala.Int", s"$P.Registry: def make(a: scala.Int): scala.Int"),
            step(
              "Result",
              "<none>",
              "(a)",
              "scala.Int",
              s"$P.Registry: def make(a: scala.Int): scala.Int (applied (a = 21))"
            )
          ),
          "result" -> Data("42")
        )
      }

      test("toString shows applied type arguments and the return type resolved after applying them") {
        val sig = "hearth.examples.methods.Parametric: def parametric2[A](a: A): A"
        testChainWalk(new examples.methods.Parametric)("parametric2")(7) <==> Data.map(
          "steps" -> Data.list(
            step("OnInstance", "<none>", "(a)", "<none>", sig),
            step("ApplyTypes", "<none>", "(a)", "<none>", s"$sig (on <instance>)"),
            step(
              "ApplyValues",
              "(a)",
              "(a)",
              "scala.Int",
              s"$sig (on <instance>, applied [A = scala.Int], returning scala.Int)"
            ),
            step(
              "Result",
              "<none>",
              "(a)",
              "scala.Int",
              s"$sig (on <instance>, applied [A = scala.Int](a = 7), returning scala.Int)"
            )
          ),
          "result" -> Data("7")
        )
      }

      test("a method inherited from a generic parent is called with the parent's type arguments applied") {
        testChainWalk(new IntChild)("get")(4).asMap.get("result") <==> Data("4")
      }

      test("defaults of generic classes are resolved against the type arguments") {
        testConstructorChainWalk[GenDefaults[Int]](1) <==> Data.map(
          "steps" -> Data.list(
            step(
              "ApplyValues",
              "(a, xs)",
              "(a, xs)",
              s"$P.GenDefaults[scala.Int]",
              s"new $P.GenDefaults[scala.Int](a: scala.Int, xs: scala.collection.immutable.List[scala.Int] = <default>)"
            ),
            step(
              "Result",
              "<none>",
              "(a, xs)",
              s"$P.GenDefaults[scala.Int]",
              s"new $P.GenDefaults[scala.Int](a: scala.Int, xs: scala.collection.immutable.List[scala.Int] = <default>) (applied (a = 1))"
            )
          ),
          "result" -> Data("GenDefaults(1, List())")
        )
        testChainWalk(new GenMethodDefaults[String]("a"))("withDefault")(1).asMap.get("result") <==> Data("1:a")
      }
    }

    group("builder chain errors") {

      test("build() reports arguments of the wrong type") {
        MethodsFixtures.testBuildWithWrongArgumentTypes(new Shapes)("threeArgs").asString.get.take(5) <==> "LEFT:"
      }
    }

    group("foldF") {

      test("foldF works with Either and Try and short-circuits on their failure") {
        MethodsFixtures.testFoldFEffects(new Shapes)("threeArgs")(1, 2, 3) <==> Data.map(
          "Either" -> Data("6"),
          "Try" -> Data("6")
        )
        MethodsFixtures.testFoldFEffects(new Shapes)("threeArgs")() <==> Data.map(
          "Either" -> Data("EFFECT FAILED: not enough Int arguments"),
          "Try" -> Data("EFFECT FAILED: not enough Int arguments")
        )
      }
    }

    group("path-dependent parameter lists") {

      test("a parameter list depending on an abstract type member of an earlier argument is a separate step") {
        MethodsFixtures.testMethodExpectations[AbstractPathDep]("unbox").asList.get.head.asMap.get("expectations") <==>
          Data.list(
            Data("NeedsInstance"),
            Data(s"NeedsValues(b: $P.AbstractPathDep#Box)"),
            Data("NeedsValues(t: b.T)")
          )
      }
    }

    group("listing methods") {
      import MethodsFixtures.{testHasMethodsNamed, testMethodListings}

      test("methods, unsortedMethods, unsortedMethodsNamed, Method.sort/sortBy and the Method.* companions agree") {
        testMethodListings[Ordered] <==> Data.map(
          "declared names in order" -> Data
            .list(Data("first"), Data("second"), Data("zeta"), Data("alpha"), Data("mid")),
          "methods contain the same methods as unsortedMethods" -> Data(true),
          "methods == Method.sort(unsortedMethods)" -> Data(true),
          "Method.sort(unsorted.filter(p)) == methods.filter(p)" -> Data(true),
          "Method.sortBy(pairs)(_._2) == Method.sort" -> Data(true),
          "unsortedMethodsNamed(n) == unsortedMethods.filter(_.name == n)" -> Data(true),
          "Method.methodsOf == Type.methods" -> Data(true),
          "Method.unsortedMethodsOf == Type.unsortedMethods" -> Data(true),
          "Method.constructorsOf == Type.constructors" -> Data(true),
          "Method.primaryConstructorOf == Type.primaryConstructor" -> Data(true)
        )
      }

      test("methods of Any and of a primitive type") {
        testHasMethodsNamed[Any]("toString", "hashCode", "equals", "==", "!=", "##", "isInstanceOf") <==> Data.map(
          "toString" -> Data(true),
          "hashCode" -> Data(true),
          "equals" -> Data(true),
          "==" -> Data(true),
          "!=" -> Data(true),
          "##" -> Data(true),
          "isInstanceOf" -> Data(true)
        )
        testHasMethodsNamed[Int]("+", "toLong", "toString") <==> Data.map(
          "+" -> Data(true),
          "toLong" -> Data(true),
          "toString" -> Data(true)
        )
      }
    }
  }
}
