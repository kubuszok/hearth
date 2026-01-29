package hearth
package std

import hearth.data.Data

/** Macro implementation is in [[StdExtensionsFixturesImpl]] */
final class StdExtensionsScala3Spec extends MacroSuite {

  group("trait std.StdExtensions") {

    group("class: IsCollection[A], returns preprocessed collection") {
      import StdExtensionsFixtures.testIsCollection

      test("for IArray") {
        testIsCollection(IArray[Short](1, 2, 3)) <==> Data.map(
          "building" -> Data("<not a collection of string>"),
          "iteration" -> Data.list(
            Data("1"),
            Data("2"),
            Data("3")
          )
        )
        testIsCollection(IArray[Int](1, 2, 3)) <==> Data.map(
          "building" -> Data("<not a collection of string>"),
          "iteration" -> Data.list(
            Data("1"),
            Data("2"),
            Data("3")
          )
        )
        testIsCollection(IArray[Long](1, 2, 3)) <==> Data.map(
          "building" -> Data("<not a collection of string>"),
          "iteration" -> Data.list(
            Data("1"),
            Data("2"),
            Data("3")
          )
        )
        testIsCollection(IArray[Float](1, 2, 3)) <==> Data.map(
          "building" -> Data("<not a collection of string>"),
          "iteration" -> Data.list(
            Data(if Platform.byHearth.isJs then "1" else "1.0"),
            Data(if Platform.byHearth.isJs then "2" else "2.0"),
            Data(if Platform.byHearth.isJs then "3" else "3.0")
          )
        )
        testIsCollection(IArray[Double](1, 2, 3)) <==> Data.map(
          "building" -> Data("<not a collection of string>"),
          "iteration" -> Data.list(
            Data(if Platform.byHearth.isJs then "1" else "1.0"),
            Data(if Platform.byHearth.isJs then "2" else "2.0"),
            Data(if Platform.byHearth.isJs then "3" else "3.0")
          )
        )
        testIsCollection(IArray[Char]('1', '2', '3')) <==> Data.map(
          "building" -> Data("<not a collection of string>"),
          "iteration" -> Data.list(
            Data("1"),
            Data("2"),
            Data("3")
          )
        )
        testIsCollection(IArray[Boolean](true, false, true)) <==> Data.map(
          "building" -> Data("<not a collection of string>"),
          "iteration" -> Data.list(
            Data("true"),
            Data("false"),
            Data("true")
          )
        )
        testIsCollection(IArray[Byte](1, 2, 3)) <==> Data.map(
          "building" -> Data("<not a collection of string>"),
          "iteration" -> Data.list(
            Data("1"),
            Data("2"),
            Data("3")
          )
        )
        testIsCollection(IArray[Unit]((), (), ())) <==> Data.map(
          "building" -> Data("<not a collection of string>"),
          "iteration" -> Data.list(
            Data(if Platform.byHearth.isJs then "undefined" else "()"),
            Data(if Platform.byHearth.isJs then "undefined" else "()"),
            Data(if Platform.byHearth.isJs then "undefined" else "()")
          )
        )
        testIsCollection(IArray[AnyRef]("one", "two", "three")) <==> Data.map(
          "building" -> Data("<not a collection of string>"),
          "iteration" -> Data.list(
            Data("one"),
            Data("two"),
            Data("three")
          )
        )
        testIsCollection(IArray("one", "two", "three")) <==> Data.map(
          "building" -> Data(
            if Platform.byHearth.isNative then "scala.scalanative.runtime.ObjectArray" else "[Ljava.lang.String"
          ),
          "iteration" -> Data.list(
            Data("one"),
            Data("two"),
            Data("three")
          )
        )
      }
    }

    group("class: IsValueType[A], opaque types") {
      import StdExtensionsFixtures.testIsValueType
      import hearth.examples.opaqueid.OpaqueId

      test("for OpaqueId") {
        testIsValueType(OpaqueId(42L)) <==> Data.map(
          "unwrapping" -> Data("unwrapped: 42"),
          "wrapping" -> Data("<not a value type of string>")
        )
      }
    }

    group("class: CtorLikes[A], smart constructors discovery") {
      import StdExtensionsFixtures.testCtorLikes
      import hearth.examples.plainctor.PlainCtorOpaque
      import hearth.examples.eitherstringctor.EitherStringCtorOpaque
      import hearth.examples.eitheriterablestringctor.EitherIterableStringCtorOpaque
      import hearth.examples.eitherthrowablector.EitherThrowableCtorOpaque
      import hearth.examples.eitheriterablethrowablector.EitherIterableThrowableCtorOpaque

      // Helper to check if a Data contains a ctor with the given type and method name
      def containsCtor(data: Data, ctorType: String, methodName: String): Boolean =
        data.get("ctors").flatMap(_.asList).exists { ctors =>
          ctors.exists { ctor =>
            ctor.get("type").contains(Data(ctorType)) &&
            ctor.get("method").contains(Data(methodName))
          }
        }

      test("PlainValue smart constructor") {
        val result = testCtorLikes[PlainCtorOpaque]
        assert(
          containsCtor(result, "PlainValue", "apply"),
          s"Expected PlainValue ctor with 'apply', got: ${result.render}"
        )
      }

      test("EitherStringOrValue smart constructor") {
        val result = testCtorLikes[EitherStringCtorOpaque]
        assert(
          containsCtor(result, "EitherStringOrValue", "parse"),
          s"Expected EitherStringOrValue ctor with 'parse', got: ${result.render}"
        )
      }

      test("EitherIterableStringOrValue smart constructor") {
        val result = testCtorLikes[EitherIterableStringCtorOpaque]
        assert(
          containsCtor(result, "EitherIterableStringOrValue", "validate"),
          s"Expected EitherIterableStringOrValue ctor with 'validate', got: ${result.render}"
        )
      }

      test("EitherThrowableOrValue smart constructor") {
        val result = testCtorLikes[EitherThrowableCtorOpaque]
        assert(
          containsCtor(result, "EitherThrowableOrValue", "safeDivide"),
          s"Expected EitherThrowableOrValue ctor with 'safeDivide', got: ${result.render}"
        )
      }

      test("EitherIterableThrowableOrValue smart constructor") {
        val result = testCtorLikes[EitherIterableThrowableCtorOpaque]
        assert(
          containsCtor(result, "EitherIterableThrowableOrValue", "validateAll"),
          s"Expected EitherIterableThrowableOrValue ctor with 'validateAll', got: ${result.render}"
        )
      }

      test("OpaqueId with multiple smart constructors") {
        import hearth.examples.opaqueid.OpaqueId
        val result = testCtorLikes[OpaqueId]
        assert(
          containsCtor(result, "PlainValue", "apply"),
          s"Expected PlainValue ctor with 'apply', got: ${result.render}"
        )
        assert(
          containsCtor(result, "EitherStringOrValue", "fromString"),
          s"Expected EitherStringOrValue ctor with 'fromString', got: ${result.render}"
        )
      }
    }
  }
}
