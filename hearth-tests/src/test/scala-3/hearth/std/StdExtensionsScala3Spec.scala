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

    group("class: IsCollectionOf[A, Item].foreach, iterates via foreach") {
      import StdExtensionsFixtures.testIsCollectionForeach

      test("for IArray[Int]") {
        testIsCollectionForeach(IArray[Int](1, 2, 3)) <==> Data.list(
          Data("1"),
          Data("2"),
          Data("3")
        )
      }

      test("for IArray[String]") {
        testIsCollectionForeach(IArray("one", "two", "three")) <==> Data.list(
          Data("one"),
          Data("two"),
          Data("three")
        )
      }

      test("for IArray[Double]") {
        testIsCollectionForeach(IArray[Double](1.0, 2.0, 3.0)) <==> Data.list(
          Data(if Platform.byHearth.isJs then "1" else "1.0"),
          Data(if Platform.byHearth.isJs then "2" else "2.0"),
          Data(if Platform.byHearth.isJs then "3" else "3.0")
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

      test("for opaque type over a class with constructors") {
        import hearth.examples.unions.OpaqueName
        // the underlying String's constructors must not be used to (uncompilably) construct the opaque type
        testIsValueType(OpaqueName("x")) <==> Data.map(
          "unwrapping" -> Data("unwrapped: x"),
          "wrapping" -> Data("test")
        )
      }

      test("re-wrapping an opaque type yields the opaque type, not its representation") {
        import hearth.examples.unions.{OpaqueId, OpaqueName}
        val expected = Data.map(
          "rewrappedIsValueType" -> Data(true),
          "rewrappedIsInner" -> Data(false)
        )
        StdExtensionsFixtures.testIsValueTypeRewrapType(OpaqueId(1L)) <==> expected
        StdExtensionsFixtures.testIsValueTypeRewrapType(OpaqueName("x")) <==> expected
      }

      test("for IArray (should not be treated as value type)") {
        testIsValueType(IArray(1, 2, 3)) <==> Data("<no value type>")
      }
    }

    group("class: CtorLikes[A], smart constructors discovery") {
      import StdExtensionsFixtures.testCtorLikes
      import hearth.examples.plainctor.PlainCtorOpaque
      import hearth.examples.eitherstringctor.EitherStringCtorOpaque
      import hearth.examples.eitheriterablestringctor.EitherIterableStringCtorOpaque
      import hearth.examples.eitherthrowablector.EitherThrowableCtorOpaque
      import hearth.examples.eitheriterablethrowablector.EitherIterableThrowableCtorOpaque

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

      test(
        "opaque type over a class with constructors only exposes its companion's smart constructors"
      ) {
        import hearth.examples.unions.OpaqueName
        val result = testCtorLikes[OpaqueName]
        // the underlying java.lang.String's constructors must not be reported for the opaque type
        assert(
          result.get("count").contains(Data("1")) && containsCtor(result, "PlainValue", "apply"),
          s"Expected only the PlainValue ctor 'apply', got: ${result.render}"
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
    group("class: IsCollection[A], IArray item type, building and size hint") {
      import StdExtensionsFixtures.{testBuildAndIterate, testCollectionTypes, testSizeHint}

      test("for IArray") {
        testCollectionTypes[IArray[Int]] <==> Data.map(
          "item" -> Data("scala.Int"),
          "ctorResult" -> Data("scala.IArray$package.IArray[scala.Int]"),
          "build" -> Data("PlainValue"),
          "asMap" -> Data("<not a map>"),
          "IsMap" -> Data("<not a map>")
        )
        testBuildAndIterate[IArray[Int]] <==> Data.map(
          "one" -> Data.list(Data("1")),
          "two" -> Data.list(Data("1"), Data("2"))
        )
        testSizeHint(IArray(1, 2, 3)) <==> Data(3)
      }
    }

    group("class: IsValueType[A], opaque type shapes") {
      import StdExtensionsFixtures.{testIsValueType, testValueTypeInfo}

      def info(inner: String, wrap: String, ctors: String*): Data = Data.map(
        "inner" -> Data(inner),
        "wrap" -> Data(wrap),
        "ctors" -> Data.list(ctors.map(Data(_))*),
        "provenance" -> Data.map(
          "provider" -> Data("hearth.std.extensions.IsValueTypeProviderForOpaque"),
          "isBuiltIn" -> Data(true)
        )
      )

      test("for an opaque type with a PlainValue companion constructor") {
        import hearth.examples.plainctor.PlainCtorOpaque
        testValueTypeInfo[PlainCtorOpaque] <==> info("java.lang.String", "PlainValue", "PlainValue")
        testIsValueType(PlainCtorOpaque("value")) <==> Data.map(
          "unwrapping" -> Data("unwrapped: value"),
          "wrapping" -> Data("test")
        )
      }

      test("for an opaque type with only an Either[Iterable[String], A] companion constructor") {
        import hearth.examples.eitheriterablestringctor.EitherIterableStringCtorOpaque
        testValueTypeInfo[EitherIterableStringCtorOpaque] <==> info(
          "scala.Int",
          "EitherIterableStringOrValue",
          "EitherIterableStringOrValue"
        )
      }

      test("for a bounded opaque type") {
        import hearth.examples.opaqueunderlying.Bounded
        testValueTypeInfo[Bounded] <==> info("scala.Int", "PlainValue", "PlainValue")
      }

      test("for an opaque type over another opaque type") {
        import hearth.examples.opaqueunderlying.Outer
        testValueTypeInfo[Outer] <==> info("scala.Int", "PlainValue", "PlainValue")
      }

      test("for an applied generic opaque type and its alias") {
        import hearth.examples.opaqueunderlying.Wrapper
        import hearth.examples.stdedgeopaque.{GenericOpaque, GenericOpaqueOfString}
        testValueTypeInfo[GenericOpaque[String]] <==> info("java.lang.String", "PlainValue", "PlainValue")
        testValueTypeInfo[GenericOpaqueOfString] <==> info("java.lang.String", "PlainValue", "PlainValue")
      }

      test("for an applied generic opaque type over a generic representation") {
        import hearth.examples.opaqueunderlying.Wrapper
        testValueTypeInfo[Wrapper[Int]] <==> info(
          "scala.collection.immutable.List[scala.Int]",
          "PlainValue",
          "PlainValue"
        )
      }

      test("for an alias of an opaque type") {
        import hearth.examples.opaqueunderlying.AliasToOpaque
        testValueTypeInfo[AliasToOpaque] <==> info("scala.Long", "PlainValue", "EitherStringOrValue", "PlainValue")
      }

      test("CtorLikes of applied generic opaque types use the applied type arguments") {
        import StdExtensionsFixtures.testCtorLikes
        import hearth.examples.opaqueunderlying.Wrapper
        import hearth.examples.stdedgeopaque.GenericOpaque
        testCtorLikes[GenericOpaque[String]] <==> Data.map(
          "count" -> Data("1"),
          "ctors" -> Data.list(
            Data.map("type" -> Data("PlainValue"), "input" -> Data("java.lang.String"), "method" -> Data("apply"))
          )
        )
        testCtorLikes[Wrapper[Int]] <==> Data.map(
          "count" -> Data("1"),
          "ctors" -> Data.list(
            Data.map(
              "type" -> Data("PlainValue"),
              "input" -> Data("scala.collection.immutable.List[scala.Int]"),
              "method" -> Data("apply")
            )
          )
        )
      }

      test("CtorLikes of an alias of an opaque type are those of the opaque type") {
        import StdExtensionsFixtures.testCtorLikes
        import hearth.examples.opaqueunderlying.AliasToOpaque
        testCtorLikes[AliasToOpaque] <==> Data.map(
          "count" -> Data("2"),
          "ctors" -> Data.list(
            Data.map("type" -> Data("PlainValue"), "input" -> Data("scala.Long"), "method" -> Data("apply")),
            Data.map(
              "type" -> Data("EitherStringOrValue"),
              "input" -> Data("java.lang.String"),
              "method" -> Data("fromString")
            )
          )
        )
      }

      test("not for an opaque type without a companion constructor") {
        import hearth.examples.stdedgeopaque.NoCompanionOpaque
        testValueTypeInfo[NoCompanionOpaque] <==> Data("<no value type>")
      }
    }
  }
}
