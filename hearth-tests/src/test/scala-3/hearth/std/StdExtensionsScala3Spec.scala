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
  }
}
