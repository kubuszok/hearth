package hearth
package data

import scala.collection.immutable.ListMap

/** Pure-logic tests for the runtime `Data` API itself (constructors, accessors, folding, updating, parsing, rendering,
  * equality and diffing).
  *
  * The existing [[DataSpec]] verifies the macro round-trip; this spec drives the public value API directly so it runs
  * on every platform without a macro fixture.
  */
final class DataValuesSpec extends MacroSuite {

  group("Data constructors and accessors") {

    test("apply() should create a null Data whose only matching accessor is asNull") {
      val d = Data()
      d.asNull ==> Some(())
      d.asInt ==> None
      d.asString ==> None
      d.asList ==> None
      d.asMap ==> None
    }

    test("apply(Int) round-trips through asInt and rejects other accessors") {
      val d = Data(42)
      d.asInt ==> Some(42)
      d.asLong ==> None
      d.asDouble ==> None
      d.asString ==> None
      d.asNull ==> None
    }

    test("apply(Long) round-trips through asLong") {
      val d = Data(42L)
      d.asLong ==> Some(42L)
      d.asInt ==> None
    }

    test("apply(Float) round-trips through asFloat") {
      val d = Data(3.14f)
      d.asFloat ==> Some(3.14f)
      d.asDouble ==> None
    }

    test("apply(Double) round-trips through asDouble") {
      val d = Data(2.71828)
      d.asDouble ==> Some(2.71828)
      d.asFloat ==> None
    }

    test("apply(Boolean) round-trips through asBoolean") {
      Data(true).asBoolean ==> Some(true)
      Data(false).asBoolean ==> Some(false)
      Data(true).asString ==> None
    }

    test("apply(String) round-trips through asString") {
      val d = Data("hello")
      d.asString ==> Some("hello")
      d.asInt ==> None
    }

    test("apply(List) and Data.list round-trip through asList") {
      val viaApply = Data(List(Data(1), Data(2), Data(3)))
      val viaList = Data.list(Data(1), Data(2), Data(3))
      viaApply.asList ==> Some(List(Data(1), Data(2), Data(3)))
      viaList.asList ==> Some(List(Data(1), Data(2), Data(3)))
      viaApply ==> viaList
      viaApply.asMap ==> None
    }

    test("apply(Map) and Data.map round-trip through asMap") {
      val viaApply = Data(Map("a" -> Data(1), "b" -> Data(2)))
      val viaMap = Data.map("a" -> Data(1), "b" -> Data(2))
      viaApply.asMap.map(_.toMap) ==> Some(Map("a" -> Data(1), "b" -> Data(2)))
      viaMap.asMap.map(_.toMap) ==> Some(Map("a" -> Data(1), "b" -> Data(2)))
      viaApply.asList ==> None
    }

    test("Data.map should preserve insertion order (ListMap)") {
      val d = Data.map("z" -> Data(1), "a" -> Data(2), "m" -> Data(3))
      d.asMap.get.keys.toList ==> List("z", "a", "m")
    }
  }

  group("Data.get / getOrElse") {

    test("get(key) should return the value for a present key on a map") {
      val d = Data.map("a" -> Data(1), "b" -> Data(2))
      d.get("a") ==> Some(Data(1))
      d.get("missing") ==> None
    }

    test("get(key) should return None on a non-map") {
      Data(1).get("a") ==> None
    }

    test("getOrElse(key, default) should fall back when key is absent or value is not a map") {
      val d = Data.map("a" -> Data(1))
      d.getOrElse("a", Data(0)) ==> Data(1)
      d.getOrElse("missing", Data(99)) ==> Data(99)
      Data(1).getOrElse("a", Data(99)) ==> Data(99)
    }

    test("get(index) should index into a list and return None out of bounds / on non-list") {
      val d = Data.list(Data(10), Data(20), Data(30))
      d.get(0) ==> Some(Data(10))
      d.get(2) ==> Some(Data(30))
      d.get(3) ==> None
      d.get(-1) ==> None
      Data(1).get(0) ==> None
    }

    test("getOrElse(index, default) should fall back out of bounds") {
      val d = Data.list(Data(10), Data(20))
      d.getOrElse(0, Data(0)) ==> Data(10)
      d.getOrElse(5, Data(-1)) ==> Data(-1)
    }
  }

  group("Data update combinators") {

    test("updateAs* should transform when the type matches") {
      Data(1).updateAsInt(_ + 1) ==> Data(2)
      Data(1L).updateAsLong(_ + 1) ==> Data(2L)
      Data(1.5f).updateAsFloat(_ * 2) ==> Data(3.0f)
      Data(1.5).updateAsDouble(_ * 2) ==> Data(3.0)
      Data(true).updateAsBoolean(!_) ==> Data(false)
      Data("a").updateAsString(_ + "b") ==> Data("ab")
      Data.list(Data(1)).updateAsList(_ :+ Data(2)) ==> Data.list(Data(1), Data(2))
      Data.map("a" -> Data(1)).updateAsMap(_ + ("b" -> Data(2))) ==> Data.map("a" -> Data(1), "b" -> Data(2))
    }

    test("updateAs* should be a no-op when the type does not match") {
      Data("not an int").updateAsInt(_ + 1) ==> Data("not an int")
      Data(1).updateAsString(_ + "x") ==> Data(1)
      Data(1).updateAsList(_ => Nil) ==> Data(1)
    }
  }

  group("Data.fold") {

    test("fold should dispatch to the branch matching the underlying value") {
      def tag(d: Data): String = d.fold(
        onNull = "null",
        onInt = i => s"int:$i",
        onLong = l => s"long:$l",
        onFloat = f => s"float:$f",
        onDouble = dd => s"double:$dd",
        onBoolean = b => s"bool:$b",
        onString = s => s"str:$s",
        onList = l => s"list:${l.size}",
        onMap = m => s"map:${m.size}"
      )
      tag(Data()) ==> "null"
      tag(Data(1)) ==> "int:1"
      tag(Data(1L)) ==> "long:1"
      tag(Data(1.0f)) ==> "float:1.0"
      tag(Data(1.0)) ==> "double:1.0"
      tag(Data(true)) ==> "bool:true"
      tag(Data("s")) ==> "str:s"
      tag(Data.list(Data(1), Data(2))) ==> "list:2"
      tag(Data.map("a" -> Data(1))) ==> "map:1"
    }
  }

  group("Data equality") {

    test("structurally equal Data should be equal regardless of construction path") {
      Data(1) ==> Data(1)
      Data.list(Data(1), Data(2)) ==> Data(List(Data(1), Data(2)))
      Data.map("a" -> Data(1)) ==> Data(ListMap("a" -> Data(1)))
    }

    test("Data of genuinely different shapes should not be equal") {
      assert(Data(1) != Data("1"))
      assert(Data(1) != Data())
      assert(Data.list(Data(1)) != Data.map("0" -> Data(1)))
    }

    test("numeric Data follows Scala's universal equality (cross-width numbers compare equal)") {
      // Data does no type-tagging: it stores plain boxed JVM values, so equality is Scala's `==`,
      // which widens numerics. Int 1 and Long 1 therefore compare equal (and so produce no diff).
      assert(Data(1) == Data(1L))
      Data(1).diff(Data(1L)) ==> Nil
      // Distinct numeric values still differ.
      assert(Data(1) != Data(2L))
    }
  }

  group("Data.parseString") {

    test("should parse integers and longs") {
      Data.parseString("42") ==> Right(Data(42))
      Data.parseString("-7") ==> Right(Data(-7))
      Data.parseString("42L") ==> Right(Data(42L))
    }

    test("should parse floats and doubles") {
      Data.parseString("3.14f") ==> Right(Data(3.14f))
      Data.parseString("2.71828") ==> Right(Data(2.71828))
      Data.parseString("-1.5d") ==> Right(Data(-1.5))
    }

    test("should parse explicitly quoted strings by stripping the quotes") {
      Data.parseString("\"hello\"") ==> Right(Data("hello"))
      Data.parseString("\"\"") ==> Right(Data(""))
    }

    test("should parse booleans case-insensitively") {
      Data.parseString("true") ==> Right(Data(true))
      Data.parseString("TRUE") ==> Right(Data(true))
      Data.parseString("False") ==> Right(Data(false))
    }

    test("should fall back to a plain string for anything else") {
      Data.parseString("hello") ==> Right(Data("hello"))
      Data.parseString("not a number 123x") ==> Right(Data("not a number 123x"))
    }

    test("should trim surrounding whitespace before classifying") {
      Data.parseString("  42  ") ==> Right(Data(42))
      Data.parseString("  true ") ==> Right(Data(true))
    }
  }

  group("Data.parseList") {

    test("should parse flat key=value pairs into a map") {
      Data.parseList(List("a=1", "b=true", "c=\"x\"")) ==> Right(
        Data.map("a" -> Data(1), "b" -> Data(true), "c" -> Data("x"))
      )
    }

    test("should parse dotted keys into nested maps") {
      Data.parseList(List("a.b.c=1", "a.b.d=2", "a.e=3")) ==> Right(
        Data.map(
          "a" -> Data.map(
            "b" -> Data.map("c" -> Data(1), "d" -> Data(2)),
            "e" -> Data(3)
          )
        )
      )
    }

    test("should ignore entries that are not key=value pairs") {
      Data.parseList(List("a=1", "garbage", "b=2")) ==> Right(
        Data.map("a" -> Data(1), "b" -> Data(2))
      )
    }

    test("should report an error when a key has both a leaf value and nested values") {
      val result = Data.parseList(List("a=1", "a.b=2"))
      assert(result.isLeft, s"Expected Left, got $result")
      result.left.toOption.get.contains("a") ==> true
    }
  }

  group("Data.render") {

    test("should render scalars with type-disambiguating suffixes") {
      Data().render ==> "null"
      Data(1).render ==> "1"
      Data(1L).render ==> "1L"
      Data(1.0f).render ==> "1.0f"
      Data(1.0).render ==> "1.0"
      Data(true).render ==> "true"
    }

    test("should render strings quoted and escape inner quotes") {
      Data("hello").render ==> "\"hello\""
      Data("a\"b").render ==> "\"a\\\"b\""
    }

    test("should render empty list and map compactly") {
      Data.list().render ==> "[]"
      Data.map().render ==> "{}"
    }

    test("should render non-empty list with indentation") {
      Data.list(Data(1), Data(2)).render ==> "[\n  1,\n  2\n]"
    }

    test("should render non-empty map with indentation") {
      Data.map("a" -> Data(1), "b" -> Data(2)).render ==> "{\n  a: 1,\n  b: 2\n}"
    }
  }

  group("Data.diff (the engine behind <==>)") {

    test("equal Data should produce an empty diff") {
      Data(1).diff(Data(1)) ==> Nil
      Data.map("a" -> Data(1)).diff(Data.map("a" -> Data(1))) ==> Nil
    }

    test("differing scalars should produce a single root entry") {
      // Note: DataOps.diff is `actual.diff(expected)`, so the receiver is `actual`.
      val diff = Data(2).diff(Data(1))
      diff.size ==> 1
      diff.head.path ==> Nil
      diff.head.expected ==> Data(1)
      diff.head.actual ==> Data(2)
    }

    test("differing map values should carry a Key path segment") {
      val diff = Data.map("a" -> Data(2)).diff(Data.map("a" -> Data(1)))
      diff.size ==> 1
      diff.head.path ==> List(DiffEntry.Key("a"))
      diff.head.renderedPath ==> "a"
    }

    test("missing / extra map keys should be reported on both sides") {
      // expected has key b, actual has key a -> two entries.
      val diff = Data.map("a" -> Data(1)).diff(Data.map("b" -> Data(1)))
      diff.map(_.renderedPath).toSet ==> Set("a", "b")
    }

    test("differing list elements should carry an Index path segment") {
      val diff = Data.list(Data(1), Data(9)).diff(Data.list(Data(1), Data(2)))
      diff.size ==> 1
      diff.head.path ==> List(DiffEntry.Index(1))
      diff.head.renderedPath ==> "[1]"
      diff.head.expected ==> Data(2)
      diff.head.actual ==> Data(9)
    }

    test("lists of different lengths should diff the missing tail against empty Data") {
      val diff = Data.list(Data(1)).diff(Data.list(Data(1), Data(2)))
      diff.size ==> 1
      diff.head.path ==> List(DiffEntry.Index(1))
      diff.head.expected ==> Data(2)
      diff.head.actual ==> Data()
    }

    test("nested differences should accumulate a multi-segment path") {
      val actual = Data.map("outer" -> Data.list(Data.map("k" -> Data(2))))
      val expected = Data.map("outer" -> Data.list(Data.map("k" -> Data(1))))
      val diff = actual.diff(expected)
      diff.size ==> 1
      diff.head.path ==> List(DiffEntry.Key("outer"), DiffEntry.Index(0), DiffEntry.Key("k"))
      diff.head.renderedPath ==> "outer[0].k"
    }
  }

  group("DiffEntry") {

    test("prependKey / prependIndex should build the path front-to-back") {
      val e = DiffEntry(expected = Data(1), actual = Data(2)).prependKey("k").prependIndex(3).prependKey("root")
      e.path ==> List(DiffEntry.Key("root"), DiffEntry.Index(3), DiffEntry.Key("k"))
      e.renderedPath ==> "root[3].k"
    }

    test("ordering should sort by rendered path") {
      val a = DiffEntry(Data(1), Data(2)).prependKey("a")
      val b = DiffEntry(Data(1), Data(2)).prependKey("b")
      List(b, a).sorted ==> List(a, b)
    }
  }
}
