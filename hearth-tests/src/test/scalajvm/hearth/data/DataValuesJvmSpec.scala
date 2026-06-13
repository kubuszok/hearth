package hearth
package data

/** JVM-only `Data` tests that depend on runtime discrimination between numeric types (Int / Float / Double).
  *
  * `Data` stores plain boxed values and dispatches `fold` / `render` / `updateAs*` on the runtime class of the boxed
  * value. On the JVM `Int`, `Float` and `Double` box to distinct classes, so e.g. `Data(1)` renders as `"1"` while
  * `Data(1.0f)` renders as `"1.0f"`. On Scala.js (and, for the same reason, on Scala Native via JS-like number
  * semantics) all of these box to a single `Double`/`number`, so the int-vs-float-vs-double distinction does not exist
  * at runtime and these assertions cannot hold. The platform-independent behavior (null/Long/Boolean/String/List/Map
  * dispatch, accessors, parsing, diff) is covered cross-platform in [[DataValuesSpec]].
  */
final class DataValuesJvmSpec extends MacroSuite {

  group("Data update combinators (numeric, JVM-only)") {

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
  }

  group("Data.fold (numeric, JVM-only)") {

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

  group("Data.render (numeric, JVM-only)") {

    test("should render numeric scalars with type-disambiguating suffixes") {
      Data(1).render ==> "1"
      Data(1L).render ==> "1L"
      Data(1.0f).render ==> "1.0f"
      Data(1.0).render ==> "1.0"
    }
  }
}
