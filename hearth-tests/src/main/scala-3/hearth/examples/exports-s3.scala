package hearth
package examples
package exports

// Scala 3-only fixtures for hearth#315: an `export`-created alias must classify the same as its underlying class
// (its `typeSymbol` is the alias, which carries none of the class flags, so flag checks must `dealias`).

object Inner {
  case class Foo(a: Int, b: String)
}

object Exported {
  export Inner.Foo
}
