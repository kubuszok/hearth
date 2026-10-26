package hearth
package examples
package exports

// An `export`-created alias must classify the same as its underlying class
// (its `typeSymbol` is the alias, which carries none of the class flags, so flag checks must `dealias`).

object Inner {
  case class Foo(a: Int, b: String)
}

object Exported {
  export Inner.Foo
}
