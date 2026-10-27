package hearth
package examples
package methods

class WithGivens {
  given givenValue: Int = 42
  def methodWithUsingParam(using x: Int): String = x.toString
}

class WithUsingClauses {
  def curriedUsing(a: Int)(using b: Int): Int = a + b
}
