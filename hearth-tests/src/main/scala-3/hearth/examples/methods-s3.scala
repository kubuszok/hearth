package hearth
package examples
package methods

class WithGivens {
  given givenValue: Int = 42
  def methodWithUsingParam(using x: Int): String = x.toString
}
