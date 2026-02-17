package hearth
package crossquotes

/** Simple higher-kinded trait for testing Expr.quote with type constructor injection.
  *
  * Used by [[CrossCtorInjectionFixturesImpl.testExprQuoteWithCtor1Body]].
  */
trait Container[F[_]] {
  def value: F[String]
}
