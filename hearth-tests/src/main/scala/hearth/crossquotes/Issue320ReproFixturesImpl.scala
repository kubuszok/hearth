package hearth
package crossquotes

/** Fixtures for the hearth#320 regression: Scala 2 cross-unit quote hygiene.
  *
  * The quote below references a companion-object method (`CrossUnitWrapper.wrap`) and a plain object
  * (`CrossUnitSupport`) by their SHORT names. Because the fixture is compiled in package `hearth.crossquotes` while the
  * spec that triggers the expansion lives in package `hearth`, the reified Scala 2 source must fully-qualify those
  * object references (`_root_.hearth.crossquotes.CrossUnitWrapper.wrap`, ...) or they fail to resolve at the expansion
  * site (`value wrap is not a member of ...CrossUnitWrapper` / `not found: value CrossUnitSupport`).
  *
  * Scala 3 quotes are hygienic and already qualify these references, so the same test passes there unchanged.
  */
trait Issue320ReproFixturesImpl { this: MacroTypedCommons =>

  def testCrossUnitQuote(value: Expr[String]): Expr[String] = Expr.quote {
    CrossUnitSupport.describe(CrossUnitWrapper.wrap(Expr.splice(value)))
  }
}
