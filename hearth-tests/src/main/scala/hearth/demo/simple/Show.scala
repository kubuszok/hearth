// We've put things into a separate package and do not use:
//   package hearth
//   package demo
//   package simple
// here, because we want to show all the imports normal users would have to do.
package hearth.demo.simple

/** Example of Show type class with sanely-automatic derivation.
  *
  * It showcases how one can generate fast code, without taxing the compiler, with easy error handling, WITHOUT repying
  * on tons of implicit defs in the companion object, nor heavy use of semi-automatic derivation: derive only for the
  * outermost type and macro will handle everything internally!
  *
  * @see
  *   [[https://kubuszok.com/2025/sanely-automatic-derivation/]] for blog post explaining the concept
  * @see
  *   [[https://www.youtube.com/watch?v=M54ux51H6Fo]] for video explaining the concept
  */
trait Show[A] extends Show.AutoDerived[A] {

  def show(value: A): String
}

object Show extends ShowCompanionCompat {

  def apply[A](implicit show: Show.AutoDerived[A]): Show[A] = show match {
    case show: Show[A] => show
  }

  /** Pre 2.13.17 and 3.7.0 we need this trick to avoid summoning the implicit value automatically.
    *
    * @see
    *   [[hearth.demo.sanely_automatic.Show]] for version which does not need this trick!
    * @see
    *   [[hearth.demo.allfeatures.FastShowPretty]] for version which NOT only does not need this trick, but also
    *   utilizes every feature of this library!
    */
  sealed trait AutoDerived[A] {

    def show(value: A): String
  }

  /** Special type - if its implicit is in scope then macros will log the derivation process.
    *
    * @see
    *   [[hearth.demo.debug.logDerivation]] for details
    */
  sealed trait LogDerivation
  object LogDerivation extends LogDerivation
}
