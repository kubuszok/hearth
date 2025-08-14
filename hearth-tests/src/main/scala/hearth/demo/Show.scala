// We've put things into a separate package and do not use:
//   package hearth
//   package demo
// here, because we want to show all the imports normal users would have to do.
package hearth.demo

/** Example of Show type class with sanely-automatic derivation.
  *
  * It showcases how one can generate fast code, without taxing the compiler, with easy error handling, WITHOUT repying
  * on tons of implicit defs in the companion object, nor heavy use of semi-automatic derivation: derive only for the
  * outermost type and macro will handle everything internally!
  *
  * @see
  *   [[https://www.youtube.com/watch?v=M54ux51H6Fo]] for details
  */
trait Show[A] extends Show.AutoDerived[A] {

  def show(value: A): String
}

object Show extends ShowCompanionCompat {

  def apply[A](implicit show: Show.AutoDerived[A]): Show[A] = show match {
    case show: Show[A] => show
  }

  // TODO: Replace summon with summonIgnoring when 2.13.17 is released, and we will be able to remove this.
  trait AutoDerived[A] {

    def show(value: A): String
  }
}
