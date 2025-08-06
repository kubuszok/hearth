// Here we do not use:
//   package hearth
//   package demo
// because we want to show all the imports normal users would have to do.
package hearth.demo

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
