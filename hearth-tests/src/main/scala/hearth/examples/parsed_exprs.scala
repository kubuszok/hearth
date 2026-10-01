package hearth
package examples
package parsed_exprs

case class Address(street: String, city: String)
case class Person(name: String, age: Int, address: Address)

case class Container(items: List[String], nested: Option[Address])

sealed trait Animal
case class Dog(name: String) extends Animal
case class Cat(name: String) extends Animal
case class AnimalHolder(animal: Animal)

trait PathEvidence[F[_]]
object PathEvidence {
  implicit val forList: PathEvidence[List] = new PathEvidence[List] {}
  implicit val forOption: PathEvidence[Option] = new PathEvidence[Option] {}
}

object dsl {

  implicit class EachOps[F[_], A](private val fa: F[A]) extends AnyVal {
    def each(implicit ev: PathEvidence[F]): A = throw new NotImplementedError
  }

  implicit class WhenOps[A](private val a: A) extends AnyVal {
    def when[B <: A]: B = throw new NotImplementedError
    // Monocle-style prism alias: focuses on a subtype.
    def as[B <: A]: B = throw new NotImplementedError
  }

  // quicklens-style Option focus: `.some` narrows `Option[A]` to `A`.
  implicit class SomeOps[A](private val oa: Option[A]) extends AnyVal {
    def some: A = throw new NotImplementedError
  }
}

/** A block-shaped DSL in the style of a parser generator:
  * `g => { import g._; val x = nonTerminal[Int]; x ::= ...; x }`. Reading it requires local `val` definitions,
  * references to them, and `import` statements.
  */
object grammar_dsl {

  final class Sym[A]

  final class Dsl {
    def nonTerminal[A]: Sym[A] = new Sym[A]
    def terminal(pattern: String): Sym[String] = { val _ = pattern; new Sym[String] }

    implicit class SymOps[A](val sym: Sym[A]) {
      def ::=(alternative: Any): Unit = { val _ = (sym, alternative); () }
    }
  }
}

trait Ctx
object Ctx {
  val instance: Ctx = new Ctx {}
}
