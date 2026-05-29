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
  }
}
