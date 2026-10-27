package hearth
package examples
package exprsedge

/** Counts how many times an expression passed to a macro was evaluated at runtime. */
final class EvalCounter {
  var count: Int = 0
  def tick(): Int = {
    count += 1
    count
  }
}

/** Not a case class: only reachable through `new`. */
class PlainClass(val a: Int, val b: String) {
  override def toString(): String = s"PlainClass($a, $b)"
}

/** Values with explicit types are not constant-folded by the compiler, so operators on them reach `semiEval`. */
object PlainObject {
  val answer: Int = 42
  val flag: Boolean = true
  val ratio: Double = 1.5
  val big: Long = 10000000000L
  val letter: Char = 'a'
  override def toString(): String = "PlainObject"
}

case class Box[A](value: A)

case class WithCollections(names: List[String], maybe: Option[Int])

case class WithSealedField(label: String, shape: hearth.examples.expr_codecs.Shape)

sealed trait Arith
object Arith {
  case class Lit(value: Int) extends Arith
  case class Add(left: Arith, right: Arith) extends Arith
}

final class AmbiguousEvidence
object AmbiguousEvidence {
  implicit val first: AmbiguousEvidence = new AmbiguousEvidence
  implicit val second: AmbiguousEvidence = new AmbiguousEvidence
}

trait DivergingEvidence[A]
object DivergingEvidence {
  implicit def diverge[A](implicit next: DivergingEvidence[List[A]]): DivergingEvidence[A] = {
    val _ = next
    new DivergingEvidence[A] {}
  }
}

/** Renders runtime values the same way the macro renders decoded values, so both sides can be compared. */
object RuntimeShow {
  def show(value: Any): String = value match {
    case null                => "null"
    case array: Array[?]     => array.map(show).mkString("Array(", ", ", ")")
    case cls: java.lang.Class[?] => s"Class(${cls.getName})"
    // Rendered the same for every Map implementation, in iteration order, so that a reordering is visible.
    case map: scala.collection.Map[?, ?] =>
      map.map { case (k, v) => s"${show(k)} -> ${show(v)}" }.mkString("Map(", ", ", ")")
    case other               => other.toString
  }
}
