package hearth
package crossquotes

import hearth.data.Data

/** Quote bodies referencing standard-library, Java and third-party-package names. At the expansion site the enclosing
  * scope defines members called `scala`, `java`, `hearth` and `hygienepkg`, so any non-`_root_`-anchored path printed
  * by the (Scala 2) Cross-Quotes would be captured by them.
  */
trait CrossQuotesHygieneFixturesImpl { this: MacroCommons =>

  def testQuoteHygiene[A: Type](value: Expr[A]): Expr[Data] = Expr.quote {
    val some: Option[A] = Some(Expr.splice(value))
    val list: List[Int] = List(1, 2, 3)
    val none: Option[String] = Option.empty[String]
    val vector: scala.collection.immutable.Vector[Int] = scala.collection.immutable.Vector(4)
    val uuid: java.util.UUID = java.util.UUID.fromString("00000000-0000-0000-0000-000000000000")
    val thing: hygienepkg.HygieneThing = hygienepkg.HygieneThing.make(5)
    Data.map(
      "some" -> Data(some.toString),
      "list" -> Data(list.toString),
      "none" -> Data(none.toString),
      "vector" -> Data(vector.toString),
      "uuid" -> Data(uuid.toString),
      "thing" -> Data(thing.toString)
    )
  }
}
