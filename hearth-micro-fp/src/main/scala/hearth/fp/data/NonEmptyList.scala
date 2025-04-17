package hearth
package fp
package data

final case class NonEmptyList[A](head: A, tail: List[A])
