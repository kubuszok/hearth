package hearth
package fp
package data

final case class NonEmptyVector[A](head: A, tail: Vector[A])
