package hearth.results

final case class NonEmptyVector[A](head: A, tail: Vector[A])
