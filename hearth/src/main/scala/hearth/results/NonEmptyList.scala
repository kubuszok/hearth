package hearth.results

final case class NonEmptyList[A](head: A, tail: List[A])
