package hearth
package examples

object intersections {

  // Scala 2 spells an intersection type with `with`, which Scala 3.10+ no longer accepts in type position.
  type And[A, B] = A with B
}
