package hearth

package object results {
  
  type Logs = Vector[Log]

  type Errors = NonEmptyVector[Throwable]
}
