package hearth
package examples

object opaqueid {

  opaque type OpaqueId = Long

  object OpaqueId {
    def apply(v: Long): OpaqueId = v

    def fromString(s: String): Either[String, OpaqueId] =
      scala.util.Try(s.toLong).toEither.left.map(_.getMessage).map(apply)
  }

  extension (id: OpaqueId) {
    def value: Long = id
  }
}
