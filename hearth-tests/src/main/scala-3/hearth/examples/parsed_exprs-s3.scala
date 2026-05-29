package hearth
package examples
package parsed_exprs

object dslContextFunctions {

  extension [F[_], A](fa: F[A])(using PathEvidence[F]) {
    def eachCF: A = throw new NotImplementedError
  }

  extension [A](a: A) {
    def whenCF[B <: A]: B = throw new NotImplementedError
  }
}
