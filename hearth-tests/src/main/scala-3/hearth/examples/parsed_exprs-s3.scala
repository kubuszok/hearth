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

object inlining {

  /** A non-inline parameter of an inline method is bound to a proxy `val` in the inlined call's bindings. */
  inline def twice(x: Int): Int = x + x
}
