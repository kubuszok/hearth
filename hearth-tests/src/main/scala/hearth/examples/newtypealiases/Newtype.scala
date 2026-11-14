package hearth
package examples
package newtypealiases

trait AliasesInTrait {
  type Newtype[+A] = Newtype.Type[A]
}

object Newtype {
  type Base
  trait Tag extends Any
  type Type[+A] <: hearth.examples.intersections.And[Base, Tag]
}
