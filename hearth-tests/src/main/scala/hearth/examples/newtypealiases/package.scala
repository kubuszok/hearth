package hearth
package examples

/** A newtype exposed through an alias that the package object inherits from a trait (as `cats.data.NonEmptyLazyList`
  * is), rather than one declared in the package object itself.
  */
package object newtypealiases extends newtypealiases.AliasesInTrait
