// We've put things into a separate package and do not use:
//   package hearth
//   package demo
// here, because we want to show all the imports normal users would have to do.
package hearth.demo

package object debug {

  /** Import [[Show.LogDerivation]] in the scope to preview how the derivation is done.
    *
    * Put outside of [[Show]] companion to prevent the implicit from being summoned automatically!
    */
  implicit val logDerivation: Show.LogDerivation = Show.LogDerivation()
}
