package hearth
package examples
package brokenaccessors
package internal

import scala.quoted.*

private[brokenaccessors] object HiddenMacros {

  def unitImpl(using Quotes): Expr[Unit] = '{ () }
}
