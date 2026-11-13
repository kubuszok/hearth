package hearth
package examples
package brokenaccessors

import hearth.data.Data
import hearth.examples.exprsedge.NamedArgsConfig

/** An inline method of a trait calling a macro of a `private[pkg]` object from a subpackage makes Scala 3 emit inline
  * accessors taking that subpackage (`internal`) as a parameter. It erases to a class that does not exist, so Java
  * reflection listing the methods of a class mixing the trait in (`Class#getMethods`) throws `NoClassDefFoundError`.
  */
private[brokenaccessors] trait HolderCompat { this: Holder.type =>

  inline def hiddenMacro: Unit = ${ internal.HiddenMacros.unitImpl }

  inline def semiEvalOfDefaults(flag: Boolean = true, size: Int = 2): Data =
    semiEvalOfConfig(using NamedArgsConfig(flag, size))

  inline def semiEvalOfConfig(using config: NamedArgsConfig): Data =
    hearth.typed.ExprsFixtures.testSemiEvalThrowable(config)
}

object Holder extends HolderCompat
