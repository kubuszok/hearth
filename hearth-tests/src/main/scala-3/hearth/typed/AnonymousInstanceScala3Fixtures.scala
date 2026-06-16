package hearth
package typed

import scala.quoted.*

/** Scala 3-only AnonymousInstance fixtures for shapes that cannot be expressed in shared (Scala 2 + 3) sources — here,
  * a context-function return type (`Int ?=> String`).
  */
final private class AnonymousInstanceScala3Fixtures(q: Quotes) extends MacroCommonsScala3(using q) {

  def testContextFunctionReturn: Expr[String] =
    AnonymousInstance.parse(using Type.of[examples.anonymous_instances.TraitWithContextFunctionReturn]) match {
      case ClassViewResult.Compatible(ai) =>
        val overrides: Map[UntypedMethod, OverrideBody] = ai.mustOverride.map { cm =>
          cm.method.asUntyped -> new OverrideBody {
            def apply(ctx: OverrideContext): Expr_?? = {
              val kE = ctx.parameters.head
              import kE.Underlying as K
              val k: Expr[K] = kE.value
              // `build(k): Int ?=> String` — the override must return a context function, NOT a method taking an
              // extra `using Int`. Build `(i: Int) ?=> k.toString + i`.
              val cf: Expr[Int ?=> String] = '{ (i: Int) ?=> $k.toString + i.toString }
              cf.as_??(using Type.of[Int ?=> String])
            }
          }
        }.toMap
        ai.construct(None, Map.empty, overrides) match {
          case Right(constructed) =>
            val inst = constructed.asInstanceOf[Expr[examples.anonymous_instances.TraitWithContextFunctionReturn]]
            '{ $inst.build("k")(using 7) }
          case Left(errors) => Expr(s"errors: ${errors.toVector.mkString("; ")}")
        }
      case ClassViewResult.Incompatible(reason) => Expr(s"incompatible: $reason")
    }
}

object AnonymousInstanceScala3Fixtures {

  inline def testContextFunctionReturn: String = ${ testContextFunctionReturnImpl }
  private def testContextFunctionReturnImpl(using q: Quotes): Expr[String] =
    new AnonymousInstanceScala3Fixtures(q).testContextFunctionReturn
}
