package hearth

import scala.collection.immutable.ListMap

trait Functions { this: MacroCommons =>

  type Parameters = List[ListMap[String, ??]]
  type Arguments = Map[String, Expr_??]

  type Function[Out]

  val Function: FunctionModule
  trait FunctionModule { this: Function.type =>

    def unapply[Out](function: Function[Out]): Some[(Parameters, Arguments => Expr[Out])]

    def parameters[Out](function: Function[Out]): Parameters
    def applyUnsafe[Out](function: Function[Out])(args: Arguments): Expr[Out]
  }

  implicit class FunctionMethods[Out](private val function: Function[Out]) {
    def parameters: Parameters = Function.parameters(function)
    def applyUnsafe(args: Arguments): Expr[Out] = Function.applyUnsafe(function)(args)
  }
}
