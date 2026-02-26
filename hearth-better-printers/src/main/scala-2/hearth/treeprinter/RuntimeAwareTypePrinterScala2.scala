package hearth
package treeprinter

trait RuntimeAwareTypePrinterScala2 {
  val c: scala.reflect.macros.blackbox.Context

  import c.universe.*

  /** Builds an `Expr[String]` that produces a type name at runtime, substituting overridden type arguments with
    * runtime-provided expressions while keeping the rest identical to the output of `printType`.
    *
    * @param tpe
    *   the compiler type to print
    * @param overrideFn
    *   returns `Some(expr)` when a type should be replaced by a runtime expression
    * @param printType
    *   fallback printer (e.g. `showCodePretty(_, SyntaxHighlight.plain)`)
    * @since 0.3.0
    */
  def runtimeAwareTypePrint(
      tpe: Type,
      overrideFn: Type => Option[c.Expr[String]],
      printType: Type => String
  ): c.Expr[String] = runtimeAwarePrinterImpl.print(tpe, overrideFn, printType)

  private object runtimeAwarePrinterImpl {

    def print(
        tpe: Type,
        overrideFn: Type => Option[c.Expr[String]],
        printType: Type => String
    ): c.Expr[String] =
      overrideFn(tpe) match {
        case Some(expr) => expr
        case None       =>
          tpe match {
            case TypeRef(_, _, args) if args.nonEmpty && hasOverrideInTree(args, overrideFn) =>
              val tyconStr = printType(tpe).takeWhile(_ != '[')
              val argExprs = args.map(arg => print(arg, overrideFn, printType))
              val parts =
                c.Expr[String](Literal(Constant(tyconStr + "["))) ::
                  intersperse(argExprs, c.Expr[String](Literal(Constant(", ")))) :::
                  c.Expr[String](Literal(Constant("]"))) :: Nil
              concatExprs(optimizeLiterals(parts))
            case _ =>
              c.Expr[String](Literal(Constant(printType(tpe))))
          }
      }

    private def hasOverrideInTree(types: List[Type], overrideFn: Type => Option[c.Expr[String]]): Boolean =
      types.exists { tpe =>
        overrideFn(tpe).isDefined || (tpe match {
          case TypeRef(_, _, args) if args.nonEmpty => hasOverrideInTree(args, overrideFn)
          case _                                    => false
        })
      }

    private def intersperse[A](list: List[A], sep: A): List[A] = list match {
      case Nil      => Nil
      case x :: Nil => x :: Nil
      case x :: xs  => x :: sep :: intersperse(xs, sep)
    }

    private def optimizeLiterals(exprs: List[c.Expr[String]]): List[c.Expr[String]] =
      exprs.foldRight(List.empty[c.Expr[String]]) { (expr, acc) =>
        (expr.tree, acc) match {
          case (Literal(Constant(s1: String)), head :: rest) =>
            head.tree match {
              case Literal(Constant(s2: String)) =>
                c.Expr[String](Literal(Constant(s1 + s2))) :: rest
              case _ => expr :: acc
            }
          case _ => expr :: acc
        }
      }

    private def concatExprs(exprs: List[c.Expr[String]]): c.Expr[String] = exprs match {
      case Nil      => c.Expr[String](Literal(Constant("")))
      case x :: Nil => x
      case x :: xs  => xs.foldLeft(x)((acc, next) => c.Expr[String](q"${acc.tree} + ${next.tree}"))
    }
  }
}
