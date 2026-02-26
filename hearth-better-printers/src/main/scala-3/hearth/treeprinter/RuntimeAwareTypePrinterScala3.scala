package hearth
package treeprinter

trait RuntimeAwareTypePrinterScala3 {
  val quotes: scala.quoted.Quotes

  import quotes.*, quotes.reflect.*

  /** Builds an `Expr[String]` that produces a type name at runtime, substituting overridden type arguments with
    * runtime-provided expressions while keeping the rest identical to the output of `printType`.
    *
    * @param tpe
    *   the compiler type representation to print
    * @param overrideFn
    *   returns `Some(expr)` when a type should be replaced by a runtime expression
    * @param printType
    *   fallback printer (e.g. `removeAnsiColors(prettyPrintRepr(...))`)
    * @since 0.8.0
    */
  def runtimeAwareTypePrint(
      tpe: TypeRepr,
      overrideFn: TypeRepr => Option[scala.quoted.Expr[String]],
      printType: TypeRepr => String
  ): scala.quoted.Expr[String] = implementationDetails.print(tpe, overrideFn, printType)

  /** Adding new val/lazy val/var to the trait is breaking backward compatibility in the bytecode (SIC!).
    *
    * Therefore we should avoid that when possible - for private fields, `private` modifier does NOT help, since the
    * compiler still needs to do some magic to allow the val to refer to other vals which can be overridden.
    *
    * Instead we can create a private object - adding it, would break backward compatibility, but if it already exists,
    * and it's not available to the user, it's fine to add things there when needed.
    */
  private object implementationDetails {

    private given scala.quoted.Quotes = quotes

    import scala.quoted.Expr as QExpr

    def dealiasAll(tpe: TypeRepr): TypeRepr =
      tpe match {
        case AppliedType(tycon, args) => AppliedType(dealiasAll(tycon), args.map(dealiasAll(_)))
        case _                        => tpe.dealias
      }

    def print(
        tpe: TypeRepr,
        overrideFn: TypeRepr => Option[QExpr[String]],
        printType: TypeRepr => String
    ): QExpr[String] =
      overrideFn(tpe) match {
        case Some(expr) => expr
        case None       =>
          dealiasAll(tpe) match {
            case AppliedType(_, args) if args.nonEmpty && hasOverrideInTree(args, overrideFn) =>
              val tyconStr = printType(tpe).takeWhile(_ != '[')
              val argExprs = args.map(arg => print(arg, overrideFn, printType))
              val parts: List[QExpr[String]] =
                QExpr(tyconStr + "[") ::
                  intersperse(argExprs, QExpr(", ")) :::
                  QExpr("]") :: Nil
              concatExprs(optimizeLiterals(parts))
            case _ =>
              QExpr(printType(tpe))
          }
      }

    private def hasOverrideInTree(types: List[TypeRepr], overrideFn: TypeRepr => Option[QExpr[String]]): Boolean =
      types.exists { tpe =>
        overrideFn(tpe).isDefined || (dealiasAll(tpe) match {
          case AppliedType(_, args) if args.nonEmpty => hasOverrideInTree(args, overrideFn)
          case _                                     => false
        })
      }

    private def intersperse[A](list: List[A], sep: A): List[A] = list match {
      case Nil      => Nil
      case x :: Nil => x :: Nil
      case x :: xs  => x :: sep :: intersperse(xs, sep)
    }

    private def tryExtractStringLiteral(expr: QExpr[String]): Option[String] =
      expr.asTerm match {
        case Literal(StringConstant(s)) => Some(s)
        case _                          => None
      }

    private def optimizeLiterals(exprs: List[QExpr[String]]): List[QExpr[String]] =
      exprs.foldRight(List.empty[QExpr[String]]) { (expr, acc) =>
        (tryExtractStringLiteral(expr), acc) match {
          case (Some(s1), head :: rest) =>
            tryExtractStringLiteral(head) match {
              case Some(s2) => QExpr(s1 + s2) :: rest
              case None     => expr :: acc
            }
          case _ => expr :: acc
        }
      }

    private def concatExprs(exprs: List[QExpr[String]]): QExpr[String] = exprs match {
      case Nil      => QExpr("")
      case x :: Nil => x
      case x :: xs  => xs.foldLeft(x)((acc, next) => '{ $acc + $next })
    }
  }
}
