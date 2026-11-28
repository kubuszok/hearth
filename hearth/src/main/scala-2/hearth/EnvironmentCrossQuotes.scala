package hearth

trait EnvironmentCrossQuotesSupport { this: Environments =>

  private[hearth] trait CrossQuotesSupport { this: CrossQuotes.type =>

    /** Strips free type symbols from expression trees returned by `Expr.splice` workaround methods.
      *
      * When `Expr.splice` inside `Expr.quote` references local type params (e.g. `A`, `B` from `def map[A, B]`), the
      * cross-quotes system wraps the splice body in a workaround method that receives those type params as
      * `WeakTypeTag`-based `Type` values created from free type symbols (`newFreeType("A")`).
      *
      * Hearth utilities (`CaseClass`, `Method`, `LambdaBuilder`, etc.) may use these types to generate expression
      * trees. Those trees contain `TypeTree` nodes whose `.tpe` references the free type symbols. The Scala 2 compiler
      * rejects any macro expansion containing free type symbols.
      *
      * This method walks the tree and replaces `TypeTree` nodes containing free types with equivalent '''untyped'''
      * type tree representations (using `Ident(TypeName("A"))` for free types). The compiler then resolves these by
      * name to the quasiquote's own type params during typechecking.
      *
      * Takes and returns an `Expr` of the current macro context ([[ctx]]).
      *
      * @since 0.4.6
      */
    def stripFreeTypes(expr: Any): Any = {
      val c = ctx[scala.reflect.macros.blackbox.Context]
      stripFreeTypesIn(c)(expr.asInstanceOf[c.Expr[Any]])
    }

    /** Kept for macros compiled against the previous releases, whose generated code calls it.
      *
      * @since 0.3.0
      */
    @deprecated("Use stripFreeTypes(expr), which takes the context from CrossQuotes.ctx", "0.4.6")
    def stripFreeTypes(ctx: scala.reflect.macros.blackbox.Context)(expr: ctx.Expr[Any]): ctx.Expr[Any] =
      stripFreeTypesIn(ctx)(expr)

    private def stripFreeTypesIn(
        ctx: scala.reflect.macros.blackbox.Context
    )(expr: ctx.Expr[Any]): ctx.Expr[Any] = {
      import ctx.universe.*
      val tree = expr.tree

      // isFreeType is not in the public blackbox.Context API surface, so we use Java reflection.
      def isFree(sym: Symbol): Boolean =
        try sym.getClass.getMethod("isFreeType").invoke(sym).asInstanceOf[java.lang.Boolean].booleanValue()
        catch { case _: Throwable => false }

      var hasFree = false
      tree.foreach {
        case tt: TypeTree if tt.tpe != null =>
          tt.tpe.foreach(tpe => if (isFree(tpe.typeSymbol)) hasFree = true)
        case _ => ()
      }
      if (!hasFree) return expr

      def typeContainsFreeType(tpe: Type): Boolean = {
        var found = false
        tpe.foreach(t => if (isFree(t.typeSymbol)) found = true)
        found
      }

      def pkgRef(sym: Symbol): Tree = {
        val parts = sym.fullName.split('.')
        parts.foldLeft[Tree](Ident(termNames.ROOTPKG)) { case (acc, part) =>
          Select(acc, TermName(part))
        }
      }

      def typeToTree(t: Type): Tree = t.dealias match {
        case TypeRef(pre, sym, args) =>
          val base: Tree = pre match {
            case NoPrefix                            => Ident(sym.name.toTypeName)
            case ThisType(pkg) if pkg.isPackageClass => Select(pkgRef(pkg), sym.name.toTypeName)
            case _                                   => Select(typeToTree(pre), sym.name.toTypeName)
          }
          if (args.isEmpty) base
          else AppliedTypeTree(base, args.map(typeToTree))
        case SingleType(pre, sym) =>
          pre match {
            case ThisType(pkg) if pkg.isPackageClass => Select(pkgRef(pkg), sym.name.toTermName)
            case _                                   => Select(typeToTree(pre), sym.name.toTermName)
          }
        case ThisType(sym) if sym.isPackageClass => pkgRef(sym)
        case ThisType(sym)                       => This(sym.name.toTypeName)
        case ConstantType(const)                 => Literal(const)
        case AnnotatedType(_, underlying)        => typeToTree(underlying)
        case ExistentialType(_, underlying)      => typeToTree(underlying)
        case other                               => TypeTree(other)
      }

      object stripper extends Transformer {
        override def transform(tree: Tree): Tree = tree match {
          case tt: TypeTree if tt.tpe != null && typeContainsFreeType(tt.tpe) =>
            typeToTree(tt.tpe)
          case _ => super.transform(tree)
        }
      }

      ctx.Expr[Any](stripper.transform(tree))
    }

    /** The arguments to put in place of `Expr.splice(seq)*` inside `Expr.quote`, as trees of the current macro context
      * ([[ctx]]).
      *
      * A sequence built with a standard library factory (`Seq(a, b)`, `List(a, b)`, also untyped, as [[VarArgs]] builds
      * it) is spliced as its elements; any other sequence (a `::` chain, a reference, a method call) is spliced as a
      * whole, as `seq: _*`.
      *
      * @since 0.4.6
      */
    def varArgsElements(seq: Any): List[Any] = {
      val c = ctx[scala.reflect.macros.blackbox.Context]
      import c.universe.*
      val factoryNames = Set("Seq", "List", "Vector", "IndexedSeq")
      def isStandardCollectionsPath(qualifier: Tree): Boolean =
        qualifier.toString.matches("(_root_\\.)?scala(\\.collection(\\.immutable)?)?")
      def isUntypedFactory(fun: Tree): Boolean = fun match {
        case Select(companion, TermName("apply")) => isUntypedFactory(companion)
        case Ident(TermName(name))                => factoryNames(name)
        case Select(qualifier, TermName(name))    => factoryNames(name) && isStandardCollectionsPath(qualifier)
        case _                                    => false
      }
      def isSeqFactoryApply(fun: Tree): Boolean = fun match {
        case TypeApply(inner, _) => isSeqFactoryApply(inner)
        case _                   =>
          val sym = fun.symbol
          if (sym == null || sym == NoSymbol) isUntypedFactory(fun)
          else
            sym.name == TermName("apply") && sym.owner.isModuleClass &&
            sym.owner.fullName.startsWith("scala.collection.") && sym.isMethod &&
            sym.asMethod.paramLists.flatten.exists(param =>
              definitions.RepeatedParamClass == param.typeSignature.typeSymbol
            )
      }
      seq.asInstanceOf[c.Expr[Any]].tree match {
        case Apply(fun, args) if isSeqFactoryApply(fun) => args
        case tree                                       => List(Typed(tree, Ident(typeNames.WILDCARD_STAR)))
      }
    }
  }
}
