package hearth
package typename

private[typename] trait TypeNameMacros { this: MacroCommons =>

  private lazy val typeNameCtor: Type.Ctor1[TypeName] = Type.Ctor1.of[TypeName]

  /** `TypeName.derived` will be ignored — so this macro will never summon itself! */
  private lazy val ignoredImplicits: Seq[UntypedMethod] =
    Type
      .of[TypeName.type]
      .asUntyped
      .methods
      .collect { case method if method.name == "derived" => method }
      .toSeq

  /** `summonImplicitIgnoring` requires Scala 2.13.17+ (`c.inferImplicitValueIgnoring`) or Scala 3.7.0+
    * (`Implicits.searchIgnoring`). On older versions, summoning is disabled entirely because `summonImplicit` would
    * find `TypeName.derived` and cause infinite recursion.
    */
  private lazy val canSummonIgnoring: Boolean = {
    val sv = Environment.currentScalaVersion
    (sv.isScala2_13 && sv.patch >= 17) || (sv.isScala3 && sv.minor >= 7)
  }

  private var cache: ValDefsCache = ValDefsCache.empty

  /** Try to summon `TypeName[X]` (ignoring our own `derived`), caching found instances as lazy vals.
    *
    * Returns `None` on Scala 2.13.16 and earlier (no safe way to summon without infinite recursion).
    */
  private def summonCachedTypeName[X: Type]: Option[Expr[TypeName[X]]] =
    if (!canSummonIgnoring) None
    else {
      implicit val typeNameX: Type[TypeName[X]] = typeNameCtor.apply[X]
      cache.get0Ary[TypeName[X]]("typeName").orElse {
        Expr.summonImplicitIgnoring[TypeName[X]](ignoredImplicits*).toOption.map { expr =>
          val builder = ValDefBuilder.ofLazy[TypeName[X]]("typeName")
          cache = builder.buildCachedWith(cache, "typeName")(_ => expr)
          cache.get0Ary[TypeName[X]]("typeName").get
        }
      }
    }

  def deriveTypeName[A: Type]: Expr[TypeName[A]] = {
    cache = ValDefsCache.empty

    val pp = Type.runtimePrettyPrint[A] { tpe =>
      import tpe.Underlying
      summonCachedTypeName[tpe.Underlying].map { tn =>
        Expr.quote(Expr.splice(tn).prettyPrint)
      }
    }
    val pl = Type.runtimePlainPrint[A] { tpe =>
      import tpe.Underlying
      summonCachedTypeName[tpe.Underlying].map { tn =>
        Expr.quote(Expr.splice(tn).plainPrint)
      }
    }
    val si = Type.runtimeShortPrint[A] { tpe =>
      import tpe.Underlying
      summonCachedTypeName[tpe.Underlying] match {
        case Some(tn) => Some(Expr.quote(Expr.splice(tn).simplePrint))
        case None     =>
          // For simplePrint, we always want to decompose applied types and show short names.
          // runtimeShortPrint only decomposes when it finds overrides in the tree, so we provide
          // the short name for leaf types (no type args) to ensure the tree is always decomposed.
          val plain = Type.plainPrint[tpe.Underlying]
          if (plain.contains("[")) None // applied type — let the printer recursively decompose
          else Some(Expr(Type.shortName[tpe.Underlying])) // leaf — provide short name
      }
    }
    val sh = Expr(Type.shortName[A])

    cache.toValDefs.use { _ =>
      Expr.quote {
        val prettyPrintVal = Expr.splice(pp)
        val plainPrintVal = Expr.splice(pl)
        val simplePrintVal = Expr.splice(si)
        val shortPrintVal = Expr.splice(sh)
        new TypeName[A] {
          def prettyPrint: String = prettyPrintVal
          def plainPrint: String = plainPrintVal
          def simplePrint: String = simplePrintVal
          def shortPrint: String = shortPrintVal
        }
      }
    }
  }
}
