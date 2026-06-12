package hearth
package typed

trait ExprCodecDerivation { this: MacroCommons =>

  // -- semiQuote --

  protected def semiQuoteInternal[A: Type](
      value: A,
      overrides: UntypedType => Existential[QuoteOverride]
  ): Either[String, Expr[A]] = {
    val overrideResult =
      if (overrides == null) None
      else {
        val tpe = UntypedType.fromTyped[A]
        val existentialOverride = overrides(tpe)
        import existentialOverride.Underlying
        existentialOverride.value.map { f =>
          f(value.asInstanceOf[Underlying]).map(_.asInstanceOf[Expr[A]])
        }
      }

    overrideResult.getOrElse {
      lookupBuiltInExprCodec[A] match {
        case Some(codec) => Right(codec.asInstanceOf[ExprCodec[A]].toExpr(value))
        case None        =>
          SingletonValue.parse[A].toOption match {
            case Some(sv) => Right(sv.singletonExpr)
            case None     =>
              CaseClass.parse[A].toOption match {
                case Some(cc) => semiQuoteCaseClass(value, cc, overrides)
                case None     =>
                  Enum.parse[A].toOption match {
                    case Some(en) => semiQuoteEnum(value, en, overrides)
                    case None     => Left(s"Cannot semi-quote value of type ${Type.prettyPrint[A]}")
                  }
              }
          }
      }
    }
  }

  private def semiQuoteCaseClass[A: Type](
      value: A,
      cc: CaseClass[A],
      overrides: UntypedType => Existential[QuoteOverride]
  ): Either[String, Expr[A]] = {
    val product = value.asInstanceOf[Product]
    val fields = cc.caseFields

    val liftedResult = fields.zipWithIndex.foldLeft[Either[String, Map[String, Expr_??]]](Right(Map.empty)) {
      case (Left(err), _)             => Left(err)
      case (Right(acc), (field, idx)) =>
        field.knownReturning match {
          case None            => Left(s"Unknown return type for field ${field.name} of ${Type.prettyPrint[A]}")
          case Some(fieldType) =>
            import fieldType.Underlying as F
            val fieldValue = product.productElement(idx)
            semiQuoteInternal[F](fieldValue.asInstanceOf[F], overrides) match {
              case Right(fieldExpr) =>
                Right(acc + (field.name -> fieldExpr.as_??(using Expr.typeOf(fieldExpr))))
              case Left(err) => Left(err)
            }
        }
    }

    liftedResult.flatMap { liftedFields =>
      cc.construct[hearth.fp.Id] { param =>
        liftedFields(param.name)
      }(using hearth.fp.DirectStyle.DirectStyleForId, hearth.fp.instances.ParallelTraverseForId)
        .toRight(s"Failed to construct ${Type.prettyPrint[A]}")
    }
  }

  private def semiQuoteEnum[A: Type](
      value: A,
      en: Enum[A],
      overrides: UntypedType => Existential[QuoteOverride]
  ): Either[String, Expr[A]] = {
    val className = value.getClass.getSimpleName.stripSuffix("$")
    en.directChildren
      .collectFirst {
        case (name, childType) if name == className =>
          import childType.Underlying as C
          semiQuoteInternal[C](value.asInstanceOf[C], overrides).map(_.upcast[A])
      }
      .getOrElse(Left(s"No child for $className in ${Type.prettyPrint[A]}"))
  }

  // -- ExprCodec.derived --

  protected def trySummonExprCodec[F: Type](): Option[ExprCodec[F]]

  protected def deriveExprCodecInternal[A: Type]: ExprCodec[A] = {
    val (evalOverrides, quoteOverrides) = buildOverrideMaps[A]

    new ExprCodec[A] {
      def toExpr(value: A): Expr[A] =
        semiQuoteInternal(value, quoteOverrides) match {
          case Right(expr) => expr
          case Left(error) => assertionFailed(s"ExprCodec.derived[${Type.prettyPrint[A]}].toExpr failed: $error")
        }

      def fromExpr(expr: Expr[A]): Option[A] =
        Expr.semiEval(expr, evalOverrides).toOption
    }
  }

  private def buildOverrideMaps[A: Type]: (
      UntypedType => Existential[EvalOverride],
      UntypedType => Existential[QuoteOverride]
  ) = {
    val evalOverrides = new java.util.ArrayList[(UntypedType, Existential[EvalOverride])]()
    val quoteOverrides = new java.util.ArrayList[(UntypedType, Existential[QuoteOverride])]()
    val visited = new java.util.ArrayList[UntypedType]()
    val derivingFor = UntypedType.fromTyped[A]

    def alreadyVisited(tpe: UntypedType): Boolean = {
      var i = 0
      while (i < visited.size()) {
        if (visited.get(i) =:= tpe) return true
        i += 1
      }
      false
    }

    def traverse[F: Type](): Unit = {
      val tpe = UntypedType.fromTyped[F]
      if (alreadyVisited(tpe) || (tpe =:= derivingFor)) return
      if (lookupBuiltInExprCodec[F].isDefined) return

      visited.add(tpe)

      trySummonExprCodec[F]().foreach { codec =>
        evalOverrides.add(
          tpe -> Existential[EvalOverride, F](EvalOverride[F] { (expr: Expr[F]) =>
            codec.fromExpr(expr).toRight(s"ExprCodec[${Type.prettyPrint[F]}].fromExpr returned None")
          })
        )
        quoteOverrides.add(
          tpe -> Existential[QuoteOverride, F](QuoteOverride[F] { (value: F) =>
            Right(codec.toExpr(value))
          })
        )
      }

      CaseClass.parse[F].toOption.foreach { cc =>
        cc.caseFields.foreach { field =>
          field.knownReturning.foreach { fieldType =>
            import fieldType.Underlying as G
            traverse[G]()
          }
        }
      }

      Enum.parse[F].toOption.foreach { en =>
        en.directChildren.foreach { case (_, childType) =>
          import childType.Underlying as C
          traverse[C]()
        }
      }
    }

    traverse[A]()

    if (evalOverrides.isEmpty) return (null, null)

    val noEvalOverride: Existential[EvalOverride] =
      Existential[EvalOverride, Any](EvalOverride.none[Any])(using Type.of[Any])
    val noQuoteOverride: Existential[QuoteOverride] =
      Existential[QuoteOverride, Any](QuoteOverride.none[Any])(using Type.of[Any])

    def lookupEval(tpe: UntypedType): Existential[EvalOverride] = {
      var i = 0
      while (i < evalOverrides.size()) {
        val (k, v) = evalOverrides.get(i)
        if (k =:= tpe) return v
        i += 1
      }
      noEvalOverride
    }

    def lookupQuote(tpe: UntypedType): Existential[QuoteOverride] = {
      var i = 0
      while (i < quoteOverrides.size()) {
        val (k, v) = quoteOverrides.get(i)
        if (k =:= tpe) return v
        i += 1
      }
      noQuoteOverride
    }

    (lookupEval _, lookupQuote _)
  }

  private[hearth] def lookupBuiltInExprCodec[F: Type]: Option[ExprCodec[Any]] =
    if (Type[F] =:= Type.of[Null]) Some(Expr.NullExprCodec.asInstanceOf[ExprCodec[Any]])
    else if (Type[F] =:= Type.of[Unit]) Some(Expr.UnitExprCodec.asInstanceOf[ExprCodec[Any]])
    else if (Type[F] =:= Type.of[Boolean]) Some(Expr.BooleanExprCodec.asInstanceOf[ExprCodec[Any]])
    else if (Type[F] =:= Type.of[Byte]) Some(Expr.ByteExprCodec.asInstanceOf[ExprCodec[Any]])
    else if (Type[F] =:= Type.of[Short]) Some(Expr.ShortExprCodec.asInstanceOf[ExprCodec[Any]])
    else if (Type[F] =:= Type.of[Int]) Some(Expr.IntExprCodec.asInstanceOf[ExprCodec[Any]])
    else if (Type[F] =:= Type.of[Long]) Some(Expr.LongExprCodec.asInstanceOf[ExprCodec[Any]])
    else if (Type[F] =:= Type.of[Float]) Some(Expr.FloatExprCodec.asInstanceOf[ExprCodec[Any]])
    else if (Type[F] =:= Type.of[Double]) Some(Expr.DoubleExprCodec.asInstanceOf[ExprCodec[Any]])
    else if (Type[F] =:= Type.of[Char]) Some(Expr.CharExprCodec.asInstanceOf[ExprCodec[Any]])
    else if (Type[F] =:= Type.of[String]) Some(Expr.StringExprCodec.asInstanceOf[ExprCodec[Any]])
    else if (Type[F] =:= Type.of[BigInt]) Some(Expr.BigIntExprCodec.asInstanceOf[ExprCodec[Any]])
    else if (Type[F] =:= Type.of[BigDecimal]) Some(Expr.BigDecimalExprCodec.asInstanceOf[ExprCodec[Any]])
    else if (Type[F] =:= Type.of[hearth.data.Data]) Some(Expr.DataExprCodec.asInstanceOf[ExprCodec[Any]])
    else
      // Vararg case-class fields/parameters are normalized to scala.collection.immutable.Seq[A], so Seq of
      // built-in-codec elements has to be supported for vararg case classes to round-trip.
      seqTypeCtor.unapply(Type[F]).flatMap { elem =>
        import elem.Underlying as E
        if (Type[F] =:= seqTypeCtor[E])
          lookupBuiltInExprCodec[E].map { elemCodec =>
            implicit val elemExprCodec: ExprCodec[E] = elemCodec.asInstanceOf[ExprCodec[E]]
            Expr.SeqExprCodec[E].asInstanceOf[ExprCodec[Any]]
          }
        else None
      }

  private lazy val seqTypeCtor: Type.Ctor1[Seq] = Type.Ctor1.of[Seq]
}
