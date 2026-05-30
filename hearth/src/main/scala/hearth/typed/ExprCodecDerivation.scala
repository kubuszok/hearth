package hearth
package typed

trait ExprCodecDerivation { this: MacroCommons =>

  protected def deriveExprCodecInternal[A: Type]: ExprCodec[A] =
    SingletonValue.parse[A].toOption match {
      case Some(sv) => deriveSingletonCodec[A](sv)
      case None =>
        CaseClass.parse[A].toOption match {
          case Some(cc) => deriveCaseClassCodec[A](cc)
          case None =>
            Enum.parse[A].toOption match {
              case Some(en) => deriveEnumCodec[A](en)
              case None =>
                assertionFailed("Cannot derive ExprCodec for " + Type.prettyPrint[A])
            }
        }
    }

  private def deriveSingletonCodec[A: Type](sv: SingletonValue[A]): ExprCodec[A] =
    new ExprCodec[A] {
      def toExpr(value: A): Expr[A] = sv.singletonExpr
      def fromExpr(expr: Expr[A]): Option[A] = expr.semiEval.toOption
    }

  private def deriveCaseClassCodec[A: Type](cc: CaseClass[A]): ExprCodec[A] = {
    val fields = cc.caseFields
    val fieldIndicesAndCodecs: List[(String, Int, ExprCodec[Any])] = fields.zipWithIndex.map {
      case (field, i) =>
        val fieldType = field.knownReturning.getOrElse(
          assertionFailed("Unknown return type for field " + field.name + " of " + Type.prettyPrint[A])
        )
        val codec = resolveFieldCodecForDerivation(fieldType)
        (field.name, i, codec)
    }

    new ExprCodec[A] {
      def toExpr(value: A): Expr[A] = {
        val product = value.asInstanceOf[Product]
        val liftedFields: Map[String, Expr_??] = fieldIndicesAndCodecs.map {
          case (name, idx, codec) =>
            val fieldValue = product.productElement(idx)
            val lifted: Expr[Any] = codec.toExpr(fieldValue)
            name -> lifted.as_??(using Expr.typeOf(lifted))
        }.toMap

        cc.construct[hearth.fp.Id] { param =>
          liftedFields(param.name)
        }(using hearth.fp.DirectStyle.DirectStyleForId, hearth.fp.instances.ParallelTraverseForId).get
      }

      def fromExpr(expr: Expr[A]): Option[A] =
        expr.semiEval.toOption
    }
  }

  private def resolveFieldCodecForDerivation(fieldType: ??): ExprCodec[Any] = {
    import fieldType.Underlying as F
    lookupBuiltInExprCodec[F].getOrElse {
      deriveExprCodecInternal[F].asInstanceOf[ExprCodec[Any]]
    }
  }

  private def lookupBuiltInExprCodec[F: Type]: Option[ExprCodec[Any]] =
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
    else None

  private def deriveEnumCodec[A: Type](en: Enum[A]): ExprCodec[A] =
    new ExprCodec[A] {
      def toExpr(value: A): Expr[A] = {
        val className = value.getClass.getSimpleName.stripSuffix("$")
        val result: Option[Expr[A]] = en.directChildren.collectFirst {
          case (name, childType) if name == className =>
            liftEnumChild[A](value, childType)
        }
        result.getOrElse(
          assertionFailed("No child codec for " + className + " in " + Type.prettyPrint[A])
        )
      }

      def fromExpr(expr: Expr[A]): Option[A] =
        expr.semiEval.toOption
    }

  private def liftEnumChild[A: Type](value: A, childType: ??<:[A]): Expr[A] = {
    import childType.Underlying as C
    val childCodec = deriveExprCodecInternal[C]
    val childExpr: Expr[C] = childCodec.toExpr(value.asInstanceOf[C])
    childExpr.upcast[A]
  }
}
