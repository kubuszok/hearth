package hearth
package typed

import hearth.fp.Id
import scala.collection.immutable.ListMap

trait TypesScala3 extends Types { this: MacroCommonsScala3 =>

  import quotes.*, quotes.reflect.*

  final override type Type[A] = scala.quoted.Type[A]

  object Type extends TypeModule {

    object platformSpecific {

      final class LiteralCodec[U: Type](lift: U => Constant) extends TypeCodec[U] {
        final def toType[A <: U](value: A): Type[A] =
          ConstantType(lift(value)).asType.asInstanceOf[Type[A]]
        final def fromType[A](A: Type[A]): Option[Existential.UpperBounded[U, Id]] =
          if A <:< Type[U] then quoted.Type
            .valueOfConstant[U](using A.asInstanceOf[Type[U]])
            .map(Existential.UpperBounded[U, Id, U](_))
          else None
      }
    }
    import platformSpecific.*

    override def shortName[A: Type]: String =
      if Type[A].isVal then {
        // Type symbol approach for case val would upcast it to the enum type.
        val name = TypeRepr.of[A].dealias.show(using Printer.TypeReprCode)
        name.lastIndexOf('.') match {
          case -1  => name
          case idx => name.substring(idx + 1)
        }
      } else (TypeRepr.of[A].dealias.typeSymbol.name: String).replaceAll("\\$", "")

    override def plainPrint[A: Type]: String = removeAnsiColors(prettyPrint[A])
    override def prettyPrint[A: Type]: String = {
      // In Scala 3 typeRepr.dealias dealiases only the "main" type but not types applied as type parameters,
      // while in Scala 2 macros it dealiases everything - to keep the same behavior between them we need to
      // apply recursive dealiasing ourselves.
      def dealiasAll(tpe: TypeRepr): TypeRepr =
        tpe match {
          case AppliedType(tycon, args) => AppliedType(dealiasAll(tycon), args.map(dealiasAll(_)))
          case _                        => tpe.dealias
        }

      val repr = dealiasAll(TypeRepr.of[A])

      scala.util
        .Try {
          val symbolFullName = (repr.typeSymbol.fullName: String).replaceAll("\\$", "")
          val colorlessReprName = repr.show(using Printer.TypeReprCode)
          val colorlessReprNameWithoutParams = colorlessReprName.takeWhile(_ != '[')
          val colorfulReprName = repr.show(using Printer.TypeReprAnsiCode)

          // Classes defined inside a "class" or "def" have package.name.ClassName removed from the type,
          // so we have to prepend it ourselves to keep behavior consistent with Scala 2.
          //
          // TODO: Currently we only add prefix to the outermost type, type parameters are not fixed this way.
          val result = if symbolFullName != colorlessReprNameWithoutParams then {
            val pkg_Len = symbolFullName.length - colorlessReprNameWithoutParams.length
            (0 to pkg_Len)
              .takeWhile(i => symbolFullName.endsWith(("_" * i) + colorlessReprNameWithoutParams))
              .lastOption match {
              case Some(_Len) => symbolFullName.substring(0, pkg_Len - _Len) + colorfulReprName
              case None       => colorfulReprName
            }
          } else colorfulReprName

          // FIXME: This is a quick workaround for missing .type, we have to fix it properly for nested types.
          // TermRef represents a singleton type (term reference) which should also get .type suffix.
          // We use termSymbol to detect TermRef-like types since TermRef.unapply can throw ClassCastException.
          val isSingletonOfTerm = repr.termSymbol != Symbol.noSymbol && !repr.termSymbol.isNoSymbol
          if (Type[A].isObject || Type[A].isVal || isSingletonOfTerm) && !result.contains(".type") then result + ".type"
          else result
        }
        .getOrElse(repr.toString)
    }

    override lazy val NullCodec: TypeCodec[Null] = LiteralCodec[Null](_ => NullConstant())
    override lazy val UnitCodec: TypeCodec[Unit] = LiteralCodec[Unit](_ => UnitConstant())
    override lazy val BooleanCodec: TypeCodec[Boolean] = LiteralCodec[Boolean](BooleanConstant(_))
    override lazy val ByteCodec: TypeCodec[Byte] = LiteralCodec[Byte](ByteConstant(_))
    override lazy val ShortCodec: TypeCodec[Short] = LiteralCodec[Short](ShortConstant(_))
    override lazy val IntCodec: TypeCodec[Int] = LiteralCodec[Int](IntConstant(_))
    override lazy val LongCodec: TypeCodec[Long] = LiteralCodec[Long](LongConstant(_))
    override lazy val FloatCodec: TypeCodec[Float] = LiteralCodec[Float](FloatConstant(_))
    override lazy val DoubleCodec: TypeCodec[Double] = LiteralCodec[Double](DoubleConstant(_))
    override lazy val CharCodec: TypeCodec[Char] = LiteralCodec[Char](CharConstant(_))
    override lazy val StringCodec: TypeCodec[String] = LiteralCodec[String](StringConstant(_))

    override def Tuple1Codec[T1: TypeCodec]: TypeCodec[Tuple1[T1]] =
      new TypeCodec[Tuple1[T1]] {
        override def toType[B <: Tuple1[T1]](value: B): Type[B] = {
          val t1 = TypeCodec[T1].toType(value._1)
          import t1.Underlying as A1
          quoted.Type.of[Tuple1[A1]].asInstanceOf[Type[B]]
        }
        override def fromType[B](B: Type[B]): Option[Existential.UpperBounded[Tuple1[T1], Id]] =
          B match {
            case '[Tuple1[a]] =>
              for {
                v1 <- TypeCodec[T1].fromType(summon[Type[a]])
              } yield Existential.UpperBounded[Tuple1[T1], Id, Tuple1[T1]](
                Tuple1(v1.value.asInstanceOf[T1])
              )(using B.asInstanceOf[Type[Tuple1[T1]]])
            case _ => None
          }
      }

    override def Tuple2Codec[T1: TypeCodec, T2: TypeCodec]: TypeCodec[(T1, T2)] =
      new TypeCodec[(T1, T2)] {
        override def toType[B <: (T1, T2)](value: B): Type[B] = {
          val t1 = TypeCodec[T1].toType(value._1)
          val t2 = TypeCodec[T2].toType(value._2)
          import t1.Underlying as A1
          import t2.Underlying as A2
          quoted.Type.of[(A1, A2)].asInstanceOf[Type[B]]
        }
        override def fromType[B](B: Type[B]): Option[Existential.UpperBounded[(T1, T2), Id]] =
          B match {
            case '[(a1, a2)] =>
              for {
                v1 <- TypeCodec[T1].fromType(summon[Type[a1]])
                v2 <- TypeCodec[T2].fromType(summon[Type[a2]])
              } yield Existential.UpperBounded[(T1, T2), Id, (T1, T2)](
                (v1.value.asInstanceOf[T1], v2.value.asInstanceOf[T2])
              )(using B.asInstanceOf[Type[(T1, T2)]])
            case _ => None
          }
      }

    override def Tuple3Codec[T1: TypeCodec, T2: TypeCodec, T3: TypeCodec]: TypeCodec[(T1, T2, T3)] =
      new TypeCodec[(T1, T2, T3)] {
        override def toType[B <: (T1, T2, T3)](value: B): Type[B] = {
          val t1 = TypeCodec[T1].toType(value._1)
          val t2 = TypeCodec[T2].toType(value._2)
          val t3 = TypeCodec[T3].toType(value._3)
          import t1.Underlying as A1
          import t2.Underlying as A2
          import t3.Underlying as A3
          quoted.Type.of[(A1, A2, A3)].asInstanceOf[Type[B]]
        }
        override def fromType[B](B: Type[B]): Option[Existential.UpperBounded[(T1, T2, T3), Id]] =
          B match {
            case '[(a1, a2, a3)] =>
              for {
                v1 <- TypeCodec[T1].fromType(summon[Type[a1]])
                v2 <- TypeCodec[T2].fromType(summon[Type[a2]])
                v3 <- TypeCodec[T3].fromType(summon[Type[a3]])
              } yield Existential.UpperBounded[(T1, T2, T3), Id, (T1, T2, T3)](
                (v1.value.asInstanceOf[T1], v2.value.asInstanceOf[T2], v3.value.asInstanceOf[T3])
              )(using B.asInstanceOf[Type[(T1, T2, T3)]])
            case _ => None
          }
      }

    override def Tuple4Codec[T1: TypeCodec, T2: TypeCodec, T3: TypeCodec, T4: TypeCodec]: TypeCodec[(T1, T2, T3, T4)] =
      new TypeCodec[(T1, T2, T3, T4)] {
        override def toType[B <: (T1, T2, T3, T4)](value: B): Type[B] = {
          val t1 = TypeCodec[T1].toType(value._1)
          val t2 = TypeCodec[T2].toType(value._2)
          val t3 = TypeCodec[T3].toType(value._3)
          val t4 = TypeCodec[T4].toType(value._4)
          import t1.Underlying as A1
          import t2.Underlying as A2
          import t3.Underlying as A3
          import t4.Underlying as A4
          quoted.Type.of[(A1, A2, A3, A4)].asInstanceOf[Type[B]]
        }
        override def fromType[B](B: Type[B]): Option[Existential.UpperBounded[(T1, T2, T3, T4), Id]] =
          B match {
            case '[(a1, a2, a3, a4)] =>
              for {
                v1 <- TypeCodec[T1].fromType(summon[Type[a1]])
                v2 <- TypeCodec[T2].fromType(summon[Type[a2]])
                v3 <- TypeCodec[T3].fromType(summon[Type[a3]])
                v4 <- TypeCodec[T4].fromType(summon[Type[a4]])
              } yield Existential.UpperBounded[(T1, T2, T3, T4), Id, (T1, T2, T3, T4)](
                (
                  v1.value.asInstanceOf[T1],
                  v2.value.asInstanceOf[T2],
                  v3.value.asInstanceOf[T3],
                  v4.value.asInstanceOf[T4]
                )
              )(using B.asInstanceOf[Type[(T1, T2, T3, T4)]])
            case _ => None
          }
      }

    override def Tuple5Codec[T1: TypeCodec, T2: TypeCodec, T3: TypeCodec, T4: TypeCodec, T5: TypeCodec]
        : TypeCodec[(T1, T2, T3, T4, T5)] =
      new TypeCodec[(T1, T2, T3, T4, T5)] {
        override def toType[B <: (T1, T2, T3, T4, T5)](value: B): Type[B] = {
          val t1 = TypeCodec[T1].toType(value._1)
          val t2 = TypeCodec[T2].toType(value._2)
          val t3 = TypeCodec[T3].toType(value._3)
          val t4 = TypeCodec[T4].toType(value._4)
          val t5 = TypeCodec[T5].toType(value._5)
          import t1.Underlying as A1
          import t2.Underlying as A2
          import t3.Underlying as A3
          import t4.Underlying as A4
          import t5.Underlying as A5
          quoted.Type.of[(A1, A2, A3, A4, A5)].asInstanceOf[Type[B]]
        }
        override def fromType[B](B: Type[B]): Option[Existential.UpperBounded[(T1, T2, T3, T4, T5), Id]] =
          B match {
            case '[(a1, a2, a3, a4, a5)] =>
              for {
                v1 <- TypeCodec[T1].fromType(summon[Type[a1]])
                v2 <- TypeCodec[T2].fromType(summon[Type[a2]])
                v3 <- TypeCodec[T3].fromType(summon[Type[a3]])
                v4 <- TypeCodec[T4].fromType(summon[Type[a4]])
                v5 <- TypeCodec[T5].fromType(summon[Type[a5]])
              } yield Existential.UpperBounded[(T1, T2, T3, T4, T5), Id, (T1, T2, T3, T4, T5)](
                (
                  v1.value.asInstanceOf[T1],
                  v2.value.asInstanceOf[T2],
                  v3.value.asInstanceOf[T3],
                  v4.value.asInstanceOf[T4],
                  v5.value.asInstanceOf[T5]
                )
              )(using B.asInstanceOf[Type[(T1, T2, T3, T4, T5)]])
            case _ => None
          }
      }

    override def Tuple6Codec[T1: TypeCodec, T2: TypeCodec, T3: TypeCodec, T4: TypeCodec, T5: TypeCodec, T6: TypeCodec]
        : TypeCodec[(T1, T2, T3, T4, T5, T6)] =
      new TypeCodec[(T1, T2, T3, T4, T5, T6)] {
        override def toType[B <: (T1, T2, T3, T4, T5, T6)](value: B): Type[B] = {
          val t1 = TypeCodec[T1].toType(value._1)
          val t2 = TypeCodec[T2].toType(value._2)
          val t3 = TypeCodec[T3].toType(value._3)
          val t4 = TypeCodec[T4].toType(value._4)
          val t5 = TypeCodec[T5].toType(value._5)
          val t6 = TypeCodec[T6].toType(value._6)
          import t1.Underlying as A1
          import t2.Underlying as A2
          import t3.Underlying as A3
          import t4.Underlying as A4
          import t5.Underlying as A5
          import t6.Underlying as A6
          quoted.Type.of[(A1, A2, A3, A4, A5, A6)].asInstanceOf[Type[B]]
        }
        override def fromType[B](B: Type[B]): Option[Existential.UpperBounded[(T1, T2, T3, T4, T5, T6), Id]] =
          B match {
            case '[(a1, a2, a3, a4, a5, a6)] =>
              for {
                v1 <- TypeCodec[T1].fromType(summon[Type[a1]])
                v2 <- TypeCodec[T2].fromType(summon[Type[a2]])
                v3 <- TypeCodec[T3].fromType(summon[Type[a3]])
                v4 <- TypeCodec[T4].fromType(summon[Type[a4]])
                v5 <- TypeCodec[T5].fromType(summon[Type[a5]])
                v6 <- TypeCodec[T6].fromType(summon[Type[a6]])
              } yield Existential.UpperBounded[(T1, T2, T3, T4, T5, T6), Id, (T1, T2, T3, T4, T5, T6)](
                (
                  v1.value.asInstanceOf[T1],
                  v2.value.asInstanceOf[T2],
                  v3.value.asInstanceOf[T3],
                  v4.value.asInstanceOf[T4],
                  v5.value.asInstanceOf[T5],
                  v6.value.asInstanceOf[T6]
                )
              )(using B.asInstanceOf[Type[(T1, T2, T3, T4, T5, T6)]])
            case _ => None
          }
      }

    override def Tuple7Codec[
        T1: TypeCodec,
        T2: TypeCodec,
        T3: TypeCodec,
        T4: TypeCodec,
        T5: TypeCodec,
        T6: TypeCodec,
        T7: TypeCodec
    ]: TypeCodec[(T1, T2, T3, T4, T5, T6, T7)] =
      new TypeCodec[(T1, T2, T3, T4, T5, T6, T7)] {
        override def toType[B <: (T1, T2, T3, T4, T5, T6, T7)](value: B): Type[B] = {
          val t1 = TypeCodec[T1].toType(value._1)
          val t2 = TypeCodec[T2].toType(value._2)
          val t3 = TypeCodec[T3].toType(value._3)
          val t4 = TypeCodec[T4].toType(value._4)
          val t5 = TypeCodec[T5].toType(value._5)
          val t6 = TypeCodec[T6].toType(value._6)
          val t7 = TypeCodec[T7].toType(value._7)
          import t1.Underlying as A1
          import t2.Underlying as A2
          import t3.Underlying as A3
          import t4.Underlying as A4
          import t5.Underlying as A5
          import t6.Underlying as A6
          import t7.Underlying as A7
          quoted.Type.of[(A1, A2, A3, A4, A5, A6, A7)].asInstanceOf[Type[B]]
        }
        override def fromType[B](B: Type[B]): Option[Existential.UpperBounded[(T1, T2, T3, T4, T5, T6, T7), Id]] =
          B match {
            case '[(a1, a2, a3, a4, a5, a6, a7)] =>
              for {
                v1 <- TypeCodec[T1].fromType(summon[Type[a1]])
                v2 <- TypeCodec[T2].fromType(summon[Type[a2]])
                v3 <- TypeCodec[T3].fromType(summon[Type[a3]])
                v4 <- TypeCodec[T4].fromType(summon[Type[a4]])
                v5 <- TypeCodec[T5].fromType(summon[Type[a5]])
                v6 <- TypeCodec[T6].fromType(summon[Type[a6]])
                v7 <- TypeCodec[T7].fromType(summon[Type[a7]])
              } yield Existential.UpperBounded[(T1, T2, T3, T4, T5, T6, T7), Id, (T1, T2, T3, T4, T5, T6, T7)](
                (
                  v1.value.asInstanceOf[T1],
                  v2.value.asInstanceOf[T2],
                  v3.value.asInstanceOf[T3],
                  v4.value.asInstanceOf[T4],
                  v5.value.asInstanceOf[T5],
                  v6.value.asInstanceOf[T6],
                  v7.value.asInstanceOf[T7]
                )
              )(using B.asInstanceOf[Type[(T1, T2, T3, T4, T5, T6, T7)]])
            case _ => None
          }
      }

    override def Tuple8Codec[
        T1: TypeCodec,
        T2: TypeCodec,
        T3: TypeCodec,
        T4: TypeCodec,
        T5: TypeCodec,
        T6: TypeCodec,
        T7: TypeCodec,
        T8: TypeCodec
    ]: TypeCodec[(T1, T2, T3, T4, T5, T6, T7, T8)] =
      new TypeCodec[(T1, T2, T3, T4, T5, T6, T7, T8)] {
        override def toType[B <: (T1, T2, T3, T4, T5, T6, T7, T8)](value: B): Type[B] = {
          val t1 = TypeCodec[T1].toType(value._1)
          val t2 = TypeCodec[T2].toType(value._2)
          val t3 = TypeCodec[T3].toType(value._3)
          val t4 = TypeCodec[T4].toType(value._4)
          val t5 = TypeCodec[T5].toType(value._5)
          val t6 = TypeCodec[T6].toType(value._6)
          val t7 = TypeCodec[T7].toType(value._7)
          val t8 = TypeCodec[T8].toType(value._8)
          import t1.Underlying as A1
          import t2.Underlying as A2
          import t3.Underlying as A3
          import t4.Underlying as A4
          import t5.Underlying as A5
          import t6.Underlying as A6
          import t7.Underlying as A7
          import t8.Underlying as A8
          quoted.Type.of[(A1, A2, A3, A4, A5, A6, A7, A8)].asInstanceOf[Type[B]]
        }
        override def fromType[B](B: Type[B]): Option[Existential.UpperBounded[(T1, T2, T3, T4, T5, T6, T7, T8), Id]] =
          B match {
            case '[(a1, a2, a3, a4, a5, a6, a7, a8)] =>
              for {
                v1 <- TypeCodec[T1].fromType(summon[Type[a1]])
                v2 <- TypeCodec[T2].fromType(summon[Type[a2]])
                v3 <- TypeCodec[T3].fromType(summon[Type[a3]])
                v4 <- TypeCodec[T4].fromType(summon[Type[a4]])
                v5 <- TypeCodec[T5].fromType(summon[Type[a5]])
                v6 <- TypeCodec[T6].fromType(summon[Type[a6]])
                v7 <- TypeCodec[T7].fromType(summon[Type[a7]])
                v8 <- TypeCodec[T8].fromType(summon[Type[a8]])
              } yield Existential.UpperBounded[(T1, T2, T3, T4, T5, T6, T7, T8), Id, (T1, T2, T3, T4, T5, T6, T7, T8)](
                (
                  v1.value.asInstanceOf[T1],
                  v2.value.asInstanceOf[T2],
                  v3.value.asInstanceOf[T3],
                  v4.value.asInstanceOf[T4],
                  v5.value.asInstanceOf[T5],
                  v6.value.asInstanceOf[T6],
                  v7.value.asInstanceOf[T7],
                  v8.value.asInstanceOf[T8]
                )
              )(using B.asInstanceOf[Type[(T1, T2, T3, T4, T5, T6, T7, T8)]])
            case _ => None
          }
      }

    override def Tuple9Codec[
        T1: TypeCodec,
        T2: TypeCodec,
        T3: TypeCodec,
        T4: TypeCodec,
        T5: TypeCodec,
        T6: TypeCodec,
        T7: TypeCodec,
        T8: TypeCodec,
        T9: TypeCodec
    ]: TypeCodec[(T1, T2, T3, T4, T5, T6, T7, T8, T9)] =
      new TypeCodec[(T1, T2, T3, T4, T5, T6, T7, T8, T9)] {
        override def toType[B <: (T1, T2, T3, T4, T5, T6, T7, T8, T9)](value: B): Type[B] = {
          val t1 = TypeCodec[T1].toType(value._1)
          val t2 = TypeCodec[T2].toType(value._2)
          val t3 = TypeCodec[T3].toType(value._3)
          val t4 = TypeCodec[T4].toType(value._4)
          val t5 = TypeCodec[T5].toType(value._5)
          val t6 = TypeCodec[T6].toType(value._6)
          val t7 = TypeCodec[T7].toType(value._7)
          val t8 = TypeCodec[T8].toType(value._8)
          val t9 = TypeCodec[T9].toType(value._9)
          import t1.Underlying as A1
          import t2.Underlying as A2
          import t3.Underlying as A3
          import t4.Underlying as A4
          import t5.Underlying as A5
          import t6.Underlying as A6
          import t7.Underlying as A7
          import t8.Underlying as A8
          import t9.Underlying as A9
          quoted.Type.of[(A1, A2, A3, A4, A5, A6, A7, A8, A9)].asInstanceOf[Type[B]]
        }
        override def fromType[B](
            B: Type[B]
        ): Option[Existential.UpperBounded[(T1, T2, T3, T4, T5, T6, T7, T8, T9), Id]] =
          B match {
            case '[(a1, a2, a3, a4, a5, a6, a7, a8, a9)] =>
              for {
                v1 <- TypeCodec[T1].fromType(summon[Type[a1]])
                v2 <- TypeCodec[T2].fromType(summon[Type[a2]])
                v3 <- TypeCodec[T3].fromType(summon[Type[a3]])
                v4 <- TypeCodec[T4].fromType(summon[Type[a4]])
                v5 <- TypeCodec[T5].fromType(summon[Type[a5]])
                v6 <- TypeCodec[T6].fromType(summon[Type[a6]])
                v7 <- TypeCodec[T7].fromType(summon[Type[a7]])
                v8 <- TypeCodec[T8].fromType(summon[Type[a8]])
                v9 <- TypeCodec[T9].fromType(summon[Type[a9]])
              } yield Existential
                .UpperBounded[(T1, T2, T3, T4, T5, T6, T7, T8, T9), Id, (T1, T2, T3, T4, T5, T6, T7, T8, T9)](
                  (
                    v1.value.asInstanceOf[T1],
                    v2.value.asInstanceOf[T2],
                    v3.value.asInstanceOf[T3],
                    v4.value.asInstanceOf[T4],
                    v5.value.asInstanceOf[T5],
                    v6.value.asInstanceOf[T6],
                    v7.value.asInstanceOf[T7],
                    v8.value.asInstanceOf[T8],
                    v9.value.asInstanceOf[T9]
                  )
                )(using B.asInstanceOf[Type[(T1, T2, T3, T4, T5, T6, T7, T8, T9)]])
            case _ => None
          }
      }

    override def Tuple10Codec[
        T1: TypeCodec,
        T2: TypeCodec,
        T3: TypeCodec,
        T4: TypeCodec,
        T5: TypeCodec,
        T6: TypeCodec,
        T7: TypeCodec,
        T8: TypeCodec,
        T9: TypeCodec,
        T10: TypeCodec
    ]: TypeCodec[(T1, T2, T3, T4, T5, T6, T7, T8, T9, T10)] =
      new TypeCodec[(T1, T2, T3, T4, T5, T6, T7, T8, T9, T10)] {
        override def toType[B <: (T1, T2, T3, T4, T5, T6, T7, T8, T9, T10)](value: B): Type[B] = {
          val t1 = TypeCodec[T1].toType(value._1)
          val t2 = TypeCodec[T2].toType(value._2)
          val t3 = TypeCodec[T3].toType(value._3)
          val t4 = TypeCodec[T4].toType(value._4)
          val t5 = TypeCodec[T5].toType(value._5)
          val t6 = TypeCodec[T6].toType(value._6)
          val t7 = TypeCodec[T7].toType(value._7)
          val t8 = TypeCodec[T8].toType(value._8)
          val t9 = TypeCodec[T9].toType(value._9)
          val t10 = TypeCodec[T10].toType(value._10)
          import t1.Underlying as A1
          import t2.Underlying as A2
          import t3.Underlying as A3
          import t4.Underlying as A4
          import t5.Underlying as A5
          import t6.Underlying as A6
          import t7.Underlying as A7
          import t8.Underlying as A8
          import t9.Underlying as A9
          import t10.Underlying as A10
          quoted.Type.of[(A1, A2, A3, A4, A5, A6, A7, A8, A9, A10)].asInstanceOf[Type[B]]
        }
        override def fromType[B](
            B: Type[B]
        ): Option[Existential.UpperBounded[(T1, T2, T3, T4, T5, T6, T7, T8, T9, T10), Id]] =
          B match {
            case '[(a1, a2, a3, a4, a5, a6, a7, a8, a9, a10)] =>
              for {
                v1 <- TypeCodec[T1].fromType(summon[Type[a1]])
                v2 <- TypeCodec[T2].fromType(summon[Type[a2]])
                v3 <- TypeCodec[T3].fromType(summon[Type[a3]])
                v4 <- TypeCodec[T4].fromType(summon[Type[a4]])
                v5 <- TypeCodec[T5].fromType(summon[Type[a5]])
                v6 <- TypeCodec[T6].fromType(summon[Type[a6]])
                v7 <- TypeCodec[T7].fromType(summon[Type[a7]])
                v8 <- TypeCodec[T8].fromType(summon[Type[a8]])
                v9 <- TypeCodec[T9].fromType(summon[Type[a9]])
                v10 <- TypeCodec[T10].fromType(summon[Type[a10]])
              } yield Existential
                .UpperBounded[(T1, T2, T3, T4, T5, T6, T7, T8, T9, T10), Id, (T1, T2, T3, T4, T5, T6, T7, T8, T9, T10)](
                  (
                    v1.value.asInstanceOf[T1],
                    v2.value.asInstanceOf[T2],
                    v3.value.asInstanceOf[T3],
                    v4.value.asInstanceOf[T4],
                    v5.value.asInstanceOf[T5],
                    v6.value.asInstanceOf[T6],
                    v7.value.asInstanceOf[T7],
                    v8.value.asInstanceOf[T8],
                    v9.value.asInstanceOf[T9],
                    v10.value.asInstanceOf[T10]
                  )
                )(using B.asInstanceOf[Type[(T1, T2, T3, T4, T5, T6, T7, T8, T9, T10)]])
            case _ => None
          }
      }

    override def Tuple11Codec[
        T1: TypeCodec,
        T2: TypeCodec,
        T3: TypeCodec,
        T4: TypeCodec,
        T5: TypeCodec,
        T6: TypeCodec,
        T7: TypeCodec,
        T8: TypeCodec,
        T9: TypeCodec,
        T10: TypeCodec,
        T11: TypeCodec
    ]: TypeCodec[(T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11)] =
      new TypeCodec[(T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11)] {
        override def toType[B <: (T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11)](value: B): Type[B] = {
          val t1 = TypeCodec[T1].toType(value._1)
          val t2 = TypeCodec[T2].toType(value._2)
          val t3 = TypeCodec[T3].toType(value._3)
          val t4 = TypeCodec[T4].toType(value._4)
          val t5 = TypeCodec[T5].toType(value._5)
          val t6 = TypeCodec[T6].toType(value._6)
          val t7 = TypeCodec[T7].toType(value._7)
          val t8 = TypeCodec[T8].toType(value._8)
          val t9 = TypeCodec[T9].toType(value._9)
          val t10 = TypeCodec[T10].toType(value._10)
          val t11 = TypeCodec[T11].toType(value._11)
          import t1.Underlying as A1
          import t2.Underlying as A2
          import t3.Underlying as A3
          import t4.Underlying as A4
          import t5.Underlying as A5
          import t6.Underlying as A6
          import t7.Underlying as A7
          import t8.Underlying as A8
          import t9.Underlying as A9
          import t10.Underlying as A10
          import t11.Underlying as A11
          quoted.Type.of[(A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11)].asInstanceOf[Type[B]]
        }
        override def fromType[B](
            B: Type[B]
        ): Option[Existential.UpperBounded[(T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11), Id]] =
          B match {
            case '[(a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11)] =>
              for {
                v1 <- TypeCodec[T1].fromType(summon[Type[a1]])
                v2 <- TypeCodec[T2].fromType(summon[Type[a2]])
                v3 <- TypeCodec[T3].fromType(summon[Type[a3]])
                v4 <- TypeCodec[T4].fromType(summon[Type[a4]])
                v5 <- TypeCodec[T5].fromType(summon[Type[a5]])
                v6 <- TypeCodec[T6].fromType(summon[Type[a6]])
                v7 <- TypeCodec[T7].fromType(summon[Type[a7]])
                v8 <- TypeCodec[T8].fromType(summon[Type[a8]])
                v9 <- TypeCodec[T9].fromType(summon[Type[a9]])
                v10 <- TypeCodec[T10].fromType(summon[Type[a10]])
                v11 <- TypeCodec[T11].fromType(summon[Type[a11]])
              } yield Existential.UpperBounded[
                (T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11),
                Id,
                (T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11)
              ](
                (
                  v1.value.asInstanceOf[T1],
                  v2.value.asInstanceOf[T2],
                  v3.value.asInstanceOf[T3],
                  v4.value.asInstanceOf[T4],
                  v5.value.asInstanceOf[T5],
                  v6.value.asInstanceOf[T6],
                  v7.value.asInstanceOf[T7],
                  v8.value.asInstanceOf[T8],
                  v9.value.asInstanceOf[T9],
                  v10.value.asInstanceOf[T10],
                  v11.value.asInstanceOf[T11]
                )
              )(using B.asInstanceOf[Type[(T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11)]])
            case _ => None
          }
      }

    override def Tuple12Codec[
        T1: TypeCodec,
        T2: TypeCodec,
        T3: TypeCodec,
        T4: TypeCodec,
        T5: TypeCodec,
        T6: TypeCodec,
        T7: TypeCodec,
        T8: TypeCodec,
        T9: TypeCodec,
        T10: TypeCodec,
        T11: TypeCodec,
        T12: TypeCodec
    ]: TypeCodec[(T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12)] =
      new TypeCodec[(T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12)] {
        override def toType[B <: (T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12)](value: B): Type[B] = {
          val t1 = TypeCodec[T1].toType(value._1)
          val t2 = TypeCodec[T2].toType(value._2)
          val t3 = TypeCodec[T3].toType(value._3)
          val t4 = TypeCodec[T4].toType(value._4)
          val t5 = TypeCodec[T5].toType(value._5)
          val t6 = TypeCodec[T6].toType(value._6)
          val t7 = TypeCodec[T7].toType(value._7)
          val t8 = TypeCodec[T8].toType(value._8)
          val t9 = TypeCodec[T9].toType(value._9)
          val t10 = TypeCodec[T10].toType(value._10)
          val t11 = TypeCodec[T11].toType(value._11)
          val t12 = TypeCodec[T12].toType(value._12)
          import t1.Underlying as A1
          import t2.Underlying as A2
          import t3.Underlying as A3
          import t4.Underlying as A4
          import t5.Underlying as A5
          import t6.Underlying as A6
          import t7.Underlying as A7
          import t8.Underlying as A8
          import t9.Underlying as A9
          import t10.Underlying as A10
          import t11.Underlying as A11
          import t12.Underlying as A12
          quoted.Type.of[(A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12)].asInstanceOf[Type[B]]
        }
        override def fromType[B](
            B: Type[B]
        ): Option[Existential.UpperBounded[(T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12), Id]] =
          B match {
            case '[(a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12)] =>
              for {
                v1 <- TypeCodec[T1].fromType(summon[Type[a1]])
                v2 <- TypeCodec[T2].fromType(summon[Type[a2]])
                v3 <- TypeCodec[T3].fromType(summon[Type[a3]])
                v4 <- TypeCodec[T4].fromType(summon[Type[a4]])
                v5 <- TypeCodec[T5].fromType(summon[Type[a5]])
                v6 <- TypeCodec[T6].fromType(summon[Type[a6]])
                v7 <- TypeCodec[T7].fromType(summon[Type[a7]])
                v8 <- TypeCodec[T8].fromType(summon[Type[a8]])
                v9 <- TypeCodec[T9].fromType(summon[Type[a9]])
                v10 <- TypeCodec[T10].fromType(summon[Type[a10]])
                v11 <- TypeCodec[T11].fromType(summon[Type[a11]])
                v12 <- TypeCodec[T12].fromType(summon[Type[a12]])
              } yield Existential.UpperBounded[
                (T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12),
                Id,
                (T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12)
              ](
                (
                  v1.value.asInstanceOf[T1],
                  v2.value.asInstanceOf[T2],
                  v3.value.asInstanceOf[T3],
                  v4.value.asInstanceOf[T4],
                  v5.value.asInstanceOf[T5],
                  v6.value.asInstanceOf[T6],
                  v7.value.asInstanceOf[T7],
                  v8.value.asInstanceOf[T8],
                  v9.value.asInstanceOf[T9],
                  v10.value.asInstanceOf[T10],
                  v11.value.asInstanceOf[T11],
                  v12.value.asInstanceOf[T12]
                )
              )(using B.asInstanceOf[Type[(T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12)]])
            case _ => None
          }
      }

    override def Tuple13Codec[
        T1: TypeCodec,
        T2: TypeCodec,
        T3: TypeCodec,
        T4: TypeCodec,
        T5: TypeCodec,
        T6: TypeCodec,
        T7: TypeCodec,
        T8: TypeCodec,
        T9: TypeCodec,
        T10: TypeCodec,
        T11: TypeCodec,
        T12: TypeCodec,
        T13: TypeCodec
    ]: TypeCodec[(T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13)] =
      new TypeCodec[(T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13)] {
        override def toType[B <: (T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13)](value: B): Type[B] = {
          val t1 = TypeCodec[T1].toType(value._1)
          val t2 = TypeCodec[T2].toType(value._2)
          val t3 = TypeCodec[T3].toType(value._3)
          val t4 = TypeCodec[T4].toType(value._4)
          val t5 = TypeCodec[T5].toType(value._5)
          val t6 = TypeCodec[T6].toType(value._6)
          val t7 = TypeCodec[T7].toType(value._7)
          val t8 = TypeCodec[T8].toType(value._8)
          val t9 = TypeCodec[T9].toType(value._9)
          val t10 = TypeCodec[T10].toType(value._10)
          val t11 = TypeCodec[T11].toType(value._11)
          val t12 = TypeCodec[T12].toType(value._12)
          val t13 = TypeCodec[T13].toType(value._13)
          import t1.Underlying as A1
          import t2.Underlying as A2
          import t3.Underlying as A3
          import t4.Underlying as A4
          import t5.Underlying as A5
          import t6.Underlying as A6
          import t7.Underlying as A7
          import t8.Underlying as A8
          import t9.Underlying as A9
          import t10.Underlying as A10
          import t11.Underlying as A11
          import t12.Underlying as A12
          import t13.Underlying as A13
          quoted.Type.of[(A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13)].asInstanceOf[Type[B]]
        }
        override def fromType[B](
            B: Type[B]
        ): Option[Existential.UpperBounded[(T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13), Id]] =
          B match {
            case '[(a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13)] =>
              for {
                v1 <- TypeCodec[T1].fromType(summon[Type[a1]])
                v2 <- TypeCodec[T2].fromType(summon[Type[a2]])
                v3 <- TypeCodec[T3].fromType(summon[Type[a3]])
                v4 <- TypeCodec[T4].fromType(summon[Type[a4]])
                v5 <- TypeCodec[T5].fromType(summon[Type[a5]])
                v6 <- TypeCodec[T6].fromType(summon[Type[a6]])
                v7 <- TypeCodec[T7].fromType(summon[Type[a7]])
                v8 <- TypeCodec[T8].fromType(summon[Type[a8]])
                v9 <- TypeCodec[T9].fromType(summon[Type[a9]])
                v10 <- TypeCodec[T10].fromType(summon[Type[a10]])
                v11 <- TypeCodec[T11].fromType(summon[Type[a11]])
                v12 <- TypeCodec[T12].fromType(summon[Type[a12]])
                v13 <- TypeCodec[T13].fromType(summon[Type[a13]])
              } yield Existential.UpperBounded[
                (T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13),
                Id,
                (T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13)
              ](
                (
                  v1.value.asInstanceOf[T1],
                  v2.value.asInstanceOf[T2],
                  v3.value.asInstanceOf[T3],
                  v4.value.asInstanceOf[T4],
                  v5.value.asInstanceOf[T5],
                  v6.value.asInstanceOf[T6],
                  v7.value.asInstanceOf[T7],
                  v8.value.asInstanceOf[T8],
                  v9.value.asInstanceOf[T9],
                  v10.value.asInstanceOf[T10],
                  v11.value.asInstanceOf[T11],
                  v12.value.asInstanceOf[T12],
                  v13.value.asInstanceOf[T13]
                )
              )(using B.asInstanceOf[Type[(T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13)]])
            case _ => None
          }
      }

    override def Tuple14Codec[
        T1: TypeCodec,
        T2: TypeCodec,
        T3: TypeCodec,
        T4: TypeCodec,
        T5: TypeCodec,
        T6: TypeCodec,
        T7: TypeCodec,
        T8: TypeCodec,
        T9: TypeCodec,
        T10: TypeCodec,
        T11: TypeCodec,
        T12: TypeCodec,
        T13: TypeCodec,
        T14: TypeCodec
    ]: TypeCodec[(T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14)] =
      new TypeCodec[(T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14)] {
        override def toType[B <: (T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14)](value: B): Type[B] = {
          val t1 = TypeCodec[T1].toType(value._1)
          val t2 = TypeCodec[T2].toType(value._2)
          val t3 = TypeCodec[T3].toType(value._3)
          val t4 = TypeCodec[T4].toType(value._4)
          val t5 = TypeCodec[T5].toType(value._5)
          val t6 = TypeCodec[T6].toType(value._6)
          val t7 = TypeCodec[T7].toType(value._7)
          val t8 = TypeCodec[T8].toType(value._8)
          val t9 = TypeCodec[T9].toType(value._9)
          val t10 = TypeCodec[T10].toType(value._10)
          val t11 = TypeCodec[T11].toType(value._11)
          val t12 = TypeCodec[T12].toType(value._12)
          val t13 = TypeCodec[T13].toType(value._13)
          val t14 = TypeCodec[T14].toType(value._14)
          import t1.Underlying as A1
          import t2.Underlying as A2
          import t3.Underlying as A3
          import t4.Underlying as A4
          import t5.Underlying as A5
          import t6.Underlying as A6
          import t7.Underlying as A7
          import t8.Underlying as A8
          import t9.Underlying as A9
          import t10.Underlying as A10
          import t11.Underlying as A11
          import t12.Underlying as A12
          import t13.Underlying as A13
          import t14.Underlying as A14
          quoted.Type.of[(A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14)].asInstanceOf[Type[B]]
        }
        override def fromType[B](
            B: Type[B]
        ): Option[Existential.UpperBounded[(T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14), Id]] =
          B match {
            case '[(a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13, a14)] =>
              for {
                v1 <- TypeCodec[T1].fromType(summon[Type[a1]])
                v2 <- TypeCodec[T2].fromType(summon[Type[a2]])
                v3 <- TypeCodec[T3].fromType(summon[Type[a3]])
                v4 <- TypeCodec[T4].fromType(summon[Type[a4]])
                v5 <- TypeCodec[T5].fromType(summon[Type[a5]])
                v6 <- TypeCodec[T6].fromType(summon[Type[a6]])
                v7 <- TypeCodec[T7].fromType(summon[Type[a7]])
                v8 <- TypeCodec[T8].fromType(summon[Type[a8]])
                v9 <- TypeCodec[T9].fromType(summon[Type[a9]])
                v10 <- TypeCodec[T10].fromType(summon[Type[a10]])
                v11 <- TypeCodec[T11].fromType(summon[Type[a11]])
                v12 <- TypeCodec[T12].fromType(summon[Type[a12]])
                v13 <- TypeCodec[T13].fromType(summon[Type[a13]])
                v14 <- TypeCodec[T14].fromType(summon[Type[a14]])
              } yield Existential.UpperBounded[
                (T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14),
                Id,
                (T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14)
              ](
                (
                  v1.value.asInstanceOf[T1],
                  v2.value.asInstanceOf[T2],
                  v3.value.asInstanceOf[T3],
                  v4.value.asInstanceOf[T4],
                  v5.value.asInstanceOf[T5],
                  v6.value.asInstanceOf[T6],
                  v7.value.asInstanceOf[T7],
                  v8.value.asInstanceOf[T8],
                  v9.value.asInstanceOf[T9],
                  v10.value.asInstanceOf[T10],
                  v11.value.asInstanceOf[T11],
                  v12.value.asInstanceOf[T12],
                  v13.value.asInstanceOf[T13],
                  v14.value.asInstanceOf[T14]
                )
              )(using B.asInstanceOf[Type[(T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14)]])
            case _ => None
          }
      }

    override def Tuple15Codec[
        T1: TypeCodec,
        T2: TypeCodec,
        T3: TypeCodec,
        T4: TypeCodec,
        T5: TypeCodec,
        T6: TypeCodec,
        T7: TypeCodec,
        T8: TypeCodec,
        T9: TypeCodec,
        T10: TypeCodec,
        T11: TypeCodec,
        T12: TypeCodec,
        T13: TypeCodec,
        T14: TypeCodec,
        T15: TypeCodec
    ]: TypeCodec[(T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15)] =
      new TypeCodec[(T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15)] {
        override def toType[B <: (T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15)](
            value: B
        ): Type[B] = {
          val t1 = TypeCodec[T1].toType(value._1)
          val t2 = TypeCodec[T2].toType(value._2)
          val t3 = TypeCodec[T3].toType(value._3)
          val t4 = TypeCodec[T4].toType(value._4)
          val t5 = TypeCodec[T5].toType(value._5)
          val t6 = TypeCodec[T6].toType(value._6)
          val t7 = TypeCodec[T7].toType(value._7)
          val t8 = TypeCodec[T8].toType(value._8)
          val t9 = TypeCodec[T9].toType(value._9)
          val t10 = TypeCodec[T10].toType(value._10)
          val t11 = TypeCodec[T11].toType(value._11)
          val t12 = TypeCodec[T12].toType(value._12)
          val t13 = TypeCodec[T13].toType(value._13)
          val t14 = TypeCodec[T14].toType(value._14)
          val t15 = TypeCodec[T15].toType(value._15)
          import t1.Underlying as A1
          import t2.Underlying as A2
          import t3.Underlying as A3
          import t4.Underlying as A4
          import t5.Underlying as A5
          import t6.Underlying as A6
          import t7.Underlying as A7
          import t8.Underlying as A8
          import t9.Underlying as A9
          import t10.Underlying as A10
          import t11.Underlying as A11
          import t12.Underlying as A12
          import t13.Underlying as A13
          import t14.Underlying as A14
          import t15.Underlying as A15
          quoted.Type.of[(A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15)].asInstanceOf[Type[B]]
        }
        override def fromType[B](
            B: Type[B]
        ): Option[Existential.UpperBounded[(T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15), Id]] =
          B match {
            case '[(a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13, a14, a15)] =>
              for {
                v1 <- TypeCodec[T1].fromType(summon[Type[a1]])
                v2 <- TypeCodec[T2].fromType(summon[Type[a2]])
                v3 <- TypeCodec[T3].fromType(summon[Type[a3]])
                v4 <- TypeCodec[T4].fromType(summon[Type[a4]])
                v5 <- TypeCodec[T5].fromType(summon[Type[a5]])
                v6 <- TypeCodec[T6].fromType(summon[Type[a6]])
                v7 <- TypeCodec[T7].fromType(summon[Type[a7]])
                v8 <- TypeCodec[T8].fromType(summon[Type[a8]])
                v9 <- TypeCodec[T9].fromType(summon[Type[a9]])
                v10 <- TypeCodec[T10].fromType(summon[Type[a10]])
                v11 <- TypeCodec[T11].fromType(summon[Type[a11]])
                v12 <- TypeCodec[T12].fromType(summon[Type[a12]])
                v13 <- TypeCodec[T13].fromType(summon[Type[a13]])
                v14 <- TypeCodec[T14].fromType(summon[Type[a14]])
                v15 <- TypeCodec[T15].fromType(summon[Type[a15]])
              } yield Existential.UpperBounded[
                (T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15),
                Id,
                (T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15)
              ](
                (
                  v1.value.asInstanceOf[T1],
                  v2.value.asInstanceOf[T2],
                  v3.value.asInstanceOf[T3],
                  v4.value.asInstanceOf[T4],
                  v5.value.asInstanceOf[T5],
                  v6.value.asInstanceOf[T6],
                  v7.value.asInstanceOf[T7],
                  v8.value.asInstanceOf[T8],
                  v9.value.asInstanceOf[T9],
                  v10.value.asInstanceOf[T10],
                  v11.value.asInstanceOf[T11],
                  v12.value.asInstanceOf[T12],
                  v13.value.asInstanceOf[T13],
                  v14.value.asInstanceOf[T14],
                  v15.value.asInstanceOf[T15]
                )
              )(using B.asInstanceOf[Type[(T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15)]])
            case _ => None
          }
      }

    override def Tuple16Codec[
        T1: TypeCodec,
        T2: TypeCodec,
        T3: TypeCodec,
        T4: TypeCodec,
        T5: TypeCodec,
        T6: TypeCodec,
        T7: TypeCodec,
        T8: TypeCodec,
        T9: TypeCodec,
        T10: TypeCodec,
        T11: TypeCodec,
        T12: TypeCodec,
        T13: TypeCodec,
        T14: TypeCodec,
        T15: TypeCodec,
        T16: TypeCodec
    ]: TypeCodec[(T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16)] =
      new TypeCodec[(T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16)] {
        override def toType[B <: (T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16)](
            value: B
        ): Type[B] = {
          val t1 = TypeCodec[T1].toType(value._1)
          val t2 = TypeCodec[T2].toType(value._2)
          val t3 = TypeCodec[T3].toType(value._3)
          val t4 = TypeCodec[T4].toType(value._4)
          val t5 = TypeCodec[T5].toType(value._5)
          val t6 = TypeCodec[T6].toType(value._6)
          val t7 = TypeCodec[T7].toType(value._7)
          val t8 = TypeCodec[T8].toType(value._8)
          val t9 = TypeCodec[T9].toType(value._9)
          val t10 = TypeCodec[T10].toType(value._10)
          val t11 = TypeCodec[T11].toType(value._11)
          val t12 = TypeCodec[T12].toType(value._12)
          val t13 = TypeCodec[T13].toType(value._13)
          val t14 = TypeCodec[T14].toType(value._14)
          val t15 = TypeCodec[T15].toType(value._15)
          val t16 = TypeCodec[T16].toType(value._16)
          import t1.Underlying as A1
          import t2.Underlying as A2
          import t3.Underlying as A3
          import t4.Underlying as A4
          import t5.Underlying as A5
          import t6.Underlying as A6
          import t7.Underlying as A7
          import t8.Underlying as A8
          import t9.Underlying as A9
          import t10.Underlying as A10
          import t11.Underlying as A11
          import t12.Underlying as A12
          import t13.Underlying as A13
          import t14.Underlying as A14
          import t15.Underlying as A15
          import t16.Underlying as A16
          quoted.Type.of[(A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16)].asInstanceOf[Type[B]]
        }
        override def fromType[B](B: Type[B]): Option[
          Existential.UpperBounded[(T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16), Id]
        ] =
          B match {
            case '[(a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13, a14, a15, a16)] =>
              for {
                v1 <- TypeCodec[T1].fromType(summon[Type[a1]])
                v2 <- TypeCodec[T2].fromType(summon[Type[a2]])
                v3 <- TypeCodec[T3].fromType(summon[Type[a3]])
                v4 <- TypeCodec[T4].fromType(summon[Type[a4]])
                v5 <- TypeCodec[T5].fromType(summon[Type[a5]])
                v6 <- TypeCodec[T6].fromType(summon[Type[a6]])
                v7 <- TypeCodec[T7].fromType(summon[Type[a7]])
                v8 <- TypeCodec[T8].fromType(summon[Type[a8]])
                v9 <- TypeCodec[T9].fromType(summon[Type[a9]])
                v10 <- TypeCodec[T10].fromType(summon[Type[a10]])
                v11 <- TypeCodec[T11].fromType(summon[Type[a11]])
                v12 <- TypeCodec[T12].fromType(summon[Type[a12]])
                v13 <- TypeCodec[T13].fromType(summon[Type[a13]])
                v14 <- TypeCodec[T14].fromType(summon[Type[a14]])
                v15 <- TypeCodec[T15].fromType(summon[Type[a15]])
                v16 <- TypeCodec[T16].fromType(summon[Type[a16]])
              } yield Existential.UpperBounded[
                (T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16),
                Id,
                (T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16)
              ](
                (
                  v1.value.asInstanceOf[T1],
                  v2.value.asInstanceOf[T2],
                  v3.value.asInstanceOf[T3],
                  v4.value.asInstanceOf[T4],
                  v5.value.asInstanceOf[T5],
                  v6.value.asInstanceOf[T6],
                  v7.value.asInstanceOf[T7],
                  v8.value.asInstanceOf[T8],
                  v9.value.asInstanceOf[T9],
                  v10.value.asInstanceOf[T10],
                  v11.value.asInstanceOf[T11],
                  v12.value.asInstanceOf[T12],
                  v13.value.asInstanceOf[T13],
                  v14.value.asInstanceOf[T14],
                  v15.value.asInstanceOf[T15],
                  v16.value.asInstanceOf[T16]
                )
              )(using B.asInstanceOf[Type[(T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16)]])
            case _ => None
          }
      }

    override def Tuple17Codec[
        T1: TypeCodec,
        T2: TypeCodec,
        T3: TypeCodec,
        T4: TypeCodec,
        T5: TypeCodec,
        T6: TypeCodec,
        T7: TypeCodec,
        T8: TypeCodec,
        T9: TypeCodec,
        T10: TypeCodec,
        T11: TypeCodec,
        T12: TypeCodec,
        T13: TypeCodec,
        T14: TypeCodec,
        T15: TypeCodec,
        T16: TypeCodec,
        T17: TypeCodec
    ]: TypeCodec[(T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17)] =
      new TypeCodec[(T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17)] {
        override def toType[B <: (T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17)](
            value: B
        ): Type[B] = {
          val t1 = TypeCodec[T1].toType(value._1)
          val t2 = TypeCodec[T2].toType(value._2)
          val t3 = TypeCodec[T3].toType(value._3)
          val t4 = TypeCodec[T4].toType(value._4)
          val t5 = TypeCodec[T5].toType(value._5)
          val t6 = TypeCodec[T6].toType(value._6)
          val t7 = TypeCodec[T7].toType(value._7)
          val t8 = TypeCodec[T8].toType(value._8)
          val t9 = TypeCodec[T9].toType(value._9)
          val t10 = TypeCodec[T10].toType(value._10)
          val t11 = TypeCodec[T11].toType(value._11)
          val t12 = TypeCodec[T12].toType(value._12)
          val t13 = TypeCodec[T13].toType(value._13)
          val t14 = TypeCodec[T14].toType(value._14)
          val t15 = TypeCodec[T15].toType(value._15)
          val t16 = TypeCodec[T16].toType(value._16)
          val t17 = TypeCodec[T17].toType(value._17)
          import t1.Underlying as A1
          import t2.Underlying as A2
          import t3.Underlying as A3
          import t4.Underlying as A4
          import t5.Underlying as A5
          import t6.Underlying as A6
          import t7.Underlying as A7
          import t8.Underlying as A8
          import t9.Underlying as A9
          import t10.Underlying as A10
          import t11.Underlying as A11
          import t12.Underlying as A12
          import t13.Underlying as A13
          import t14.Underlying as A14
          import t15.Underlying as A15
          import t16.Underlying as A16
          import t17.Underlying as A17
          quoted.Type
            .of[(A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17)]
            .asInstanceOf[Type[B]]
        }
        override def fromType[B](B: Type[B]): Option[
          Existential.UpperBounded[(T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17), Id]
        ] =
          B match {
            case '[(a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13, a14, a15, a16, a17)] =>
              for {
                v1 <- TypeCodec[T1].fromType(summon[Type[a1]])
                v2 <- TypeCodec[T2].fromType(summon[Type[a2]])
                v3 <- TypeCodec[T3].fromType(summon[Type[a3]])
                v4 <- TypeCodec[T4].fromType(summon[Type[a4]])
                v5 <- TypeCodec[T5].fromType(summon[Type[a5]])
                v6 <- TypeCodec[T6].fromType(summon[Type[a6]])
                v7 <- TypeCodec[T7].fromType(summon[Type[a7]])
                v8 <- TypeCodec[T8].fromType(summon[Type[a8]])
                v9 <- TypeCodec[T9].fromType(summon[Type[a9]])
                v10 <- TypeCodec[T10].fromType(summon[Type[a10]])
                v11 <- TypeCodec[T11].fromType(summon[Type[a11]])
                v12 <- TypeCodec[T12].fromType(summon[Type[a12]])
                v13 <- TypeCodec[T13].fromType(summon[Type[a13]])
                v14 <- TypeCodec[T14].fromType(summon[Type[a14]])
                v15 <- TypeCodec[T15].fromType(summon[Type[a15]])
                v16 <- TypeCodec[T16].fromType(summon[Type[a16]])
                v17 <- TypeCodec[T17].fromType(summon[Type[a17]])
              } yield Existential.UpperBounded[
                (T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17),
                Id,
                (T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17)
              ](
                (
                  v1.value.asInstanceOf[T1],
                  v2.value.asInstanceOf[T2],
                  v3.value.asInstanceOf[T3],
                  v4.value.asInstanceOf[T4],
                  v5.value.asInstanceOf[T5],
                  v6.value.asInstanceOf[T6],
                  v7.value.asInstanceOf[T7],
                  v8.value.asInstanceOf[T8],
                  v9.value.asInstanceOf[T9],
                  v10.value.asInstanceOf[T10],
                  v11.value.asInstanceOf[T11],
                  v12.value.asInstanceOf[T12],
                  v13.value.asInstanceOf[T13],
                  v14.value.asInstanceOf[T14],
                  v15.value.asInstanceOf[T15],
                  v16.value.asInstanceOf[T16],
                  v17.value.asInstanceOf[T17]
                )
              )(using
                B.asInstanceOf[Type[(T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17)]]
              )
            case _ => None
          }
      }

    override def Tuple18Codec[
        T1: TypeCodec,
        T2: TypeCodec,
        T3: TypeCodec,
        T4: TypeCodec,
        T5: TypeCodec,
        T6: TypeCodec,
        T7: TypeCodec,
        T8: TypeCodec,
        T9: TypeCodec,
        T10: TypeCodec,
        T11: TypeCodec,
        T12: TypeCodec,
        T13: TypeCodec,
        T14: TypeCodec,
        T15: TypeCodec,
        T16: TypeCodec,
        T17: TypeCodec,
        T18: TypeCodec
    ]: TypeCodec[(T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18)] =
      new TypeCodec[(T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18)] {
        override def toType[B <: (T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18)](
            value: B
        ): Type[B] = {
          val t1 = TypeCodec[T1].toType(value._1)
          val t2 = TypeCodec[T2].toType(value._2)
          val t3 = TypeCodec[T3].toType(value._3)
          val t4 = TypeCodec[T4].toType(value._4)
          val t5 = TypeCodec[T5].toType(value._5)
          val t6 = TypeCodec[T6].toType(value._6)
          val t7 = TypeCodec[T7].toType(value._7)
          val t8 = TypeCodec[T8].toType(value._8)
          val t9 = TypeCodec[T9].toType(value._9)
          val t10 = TypeCodec[T10].toType(value._10)
          val t11 = TypeCodec[T11].toType(value._11)
          val t12 = TypeCodec[T12].toType(value._12)
          val t13 = TypeCodec[T13].toType(value._13)
          val t14 = TypeCodec[T14].toType(value._14)
          val t15 = TypeCodec[T15].toType(value._15)
          val t16 = TypeCodec[T16].toType(value._16)
          val t17 = TypeCodec[T17].toType(value._17)
          val t18 = TypeCodec[T18].toType(value._18)
          import t1.Underlying as A1
          import t2.Underlying as A2
          import t3.Underlying as A3
          import t4.Underlying as A4
          import t5.Underlying as A5
          import t6.Underlying as A6
          import t7.Underlying as A7
          import t8.Underlying as A8
          import t9.Underlying as A9
          import t10.Underlying as A10
          import t11.Underlying as A11
          import t12.Underlying as A12
          import t13.Underlying as A13
          import t14.Underlying as A14
          import t15.Underlying as A15
          import t16.Underlying as A16
          import t17.Underlying as A17
          import t18.Underlying as A18
          quoted.Type
            .of[(A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18)]
            .asInstanceOf[Type[B]]
        }
        override def fromType[B](B: Type[B]): Option[Existential.UpperBounded[
          (T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18),
          Id
        ]] =
          B match {
            case '[(a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13, a14, a15, a16, a17, a18)] =>
              for {
                v1 <- TypeCodec[T1].fromType(summon[Type[a1]])
                v2 <- TypeCodec[T2].fromType(summon[Type[a2]])
                v3 <- TypeCodec[T3].fromType(summon[Type[a3]])
                v4 <- TypeCodec[T4].fromType(summon[Type[a4]])
                v5 <- TypeCodec[T5].fromType(summon[Type[a5]])
                v6 <- TypeCodec[T6].fromType(summon[Type[a6]])
                v7 <- TypeCodec[T7].fromType(summon[Type[a7]])
                v8 <- TypeCodec[T8].fromType(summon[Type[a8]])
                v9 <- TypeCodec[T9].fromType(summon[Type[a9]])
                v10 <- TypeCodec[T10].fromType(summon[Type[a10]])
                v11 <- TypeCodec[T11].fromType(summon[Type[a11]])
                v12 <- TypeCodec[T12].fromType(summon[Type[a12]])
                v13 <- TypeCodec[T13].fromType(summon[Type[a13]])
                v14 <- TypeCodec[T14].fromType(summon[Type[a14]])
                v15 <- TypeCodec[T15].fromType(summon[Type[a15]])
                v16 <- TypeCodec[T16].fromType(summon[Type[a16]])
                v17 <- TypeCodec[T17].fromType(summon[Type[a17]])
                v18 <- TypeCodec[T18].fromType(summon[Type[a18]])
              } yield Existential.UpperBounded[
                (T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18),
                Id,
                (T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18)
              ](
                (
                  v1.value.asInstanceOf[T1],
                  v2.value.asInstanceOf[T2],
                  v3.value.asInstanceOf[T3],
                  v4.value.asInstanceOf[T4],
                  v5.value.asInstanceOf[T5],
                  v6.value.asInstanceOf[T6],
                  v7.value.asInstanceOf[T7],
                  v8.value.asInstanceOf[T8],
                  v9.value.asInstanceOf[T9],
                  v10.value.asInstanceOf[T10],
                  v11.value.asInstanceOf[T11],
                  v12.value.asInstanceOf[T12],
                  v13.value.asInstanceOf[T13],
                  v14.value.asInstanceOf[T14],
                  v15.value.asInstanceOf[T15],
                  v16.value.asInstanceOf[T16],
                  v17.value.asInstanceOf[T17],
                  v18.value.asInstanceOf[T18]
                )
              )(using
                B.asInstanceOf[Type[(T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18)]]
              )
            case _ => None
          }
      }

    override def Tuple19Codec[
        T1: TypeCodec,
        T2: TypeCodec,
        T3: TypeCodec,
        T4: TypeCodec,
        T5: TypeCodec,
        T6: TypeCodec,
        T7: TypeCodec,
        T8: TypeCodec,
        T9: TypeCodec,
        T10: TypeCodec,
        T11: TypeCodec,
        T12: TypeCodec,
        T13: TypeCodec,
        T14: TypeCodec,
        T15: TypeCodec,
        T16: TypeCodec,
        T17: TypeCodec,
        T18: TypeCodec,
        T19: TypeCodec
    ]: TypeCodec[(T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19)] =
      new TypeCodec[(T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19)] {
        override def toType[
            B <: (T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19)
        ](value: B): Type[B] = {
          val t1 = TypeCodec[T1].toType(value._1)
          val t2 = TypeCodec[T2].toType(value._2)
          val t3 = TypeCodec[T3].toType(value._3)
          val t4 = TypeCodec[T4].toType(value._4)
          val t5 = TypeCodec[T5].toType(value._5)
          val t6 = TypeCodec[T6].toType(value._6)
          val t7 = TypeCodec[T7].toType(value._7)
          val t8 = TypeCodec[T8].toType(value._8)
          val t9 = TypeCodec[T9].toType(value._9)
          val t10 = TypeCodec[T10].toType(value._10)
          val t11 = TypeCodec[T11].toType(value._11)
          val t12 = TypeCodec[T12].toType(value._12)
          val t13 = TypeCodec[T13].toType(value._13)
          val t14 = TypeCodec[T14].toType(value._14)
          val t15 = TypeCodec[T15].toType(value._15)
          val t16 = TypeCodec[T16].toType(value._16)
          val t17 = TypeCodec[T17].toType(value._17)
          val t18 = TypeCodec[T18].toType(value._18)
          val t19 = TypeCodec[T19].toType(value._19)
          import t1.Underlying as A1
          import t2.Underlying as A2
          import t3.Underlying as A3
          import t4.Underlying as A4
          import t5.Underlying as A5
          import t6.Underlying as A6
          import t7.Underlying as A7
          import t8.Underlying as A8
          import t9.Underlying as A9
          import t10.Underlying as A10
          import t11.Underlying as A11
          import t12.Underlying as A12
          import t13.Underlying as A13
          import t14.Underlying as A14
          import t15.Underlying as A15
          import t16.Underlying as A16
          import t17.Underlying as A17
          import t18.Underlying as A18
          import t19.Underlying as A19
          quoted.Type
            .of[(A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19)]
            .asInstanceOf[Type[B]]
        }
        override def fromType[B](B: Type[B]): Option[Existential.UpperBounded[
          (T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19),
          Id
        ]] =
          B match {
            case '[(a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13, a14, a15, a16, a17, a18, a19)] =>
              for {
                v1 <- TypeCodec[T1].fromType(summon[Type[a1]])
                v2 <- TypeCodec[T2].fromType(summon[Type[a2]])
                v3 <- TypeCodec[T3].fromType(summon[Type[a3]])
                v4 <- TypeCodec[T4].fromType(summon[Type[a4]])
                v5 <- TypeCodec[T5].fromType(summon[Type[a5]])
                v6 <- TypeCodec[T6].fromType(summon[Type[a6]])
                v7 <- TypeCodec[T7].fromType(summon[Type[a7]])
                v8 <- TypeCodec[T8].fromType(summon[Type[a8]])
                v9 <- TypeCodec[T9].fromType(summon[Type[a9]])
                v10 <- TypeCodec[T10].fromType(summon[Type[a10]])
                v11 <- TypeCodec[T11].fromType(summon[Type[a11]])
                v12 <- TypeCodec[T12].fromType(summon[Type[a12]])
                v13 <- TypeCodec[T13].fromType(summon[Type[a13]])
                v14 <- TypeCodec[T14].fromType(summon[Type[a14]])
                v15 <- TypeCodec[T15].fromType(summon[Type[a15]])
                v16 <- TypeCodec[T16].fromType(summon[Type[a16]])
                v17 <- TypeCodec[T17].fromType(summon[Type[a17]])
                v18 <- TypeCodec[T18].fromType(summon[Type[a18]])
                v19 <- TypeCodec[T19].fromType(summon[Type[a19]])
              } yield Existential.UpperBounded[
                (T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19),
                Id,
                (T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19)
              ](
                (
                  v1.value.asInstanceOf[T1],
                  v2.value.asInstanceOf[T2],
                  v3.value.asInstanceOf[T3],
                  v4.value.asInstanceOf[T4],
                  v5.value.asInstanceOf[T5],
                  v6.value.asInstanceOf[T6],
                  v7.value.asInstanceOf[T7],
                  v8.value.asInstanceOf[T8],
                  v9.value.asInstanceOf[T9],
                  v10.value.asInstanceOf[T10],
                  v11.value.asInstanceOf[T11],
                  v12.value.asInstanceOf[T12],
                  v13.value.asInstanceOf[T13],
                  v14.value.asInstanceOf[T14],
                  v15.value.asInstanceOf[T15],
                  v16.value.asInstanceOf[T16],
                  v17.value.asInstanceOf[T17],
                  v18.value.asInstanceOf[T18],
                  v19.value.asInstanceOf[T19]
                )
              )(using
                B.asInstanceOf[Type[
                  (T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19)
                ]]
              )
            case _ => None
          }
      }

    override def Tuple20Codec[
        T1: TypeCodec,
        T2: TypeCodec,
        T3: TypeCodec,
        T4: TypeCodec,
        T5: TypeCodec,
        T6: TypeCodec,
        T7: TypeCodec,
        T8: TypeCodec,
        T9: TypeCodec,
        T10: TypeCodec,
        T11: TypeCodec,
        T12: TypeCodec,
        T13: TypeCodec,
        T14: TypeCodec,
        T15: TypeCodec,
        T16: TypeCodec,
        T17: TypeCodec,
        T18: TypeCodec,
        T19: TypeCodec,
        T20: TypeCodec
    ]: TypeCodec[(T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19, T20)] =
      new TypeCodec[(T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19, T20)] {
        override def toType[
            B <: (T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19, T20)
        ](value: B): Type[B] = {
          val t1 = TypeCodec[T1].toType(value._1)
          val t2 = TypeCodec[T2].toType(value._2)
          val t3 = TypeCodec[T3].toType(value._3)
          val t4 = TypeCodec[T4].toType(value._4)
          val t5 = TypeCodec[T5].toType(value._5)
          val t6 = TypeCodec[T6].toType(value._6)
          val t7 = TypeCodec[T7].toType(value._7)
          val t8 = TypeCodec[T8].toType(value._8)
          val t9 = TypeCodec[T9].toType(value._9)
          val t10 = TypeCodec[T10].toType(value._10)
          val t11 = TypeCodec[T11].toType(value._11)
          val t12 = TypeCodec[T12].toType(value._12)
          val t13 = TypeCodec[T13].toType(value._13)
          val t14 = TypeCodec[T14].toType(value._14)
          val t15 = TypeCodec[T15].toType(value._15)
          val t16 = TypeCodec[T16].toType(value._16)
          val t17 = TypeCodec[T17].toType(value._17)
          val t18 = TypeCodec[T18].toType(value._18)
          val t19 = TypeCodec[T19].toType(value._19)
          val t20 = TypeCodec[T20].toType(value._20)
          import t1.Underlying as A1
          import t2.Underlying as A2
          import t3.Underlying as A3
          import t4.Underlying as A4
          import t5.Underlying as A5
          import t6.Underlying as A6
          import t7.Underlying as A7
          import t8.Underlying as A8
          import t9.Underlying as A9
          import t10.Underlying as A10
          import t11.Underlying as A11
          import t12.Underlying as A12
          import t13.Underlying as A13
          import t14.Underlying as A14
          import t15.Underlying as A15
          import t16.Underlying as A16
          import t17.Underlying as A17
          import t18.Underlying as A18
          import t19.Underlying as A19
          import t20.Underlying as A20
          quoted.Type
            .of[(A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20)]
            .asInstanceOf[Type[B]]
        }
        override def fromType[B](B: Type[B]): Option[Existential.UpperBounded[
          (T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19, T20),
          Id
        ]] =
          B match {
            case '[(a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13, a14, a15, a16, a17, a18, a19, a20)] =>
              for {
                v1 <- TypeCodec[T1].fromType(summon[Type[a1]])
                v2 <- TypeCodec[T2].fromType(summon[Type[a2]])
                v3 <- TypeCodec[T3].fromType(summon[Type[a3]])
                v4 <- TypeCodec[T4].fromType(summon[Type[a4]])
                v5 <- TypeCodec[T5].fromType(summon[Type[a5]])
                v6 <- TypeCodec[T6].fromType(summon[Type[a6]])
                v7 <- TypeCodec[T7].fromType(summon[Type[a7]])
                v8 <- TypeCodec[T8].fromType(summon[Type[a8]])
                v9 <- TypeCodec[T9].fromType(summon[Type[a9]])
                v10 <- TypeCodec[T10].fromType(summon[Type[a10]])
                v11 <- TypeCodec[T11].fromType(summon[Type[a11]])
                v12 <- TypeCodec[T12].fromType(summon[Type[a12]])
                v13 <- TypeCodec[T13].fromType(summon[Type[a13]])
                v14 <- TypeCodec[T14].fromType(summon[Type[a14]])
                v15 <- TypeCodec[T15].fromType(summon[Type[a15]])
                v16 <- TypeCodec[T16].fromType(summon[Type[a16]])
                v17 <- TypeCodec[T17].fromType(summon[Type[a17]])
                v18 <- TypeCodec[T18].fromType(summon[Type[a18]])
                v19 <- TypeCodec[T19].fromType(summon[Type[a19]])
                v20 <- TypeCodec[T20].fromType(summon[Type[a20]])
              } yield Existential.UpperBounded[
                (T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19, T20),
                Id,
                (T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19, T20)
              ](
                (
                  v1.value.asInstanceOf[T1],
                  v2.value.asInstanceOf[T2],
                  v3.value.asInstanceOf[T3],
                  v4.value.asInstanceOf[T4],
                  v5.value.asInstanceOf[T5],
                  v6.value.asInstanceOf[T6],
                  v7.value.asInstanceOf[T7],
                  v8.value.asInstanceOf[T8],
                  v9.value.asInstanceOf[T9],
                  v10.value.asInstanceOf[T10],
                  v11.value.asInstanceOf[T11],
                  v12.value.asInstanceOf[T12],
                  v13.value.asInstanceOf[T13],
                  v14.value.asInstanceOf[T14],
                  v15.value.asInstanceOf[T15],
                  v16.value.asInstanceOf[T16],
                  v17.value.asInstanceOf[T17],
                  v18.value.asInstanceOf[T18],
                  v19.value.asInstanceOf[T19],
                  v20.value.asInstanceOf[T20]
                )
              )(using
                B.asInstanceOf[Type[
                  (T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19, T20)
                ]]
              )
            case _ => None
          }
      }

    override def Tuple21Codec[
        T1: TypeCodec,
        T2: TypeCodec,
        T3: TypeCodec,
        T4: TypeCodec,
        T5: TypeCodec,
        T6: TypeCodec,
        T7: TypeCodec,
        T8: TypeCodec,
        T9: TypeCodec,
        T10: TypeCodec,
        T11: TypeCodec,
        T12: TypeCodec,
        T13: TypeCodec,
        T14: TypeCodec,
        T15: TypeCodec,
        T16: TypeCodec,
        T17: TypeCodec,
        T18: TypeCodec,
        T19: TypeCodec,
        T20: TypeCodec,
        T21: TypeCodec
    ]: TypeCodec[(T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19, T20, T21)] =
      new TypeCodec[(T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19, T20, T21)] {
        override def toType[
            B <: (T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19, T20, T21)
        ](value: B): Type[B] = {
          val t1 = TypeCodec[T1].toType(value._1)
          val t2 = TypeCodec[T2].toType(value._2)
          val t3 = TypeCodec[T3].toType(value._3)
          val t4 = TypeCodec[T4].toType(value._4)
          val t5 = TypeCodec[T5].toType(value._5)
          val t6 = TypeCodec[T6].toType(value._6)
          val t7 = TypeCodec[T7].toType(value._7)
          val t8 = TypeCodec[T8].toType(value._8)
          val t9 = TypeCodec[T9].toType(value._9)
          val t10 = TypeCodec[T10].toType(value._10)
          val t11 = TypeCodec[T11].toType(value._11)
          val t12 = TypeCodec[T12].toType(value._12)
          val t13 = TypeCodec[T13].toType(value._13)
          val t14 = TypeCodec[T14].toType(value._14)
          val t15 = TypeCodec[T15].toType(value._15)
          val t16 = TypeCodec[T16].toType(value._16)
          val t17 = TypeCodec[T17].toType(value._17)
          val t18 = TypeCodec[T18].toType(value._18)
          val t19 = TypeCodec[T19].toType(value._19)
          val t20 = TypeCodec[T20].toType(value._20)
          val t21 = TypeCodec[T21].toType(value._21)
          import t1.Underlying as A1
          import t2.Underlying as A2
          import t3.Underlying as A3
          import t4.Underlying as A4
          import t5.Underlying as A5
          import t6.Underlying as A6
          import t7.Underlying as A7
          import t8.Underlying as A8
          import t9.Underlying as A9
          import t10.Underlying as A10
          import t11.Underlying as A11
          import t12.Underlying as A12
          import t13.Underlying as A13
          import t14.Underlying as A14
          import t15.Underlying as A15
          import t16.Underlying as A16
          import t17.Underlying as A17
          import t18.Underlying as A18
          import t19.Underlying as A19
          import t20.Underlying as A20
          import t21.Underlying as A21
          quoted.Type
            .of[(A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21)]
            .asInstanceOf[Type[B]]
        }
        override def fromType[B](B: Type[B]): Option[Existential.UpperBounded[
          (T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19, T20, T21),
          Id
        ]] =
          B match {
            case '[(a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13, a14, a15, a16, a17, a18, a19, a20, a21)] =>
              for {
                v1 <- TypeCodec[T1].fromType(summon[Type[a1]])
                v2 <- TypeCodec[T2].fromType(summon[Type[a2]])
                v3 <- TypeCodec[T3].fromType(summon[Type[a3]])
                v4 <- TypeCodec[T4].fromType(summon[Type[a4]])
                v5 <- TypeCodec[T5].fromType(summon[Type[a5]])
                v6 <- TypeCodec[T6].fromType(summon[Type[a6]])
                v7 <- TypeCodec[T7].fromType(summon[Type[a7]])
                v8 <- TypeCodec[T8].fromType(summon[Type[a8]])
                v9 <- TypeCodec[T9].fromType(summon[Type[a9]])
                v10 <- TypeCodec[T10].fromType(summon[Type[a10]])
                v11 <- TypeCodec[T11].fromType(summon[Type[a11]])
                v12 <- TypeCodec[T12].fromType(summon[Type[a12]])
                v13 <- TypeCodec[T13].fromType(summon[Type[a13]])
                v14 <- TypeCodec[T14].fromType(summon[Type[a14]])
                v15 <- TypeCodec[T15].fromType(summon[Type[a15]])
                v16 <- TypeCodec[T16].fromType(summon[Type[a16]])
                v17 <- TypeCodec[T17].fromType(summon[Type[a17]])
                v18 <- TypeCodec[T18].fromType(summon[Type[a18]])
                v19 <- TypeCodec[T19].fromType(summon[Type[a19]])
                v20 <- TypeCodec[T20].fromType(summon[Type[a20]])
                v21 <- TypeCodec[T21].fromType(summon[Type[a21]])
              } yield Existential.UpperBounded[
                (T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19, T20, T21),
                Id,
                (T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19, T20, T21)
              ](
                (
                  v1.value.asInstanceOf[T1],
                  v2.value.asInstanceOf[T2],
                  v3.value.asInstanceOf[T3],
                  v4.value.asInstanceOf[T4],
                  v5.value.asInstanceOf[T5],
                  v6.value.asInstanceOf[T6],
                  v7.value.asInstanceOf[T7],
                  v8.value.asInstanceOf[T8],
                  v9.value.asInstanceOf[T9],
                  v10.value.asInstanceOf[T10],
                  v11.value.asInstanceOf[T11],
                  v12.value.asInstanceOf[T12],
                  v13.value.asInstanceOf[T13],
                  v14.value.asInstanceOf[T14],
                  v15.value.asInstanceOf[T15],
                  v16.value.asInstanceOf[T16],
                  v17.value.asInstanceOf[T17],
                  v18.value.asInstanceOf[T18],
                  v19.value.asInstanceOf[T19],
                  v20.value.asInstanceOf[T20],
                  v21.value.asInstanceOf[T21]
                )
              )(using
                B.asInstanceOf[Type[
                  (T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19, T20, T21)
                ]]
              )
            case _ => None
          }
      }

    override def Tuple22Codec[
        T1: TypeCodec,
        T2: TypeCodec,
        T3: TypeCodec,
        T4: TypeCodec,
        T5: TypeCodec,
        T6: TypeCodec,
        T7: TypeCodec,
        T8: TypeCodec,
        T9: TypeCodec,
        T10: TypeCodec,
        T11: TypeCodec,
        T12: TypeCodec,
        T13: TypeCodec,
        T14: TypeCodec,
        T15: TypeCodec,
        T16: TypeCodec,
        T17: TypeCodec,
        T18: TypeCodec,
        T19: TypeCodec,
        T20: TypeCodec,
        T21: TypeCodec,
        T22: TypeCodec
    ]: TypeCodec[
      (T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19, T20, T21, T22)
    ] =
      new TypeCodec[
        (T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19, T20, T21, T22)
      ] {
        override def toType[
            B <: (T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19, T20, T21, T22)
        ](value: B): Type[B] = {
          val t1 = TypeCodec[T1].toType(value._1)
          val t2 = TypeCodec[T2].toType(value._2)
          val t3 = TypeCodec[T3].toType(value._3)
          val t4 = TypeCodec[T4].toType(value._4)
          val t5 = TypeCodec[T5].toType(value._5)
          val t6 = TypeCodec[T6].toType(value._6)
          val t7 = TypeCodec[T7].toType(value._7)
          val t8 = TypeCodec[T8].toType(value._8)
          val t9 = TypeCodec[T9].toType(value._9)
          val t10 = TypeCodec[T10].toType(value._10)
          val t11 = TypeCodec[T11].toType(value._11)
          val t12 = TypeCodec[T12].toType(value._12)
          val t13 = TypeCodec[T13].toType(value._13)
          val t14 = TypeCodec[T14].toType(value._14)
          val t15 = TypeCodec[T15].toType(value._15)
          val t16 = TypeCodec[T16].toType(value._16)
          val t17 = TypeCodec[T17].toType(value._17)
          val t18 = TypeCodec[T18].toType(value._18)
          val t19 = TypeCodec[T19].toType(value._19)
          val t20 = TypeCodec[T20].toType(value._20)
          val t21 = TypeCodec[T21].toType(value._21)
          val t22 = TypeCodec[T22].toType(value._22)
          import t1.Underlying as A1
          import t2.Underlying as A2
          import t3.Underlying as A3
          import t4.Underlying as A4
          import t5.Underlying as A5
          import t6.Underlying as A6
          import t7.Underlying as A7
          import t8.Underlying as A8
          import t9.Underlying as A9
          import t10.Underlying as A10
          import t11.Underlying as A11
          import t12.Underlying as A12
          import t13.Underlying as A13
          import t14.Underlying as A14
          import t15.Underlying as A15
          import t16.Underlying as A16
          import t17.Underlying as A17
          import t18.Underlying as A18
          import t19.Underlying as A19
          import t20.Underlying as A20
          import t21.Underlying as A21
          import t22.Underlying as A22
          quoted.Type
            .of[(A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21, A22)]
            .asInstanceOf[Type[B]]
        }
        override def fromType[B](B: Type[B]): Option[Existential.UpperBounded[
          (T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19, T20, T21, T22),
          Id
        ]] =
          B match {
            case '[(
                    a1,
                    a2,
                    a3,
                    a4,
                    a5,
                    a6,
                    a7,
                    a8,
                    a9,
                    a10,
                    a11,
                    a12,
                    a13,
                    a14,
                    a15,
                    a16,
                    a17,
                    a18,
                    a19,
                    a20,
                    a21,
                    a22
                )] =>
              for {
                v1 <- TypeCodec[T1].fromType(summon[Type[a1]])
                v2 <- TypeCodec[T2].fromType(summon[Type[a2]])
                v3 <- TypeCodec[T3].fromType(summon[Type[a3]])
                v4 <- TypeCodec[T4].fromType(summon[Type[a4]])
                v5 <- TypeCodec[T5].fromType(summon[Type[a5]])
                v6 <- TypeCodec[T6].fromType(summon[Type[a6]])
                v7 <- TypeCodec[T7].fromType(summon[Type[a7]])
                v8 <- TypeCodec[T8].fromType(summon[Type[a8]])
                v9 <- TypeCodec[T9].fromType(summon[Type[a9]])
                v10 <- TypeCodec[T10].fromType(summon[Type[a10]])
                v11 <- TypeCodec[T11].fromType(summon[Type[a11]])
                v12 <- TypeCodec[T12].fromType(summon[Type[a12]])
                v13 <- TypeCodec[T13].fromType(summon[Type[a13]])
                v14 <- TypeCodec[T14].fromType(summon[Type[a14]])
                v15 <- TypeCodec[T15].fromType(summon[Type[a15]])
                v16 <- TypeCodec[T16].fromType(summon[Type[a16]])
                v17 <- TypeCodec[T17].fromType(summon[Type[a17]])
                v18 <- TypeCodec[T18].fromType(summon[Type[a18]])
                v19 <- TypeCodec[T19].fromType(summon[Type[a19]])
                v20 <- TypeCodec[T20].fromType(summon[Type[a20]])
                v21 <- TypeCodec[T21].fromType(summon[Type[a21]])
                v22 <- TypeCodec[T22].fromType(summon[Type[a22]])
              } yield Existential.UpperBounded[
                (T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19, T20, T21, T22),
                Id,
                (T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19, T20, T21, T22)
              ](
                (
                  v1.value.asInstanceOf[T1],
                  v2.value.asInstanceOf[T2],
                  v3.value.asInstanceOf[T3],
                  v4.value.asInstanceOf[T4],
                  v5.value.asInstanceOf[T5],
                  v6.value.asInstanceOf[T6],
                  v7.value.asInstanceOf[T7],
                  v8.value.asInstanceOf[T8],
                  v9.value.asInstanceOf[T9],
                  v10.value.asInstanceOf[T10],
                  v11.value.asInstanceOf[T11],
                  v12.value.asInstanceOf[T12],
                  v13.value.asInstanceOf[T13],
                  v14.value.asInstanceOf[T14],
                  v15.value.asInstanceOf[T15],
                  v16.value.asInstanceOf[T16],
                  v17.value.asInstanceOf[T17],
                  v18.value.asInstanceOf[T18],
                  v19.value.asInstanceOf[T19],
                  v20.value.asInstanceOf[T20],
                  v21.value.asInstanceOf[T21],
                  v22.value.asInstanceOf[T22]
                )
              )(using
                B.asInstanceOf[Type[
                  (T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19, T20, T21, T22)
                ]]
              )
            case _ => None
          }
      }

    override def OptionCodec[A: TypeCodec]: TypeCodec[Option[A]] =
      new TypeCodec[Option[A]] {
        private val someCodec = SomeCodec[A]
        private val noneCodec = NoneCodec
        override def toType[B <: Option[A]](value: B): Type[B] = value match {
          case s: Some[A @unchecked] => someCodec.toType(s).asInstanceOf[Type[B]]
          case None                  => noneCodec.toType(None).asInstanceOf[Type[B]]
        }
        override def fromType[B](B: Type[B]): Option[Existential.UpperBounded[Option[A], Id]] =
          noneCodec
            .fromType(B)
            .map(_.asInstanceOf[Existential.UpperBounded[Option[A], Id]])
            .orElse(someCodec.fromType(B).map(_.asInstanceOf[Existential.UpperBounded[Option[A], Id]]))
            .orElse(B match {
              case '[Option[a]] =>
                // Cannot decode Option[A] without knowing if it is Some or None
                None
              case _ => None
            })
      }

    override def SomeCodec[A: TypeCodec]: TypeCodec[Some[A]] =
      new TypeCodec[Some[A]] {
        override def toType[B <: Some[A]](value: B): Type[B] = {
          val ta = TypeCodec[A].toType(value.value)
          import ta.Underlying as A0
          quoted.Type.of[Some[A0]].asInstanceOf[Type[B]]
        }
        override def fromType[B](B: Type[B]): Option[Existential.UpperBounded[Some[A], Id]] =
          B match {
            case '[Some[a]] =>
              TypeCodec[A].fromType(summon[Type[a]]).map { v =>
                Existential.UpperBounded[Some[A], Id, Some[A]](Some(v.value.asInstanceOf[A]))(using
                  B.asInstanceOf[Type[Some[A]]]
                )
              }
            case _ => None
          }
      }

    override lazy val NoneCodec: TypeCodec[None.type] = new TypeCodec[None.type] {
      override def toType[B <: None.type](value: B): Type[B] = quoted.Type.of[None.type].asInstanceOf[Type[B]]
      override def fromType[B](B: Type[B]): Option[Existential.UpperBounded[None.type, Id]] =
        B match {
          case '[None.type] =>
            Some(Existential.UpperBounded[None.type, Id, None.type](None)(using B.asInstanceOf[Type[None.type]]))
          case _ => None
        }
    }

    override def EitherCodec[L: TypeCodec: Type, R: TypeCodec: Type]: TypeCodec[Either[L, R]] =
      new TypeCodec[Either[L, R]] {
        private val leftCodec = LeftCodec[L, R]
        private val rightCodec = RightCodec[L, R]
        override def toType[B <: Either[L, R]](value: B): Type[B] = value match {
          case l: Left[L @unchecked, R @unchecked]  => leftCodec.toType(l).asInstanceOf[Type[B]]
          case r: Right[L @unchecked, R @unchecked] => rightCodec.toType(r).asInstanceOf[Type[B]]
        }
        override def fromType[B](B: Type[B]): Option[Existential.UpperBounded[Either[L, R], Id]] =
          leftCodec
            .fromType(B)
            .map(_.asInstanceOf[Existential.UpperBounded[Either[L, R], Id]])
            .orElse(rightCodec.fromType(B).map(_.asInstanceOf[Existential.UpperBounded[Either[L, R], Id]]))
      }

    override def LeftCodec[L: TypeCodec: Type, R: TypeCodec: Type]: TypeCodec[Left[L, R]] =
      new TypeCodec[Left[L, R]] {
        override def toType[B <: Left[L, R]](value: B): Type[B] = {
          val tl = TypeCodec[L].toType(value.value)
          import tl.Underlying as L0
          quoted.Type.of[Left[L0, R]].asInstanceOf[Type[B]]
        }
        override def fromType[B](B: Type[B]): Option[Existential.UpperBounded[Left[L, R], Id]] =
          B match {
            case '[Left[l, ?]] =>
              TypeCodec[L].fromType(summon[Type[l]]).map { v =>
                Existential.UpperBounded[Left[L, R], Id, Left[L, R]](Left(v.value.asInstanceOf[L]))(using
                  B.asInstanceOf[Type[Left[L, R]]]
                )
              }
            case _ => None
          }
      }

    override def RightCodec[L: TypeCodec: Type, R: TypeCodec: Type]: TypeCodec[Right[L, R]] =
      new TypeCodec[Right[L, R]] {
        override def toType[B <: Right[L, R]](value: B): Type[B] = {
          val tr = TypeCodec[R].toType(value.value)
          import tr.Underlying as R0
          quoted.Type.of[Right[L, R0]].asInstanceOf[Type[B]]
        }
        override def fromType[B](B: Type[B]): Option[Existential.UpperBounded[Right[L, R], Id]] =
          B match {
            case '[Right[?, r]] =>
              TypeCodec[R].fromType(summon[Type[r]]).map { v =>
                Existential.UpperBounded[Right[L, R], Id, Right[L, R]](Right(v.value.asInstanceOf[R]))(using
                  B.asInstanceOf[Type[Right[L, R]]]
                )
              }
            case _ => None
          }
      }
  }
}
