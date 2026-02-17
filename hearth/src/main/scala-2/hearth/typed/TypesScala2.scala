package hearth
package typed

import hearth.fp.Id
import hearth.treeprinter.SyntaxHighlight

trait TypesScala2 extends Types { this: MacroCommonsScala2 =>

  import c.universe.*

  final override type Type[A] = c.WeakTypeTag[A]

  object Type extends TypeModule {

    object platformSpecific {

      /** It is surprisingly ridiculous but I've found no other way of telling whether I am looking at enum abstract
        * class or its value, since EVERYTHING else looks the same: parent is not abstract, everyone is static, everyone
        * has the same baseClasses, everyone reports to have public primaryConstructor (which is <none>). The only
        * different in behavior is that one prints com.my.Enum and another com.my.Enum(MyValue).
        */
      val javaEnumRegexpFormat = raw"^(.+)\((.+)\)$$".r

      /** Workaround for <https://issues.scala-lang.org/browse/SI-7755> and
        * <https://github.com/scalalandio/chimney/issues/562> and similar.
        */
      def forceTypeSymbolInitialization[A: Type]: Unit = forceTypeSymbolInitialization(
        UntypedType.fromTyped[A].typeSymbol
      )
      def forceTypeSymbolInitialization(s: Symbol): Unit = s.typeSignature

      final class LiteralCodec[U: Type] extends TypeCodec[U] {
        def toType[A <: U](value: A): Type[A] =
          UntypedType.toTyped(c.universe.internal.constantType(Constant(value.asInstanceOf[AnyVal])))
        def fromType[A](A: Type[A]): Option[Existential.UpperBounded[U, Id]] =
          if (A <:< Type[U]) {
            scala.util
              .Try(
                A.tpe
                  .asInstanceOf[scala.reflect.internal.Types#ConstantType]
                  .value // Constant
                  .value // scala.Any
                  .asInstanceOf[U]
              )
              .toOption
              .map(Existential.UpperBounded[U, Id, U](_))
          } else None
      }
    }
    import platformSpecific.*

    override def shortName[A: Type]: String = {
      val tpe = Type[A].tpe
      tpe.toString match {
        case javaEnumRegexpFormat(_, valueName) if tpe.typeSymbol.isJavaEnum => valueName
        case _                                                               => tpe.dealias.typeSymbol.name.toString
      }
    }
    override def plainPrint[A: Type]: String = showCodePretty(Type[A].tpe.dealias, SyntaxHighlight.plain)
    override def prettyPrint[A: Type]: String = showCodePretty(Type[A].tpe.dealias, SyntaxHighlight.ANSI)

    override lazy val NullCodec: TypeCodec[Null] = new LiteralCodec[Null]
    override lazy val UnitCodec: TypeCodec[Unit] = new LiteralCodec[Unit]
    override lazy val BooleanCodec: TypeCodec[Boolean] = new LiteralCodec[Boolean]
    override lazy val ByteCodec: TypeCodec[Byte] = new LiteralCodec[Byte]
    override lazy val ShortCodec: TypeCodec[Short] = new LiteralCodec[Short]
    override lazy val IntCodec: TypeCodec[Int] = new LiteralCodec[Int]
    override lazy val LongCodec: TypeCodec[Long] = new LiteralCodec[Long]
    override lazy val FloatCodec: TypeCodec[Float] = new LiteralCodec[Float]
    override lazy val DoubleCodec: TypeCodec[Double] = new LiteralCodec[Double]
    override lazy val CharCodec: TypeCodec[Char] = new LiteralCodec[Char]
    override lazy val StringCodec: TypeCodec[String] = new LiteralCodec[String]

    override def Tuple1Codec[T1: TypeCodec]: TypeCodec[Tuple1[T1]] =
      new TypeCodec[Tuple1[T1]] {
        override def toType[B <: Tuple1[T1]](value: B): Type[B] = {
          val t1 = TypeCodec[T1].toType(value._1)
          UntypedType.toTyped(
            c.universe.appliedType(c.mirror.staticClass("scala.Tuple1").toType.typeConstructor, scala.List(t1.tpe))
          )
        }
        override def fromType[B](B: Type[B]): Option[Existential.UpperBounded[Tuple1[T1], Id]] = {
          val tpe = B.tpe.dealias
          tpe.typeArgs match {
            case scala.List(a1) =>
              for {
                v1 <- TypeCodec[T1].fromType(UntypedType.toTyped(a1))
              } yield Existential.UpperBounded[Tuple1[T1], Id, Tuple1[T1]](
                Tuple1(v1.value.asInstanceOf[T1])
              )(using B.asInstanceOf[Type[Tuple1[T1]]])
            case _ => None
          }
        }
      }

    override def Tuple2Codec[T1: TypeCodec, T2: TypeCodec]: TypeCodec[(T1, T2)] =
      new TypeCodec[(T1, T2)] {
        override def toType[B <: (T1, T2)](value: B): Type[B] = {
          val t1 = TypeCodec[T1].toType(value._1)
          val t2 = TypeCodec[T2].toType(value._2)
          UntypedType.toTyped(
            c.universe
              .appliedType(c.mirror.staticClass("scala.Tuple2").toType.typeConstructor, scala.List(t1.tpe, t2.tpe))
          )
        }
        override def fromType[B](B: Type[B]): Option[Existential.UpperBounded[(T1, T2), Id]] = {
          val tpe = B.tpe.dealias
          tpe.typeArgs match {
            case scala.List(a1, a2) =>
              for {
                v1 <- TypeCodec[T1].fromType(UntypedType.toTyped(a1))
                v2 <- TypeCodec[T2].fromType(UntypedType.toTyped(a2))
              } yield Existential.UpperBounded[(T1, T2), Id, (T1, T2)](
                (v1.value.asInstanceOf[T1], v2.value.asInstanceOf[T2])
              )(using B.asInstanceOf[Type[(T1, T2)]])
            case _ => None
          }
        }
      }

    override def Tuple3Codec[T1: TypeCodec, T2: TypeCodec, T3: TypeCodec]: TypeCodec[(T1, T2, T3)] =
      new TypeCodec[(T1, T2, T3)] {
        override def toType[B <: (T1, T2, T3)](value: B): Type[B] = {
          val t1 = TypeCodec[T1].toType(value._1)
          val t2 = TypeCodec[T2].toType(value._2)
          val t3 = TypeCodec[T3].toType(value._3)
          UntypedType.toTyped(
            c.universe.appliedType(
              c.mirror.staticClass("scala.Tuple3").toType.typeConstructor,
              scala.List(t1.tpe, t2.tpe, t3.tpe)
            )
          )
        }
        override def fromType[B](B: Type[B]): Option[Existential.UpperBounded[(T1, T2, T3), Id]] = {
          val tpe = B.tpe.dealias
          tpe.typeArgs match {
            case scala.List(a1, a2, a3) =>
              for {
                v1 <- TypeCodec[T1].fromType(UntypedType.toTyped(a1))
                v2 <- TypeCodec[T2].fromType(UntypedType.toTyped(a2))
                v3 <- TypeCodec[T3].fromType(UntypedType.toTyped(a3))
              } yield Existential.UpperBounded[(T1, T2, T3), Id, (T1, T2, T3)](
                (v1.value.asInstanceOf[T1], v2.value.asInstanceOf[T2], v3.value.asInstanceOf[T3])
              )(using B.asInstanceOf[Type[(T1, T2, T3)]])
            case _ => None
          }
        }
      }

    override def Tuple4Codec[T1: TypeCodec, T2: TypeCodec, T3: TypeCodec, T4: TypeCodec]: TypeCodec[(T1, T2, T3, T4)] =
      new TypeCodec[(T1, T2, T3, T4)] {
        override def toType[B <: (T1, T2, T3, T4)](value: B): Type[B] = {
          val t1 = TypeCodec[T1].toType(value._1)
          val t2 = TypeCodec[T2].toType(value._2)
          val t3 = TypeCodec[T3].toType(value._3)
          val t4 = TypeCodec[T4].toType(value._4)
          UntypedType.toTyped(
            c.universe
              .appliedType(
                c.mirror.staticClass("scala.Tuple4").toType.typeConstructor,
                scala.List(t1.tpe, t2.tpe, t3.tpe, t4.tpe)
              )
          )
        }
        override def fromType[B](B: Type[B]): Option[Existential.UpperBounded[(T1, T2, T3, T4), Id]] = {
          val tpe = B.tpe.dealias
          tpe.typeArgs match {
            case scala.List(a1, a2, a3, a4) =>
              for {
                v1 <- TypeCodec[T1].fromType(UntypedType.toTyped(a1))
                v2 <- TypeCodec[T2].fromType(UntypedType.toTyped(a2))
                v3 <- TypeCodec[T3].fromType(UntypedType.toTyped(a3))
                v4 <- TypeCodec[T4].fromType(UntypedType.toTyped(a4))
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
          UntypedType.toTyped(
            c.universe.appliedType(
              c.mirror.staticClass("scala.Tuple5").toType.typeConstructor,
              scala.List(t1.tpe, t2.tpe, t3.tpe, t4.tpe, t5.tpe)
            )
          )
        }
        override def fromType[B](B: Type[B]): Option[Existential.UpperBounded[(T1, T2, T3, T4, T5), Id]] = {
          val tpe = B.tpe.dealias
          tpe.typeArgs match {
            case scala.List(a1, a2, a3, a4, a5) =>
              for {
                v1 <- TypeCodec[T1].fromType(UntypedType.toTyped(a1))
                v2 <- TypeCodec[T2].fromType(UntypedType.toTyped(a2))
                v3 <- TypeCodec[T3].fromType(UntypedType.toTyped(a3))
                v4 <- TypeCodec[T4].fromType(UntypedType.toTyped(a4))
                v5 <- TypeCodec[T5].fromType(UntypedType.toTyped(a5))
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
          UntypedType.toTyped(
            c.universe.appliedType(
              c.mirror.staticClass("scala.Tuple6").toType.typeConstructor,
              scala.List(t1.tpe, t2.tpe, t3.tpe, t4.tpe, t5.tpe, t6.tpe)
            )
          )
        }
        override def fromType[B](B: Type[B]): Option[Existential.UpperBounded[(T1, T2, T3, T4, T5, T6), Id]] = {
          val tpe = B.tpe.dealias
          tpe.typeArgs match {
            case scala.List(a1, a2, a3, a4, a5, a6) =>
              for {
                v1 <- TypeCodec[T1].fromType(UntypedType.toTyped(a1))
                v2 <- TypeCodec[T2].fromType(UntypedType.toTyped(a2))
                v3 <- TypeCodec[T3].fromType(UntypedType.toTyped(a3))
                v4 <- TypeCodec[T4].fromType(UntypedType.toTyped(a4))
                v5 <- TypeCodec[T5].fromType(UntypedType.toTyped(a5))
                v6 <- TypeCodec[T6].fromType(UntypedType.toTyped(a6))
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
          UntypedType.toTyped(
            c.universe.appliedType(
              c.mirror.staticClass("scala.Tuple7").toType.typeConstructor,
              scala.List(t1.tpe, t2.tpe, t3.tpe, t4.tpe, t5.tpe, t6.tpe, t7.tpe)
            )
          )
        }
        override def fromType[B](B: Type[B]): Option[Existential.UpperBounded[(T1, T2, T3, T4, T5, T6, T7), Id]] = {
          val tpe = B.tpe.dealias
          tpe.typeArgs match {
            case scala.List(a1, a2, a3, a4, a5, a6, a7) =>
              for {
                v1 <- TypeCodec[T1].fromType(UntypedType.toTyped(a1))
                v2 <- TypeCodec[T2].fromType(UntypedType.toTyped(a2))
                v3 <- TypeCodec[T3].fromType(UntypedType.toTyped(a3))
                v4 <- TypeCodec[T4].fromType(UntypedType.toTyped(a4))
                v5 <- TypeCodec[T5].fromType(UntypedType.toTyped(a5))
                v6 <- TypeCodec[T6].fromType(UntypedType.toTyped(a6))
                v7 <- TypeCodec[T7].fromType(UntypedType.toTyped(a7))
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
          UntypedType.toTyped(
            c.universe.appliedType(
              c.mirror.staticClass("scala.Tuple8").toType.typeConstructor,
              scala.List(t1.tpe, t2.tpe, t3.tpe, t4.tpe, t5.tpe, t6.tpe, t7.tpe, t8.tpe)
            )
          )
        }
        override def fromType[B](B: Type[B]): Option[Existential.UpperBounded[(T1, T2, T3, T4, T5, T6, T7, T8), Id]] = {
          val tpe = B.tpe.dealias
          tpe.typeArgs match {
            case scala.List(a1, a2, a3, a4, a5, a6, a7, a8) =>
              for {
                v1 <- TypeCodec[T1].fromType(UntypedType.toTyped(a1))
                v2 <- TypeCodec[T2].fromType(UntypedType.toTyped(a2))
                v3 <- TypeCodec[T3].fromType(UntypedType.toTyped(a3))
                v4 <- TypeCodec[T4].fromType(UntypedType.toTyped(a4))
                v5 <- TypeCodec[T5].fromType(UntypedType.toTyped(a5))
                v6 <- TypeCodec[T6].fromType(UntypedType.toTyped(a6))
                v7 <- TypeCodec[T7].fromType(UntypedType.toTyped(a7))
                v8 <- TypeCodec[T8].fromType(UntypedType.toTyped(a8))
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
          UntypedType.toTyped(
            c.universe.appliedType(
              c.mirror.staticClass("scala.Tuple9").toType.typeConstructor,
              scala.List(t1.tpe, t2.tpe, t3.tpe, t4.tpe, t5.tpe, t6.tpe, t7.tpe, t8.tpe, t9.tpe)
            )
          )
        }
        override def fromType[B](
            B: Type[B]
        ): Option[Existential.UpperBounded[(T1, T2, T3, T4, T5, T6, T7, T8, T9), Id]] = {
          val tpe = B.tpe.dealias
          tpe.typeArgs match {
            case scala.List(a1, a2, a3, a4, a5, a6, a7, a8, a9) =>
              for {
                v1 <- TypeCodec[T1].fromType(UntypedType.toTyped(a1))
                v2 <- TypeCodec[T2].fromType(UntypedType.toTyped(a2))
                v3 <- TypeCodec[T3].fromType(UntypedType.toTyped(a3))
                v4 <- TypeCodec[T4].fromType(UntypedType.toTyped(a4))
                v5 <- TypeCodec[T5].fromType(UntypedType.toTyped(a5))
                v6 <- TypeCodec[T6].fromType(UntypedType.toTyped(a6))
                v7 <- TypeCodec[T7].fromType(UntypedType.toTyped(a7))
                v8 <- TypeCodec[T8].fromType(UntypedType.toTyped(a8))
                v9 <- TypeCodec[T9].fromType(UntypedType.toTyped(a9))
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
          UntypedType.toTyped(
            c.universe.appliedType(
              c.mirror.staticClass("scala.Tuple10").toType.typeConstructor,
              scala.List(t1.tpe, t2.tpe, t3.tpe, t4.tpe, t5.tpe, t6.tpe, t7.tpe, t8.tpe, t9.tpe, t10.tpe)
            )
          )
        }
        override def fromType[B](
            B: Type[B]
        ): Option[Existential.UpperBounded[(T1, T2, T3, T4, T5, T6, T7, T8, T9, T10), Id]] = {
          val tpe = B.tpe.dealias
          tpe.typeArgs match {
            case scala.List(a1, a2, a3, a4, a5, a6, a7, a8, a9, a10) =>
              for {
                v1 <- TypeCodec[T1].fromType(UntypedType.toTyped(a1))
                v2 <- TypeCodec[T2].fromType(UntypedType.toTyped(a2))
                v3 <- TypeCodec[T3].fromType(UntypedType.toTyped(a3))
                v4 <- TypeCodec[T4].fromType(UntypedType.toTyped(a4))
                v5 <- TypeCodec[T5].fromType(UntypedType.toTyped(a5))
                v6 <- TypeCodec[T6].fromType(UntypedType.toTyped(a6))
                v7 <- TypeCodec[T7].fromType(UntypedType.toTyped(a7))
                v8 <- TypeCodec[T8].fromType(UntypedType.toTyped(a8))
                v9 <- TypeCodec[T9].fromType(UntypedType.toTyped(a9))
                v10 <- TypeCodec[T10].fromType(UntypedType.toTyped(a10))
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
          UntypedType.toTyped(
            c.universe.appliedType(
              c.mirror.staticClass("scala.Tuple11").toType.typeConstructor,
              scala.List(t1.tpe, t2.tpe, t3.tpe, t4.tpe, t5.tpe, t6.tpe, t7.tpe, t8.tpe, t9.tpe, t10.tpe, t11.tpe)
            )
          )
        }
        override def fromType[B](
            B: Type[B]
        ): Option[Existential.UpperBounded[(T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11), Id]] = {
          val tpe = B.tpe.dealias
          tpe.typeArgs match {
            case scala.List(a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11) =>
              for {
                v1 <- TypeCodec[T1].fromType(UntypedType.toTyped(a1))
                v2 <- TypeCodec[T2].fromType(UntypedType.toTyped(a2))
                v3 <- TypeCodec[T3].fromType(UntypedType.toTyped(a3))
                v4 <- TypeCodec[T4].fromType(UntypedType.toTyped(a4))
                v5 <- TypeCodec[T5].fromType(UntypedType.toTyped(a5))
                v6 <- TypeCodec[T6].fromType(UntypedType.toTyped(a6))
                v7 <- TypeCodec[T7].fromType(UntypedType.toTyped(a7))
                v8 <- TypeCodec[T8].fromType(UntypedType.toTyped(a8))
                v9 <- TypeCodec[T9].fromType(UntypedType.toTyped(a9))
                v10 <- TypeCodec[T10].fromType(UntypedType.toTyped(a10))
                v11 <- TypeCodec[T11].fromType(UntypedType.toTyped(a11))
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
          UntypedType.toTyped(
            c.universe.appliedType(
              c.mirror.staticClass("scala.Tuple12").toType.typeConstructor,
              scala.List(
                t1.tpe,
                t2.tpe,
                t3.tpe,
                t4.tpe,
                t5.tpe,
                t6.tpe,
                t7.tpe,
                t8.tpe,
                t9.tpe,
                t10.tpe,
                t11.tpe,
                t12.tpe
              )
            )
          )
        }
        override def fromType[B](
            B: Type[B]
        ): Option[Existential.UpperBounded[(T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12), Id]] = {
          val tpe = B.tpe.dealias
          tpe.typeArgs match {
            case scala.List(a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12) =>
              for {
                v1 <- TypeCodec[T1].fromType(UntypedType.toTyped(a1))
                v2 <- TypeCodec[T2].fromType(UntypedType.toTyped(a2))
                v3 <- TypeCodec[T3].fromType(UntypedType.toTyped(a3))
                v4 <- TypeCodec[T4].fromType(UntypedType.toTyped(a4))
                v5 <- TypeCodec[T5].fromType(UntypedType.toTyped(a5))
                v6 <- TypeCodec[T6].fromType(UntypedType.toTyped(a6))
                v7 <- TypeCodec[T7].fromType(UntypedType.toTyped(a7))
                v8 <- TypeCodec[T8].fromType(UntypedType.toTyped(a8))
                v9 <- TypeCodec[T9].fromType(UntypedType.toTyped(a9))
                v10 <- TypeCodec[T10].fromType(UntypedType.toTyped(a10))
                v11 <- TypeCodec[T11].fromType(UntypedType.toTyped(a11))
                v12 <- TypeCodec[T12].fromType(UntypedType.toTyped(a12))
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
          UntypedType.toTyped(
            c.universe.appliedType(
              c.mirror.staticClass("scala.Tuple13").toType.typeConstructor,
              scala.List(
                t1.tpe,
                t2.tpe,
                t3.tpe,
                t4.tpe,
                t5.tpe,
                t6.tpe,
                t7.tpe,
                t8.tpe,
                t9.tpe,
                t10.tpe,
                t11.tpe,
                t12.tpe,
                t13.tpe
              )
            )
          )
        }
        override def fromType[B](
            B: Type[B]
        ): Option[Existential.UpperBounded[(T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13), Id]] = {
          val tpe = B.tpe.dealias
          tpe.typeArgs match {
            case scala.List(a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13) =>
              for {
                v1 <- TypeCodec[T1].fromType(UntypedType.toTyped(a1))
                v2 <- TypeCodec[T2].fromType(UntypedType.toTyped(a2))
                v3 <- TypeCodec[T3].fromType(UntypedType.toTyped(a3))
                v4 <- TypeCodec[T4].fromType(UntypedType.toTyped(a4))
                v5 <- TypeCodec[T5].fromType(UntypedType.toTyped(a5))
                v6 <- TypeCodec[T6].fromType(UntypedType.toTyped(a6))
                v7 <- TypeCodec[T7].fromType(UntypedType.toTyped(a7))
                v8 <- TypeCodec[T8].fromType(UntypedType.toTyped(a8))
                v9 <- TypeCodec[T9].fromType(UntypedType.toTyped(a9))
                v10 <- TypeCodec[T10].fromType(UntypedType.toTyped(a10))
                v11 <- TypeCodec[T11].fromType(UntypedType.toTyped(a11))
                v12 <- TypeCodec[T12].fromType(UntypedType.toTyped(a12))
                v13 <- TypeCodec[T13].fromType(UntypedType.toTyped(a13))
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
          UntypedType.toTyped(
            c.universe.appliedType(
              c.mirror.staticClass("scala.Tuple14").toType.typeConstructor,
              scala.List(
                t1.tpe,
                t2.tpe,
                t3.tpe,
                t4.tpe,
                t5.tpe,
                t6.tpe,
                t7.tpe,
                t8.tpe,
                t9.tpe,
                t10.tpe,
                t11.tpe,
                t12.tpe,
                t13.tpe,
                t14.tpe
              )
            )
          )
        }
        override def fromType[B](
            B: Type[B]
        ): Option[Existential.UpperBounded[(T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14), Id]] = {
          val tpe = B.tpe.dealias
          tpe.typeArgs match {
            case scala.List(a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13, a14) =>
              for {
                v1 <- TypeCodec[T1].fromType(UntypedType.toTyped(a1))
                v2 <- TypeCodec[T2].fromType(UntypedType.toTyped(a2))
                v3 <- TypeCodec[T3].fromType(UntypedType.toTyped(a3))
                v4 <- TypeCodec[T4].fromType(UntypedType.toTyped(a4))
                v5 <- TypeCodec[T5].fromType(UntypedType.toTyped(a5))
                v6 <- TypeCodec[T6].fromType(UntypedType.toTyped(a6))
                v7 <- TypeCodec[T7].fromType(UntypedType.toTyped(a7))
                v8 <- TypeCodec[T8].fromType(UntypedType.toTyped(a8))
                v9 <- TypeCodec[T9].fromType(UntypedType.toTyped(a9))
                v10 <- TypeCodec[T10].fromType(UntypedType.toTyped(a10))
                v11 <- TypeCodec[T11].fromType(UntypedType.toTyped(a11))
                v12 <- TypeCodec[T12].fromType(UntypedType.toTyped(a12))
                v13 <- TypeCodec[T13].fromType(UntypedType.toTyped(a13))
                v14 <- TypeCodec[T14].fromType(UntypedType.toTyped(a14))
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
          UntypedType.toTyped(
            c.universe.appliedType(
              c.mirror.staticClass("scala.Tuple15").toType.typeConstructor,
              scala.List(
                t1.tpe,
                t2.tpe,
                t3.tpe,
                t4.tpe,
                t5.tpe,
                t6.tpe,
                t7.tpe,
                t8.tpe,
                t9.tpe,
                t10.tpe,
                t11.tpe,
                t12.tpe,
                t13.tpe,
                t14.tpe,
                t15.tpe
              )
            )
          )
        }
        override def fromType[B](
            B: Type[B]
        ): Option[Existential.UpperBounded[(T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15), Id]] = {
          val tpe = B.tpe.dealias
          tpe.typeArgs match {
            case scala.List(a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13, a14, a15) =>
              for {
                v1 <- TypeCodec[T1].fromType(UntypedType.toTyped(a1))
                v2 <- TypeCodec[T2].fromType(UntypedType.toTyped(a2))
                v3 <- TypeCodec[T3].fromType(UntypedType.toTyped(a3))
                v4 <- TypeCodec[T4].fromType(UntypedType.toTyped(a4))
                v5 <- TypeCodec[T5].fromType(UntypedType.toTyped(a5))
                v6 <- TypeCodec[T6].fromType(UntypedType.toTyped(a6))
                v7 <- TypeCodec[T7].fromType(UntypedType.toTyped(a7))
                v8 <- TypeCodec[T8].fromType(UntypedType.toTyped(a8))
                v9 <- TypeCodec[T9].fromType(UntypedType.toTyped(a9))
                v10 <- TypeCodec[T10].fromType(UntypedType.toTyped(a10))
                v11 <- TypeCodec[T11].fromType(UntypedType.toTyped(a11))
                v12 <- TypeCodec[T12].fromType(UntypedType.toTyped(a12))
                v13 <- TypeCodec[T13].fromType(UntypedType.toTyped(a13))
                v14 <- TypeCodec[T14].fromType(UntypedType.toTyped(a14))
                v15 <- TypeCodec[T15].fromType(UntypedType.toTyped(a15))
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
          UntypedType.toTyped(
            c.universe.appliedType(
              c.mirror.staticClass("scala.Tuple16").toType.typeConstructor,
              scala.List(
                t1.tpe,
                t2.tpe,
                t3.tpe,
                t4.tpe,
                t5.tpe,
                t6.tpe,
                t7.tpe,
                t8.tpe,
                t9.tpe,
                t10.tpe,
                t11.tpe,
                t12.tpe,
                t13.tpe,
                t14.tpe,
                t15.tpe,
                t16.tpe
              )
            )
          )
        }
        override def fromType[B](B: Type[B]): Option[
          Existential.UpperBounded[(T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16), Id]
        ] = {
          val tpe = B.tpe.dealias
          tpe.typeArgs match {
            case scala.List(a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13, a14, a15, a16) =>
              for {
                v1 <- TypeCodec[T1].fromType(UntypedType.toTyped(a1))
                v2 <- TypeCodec[T2].fromType(UntypedType.toTyped(a2))
                v3 <- TypeCodec[T3].fromType(UntypedType.toTyped(a3))
                v4 <- TypeCodec[T4].fromType(UntypedType.toTyped(a4))
                v5 <- TypeCodec[T5].fromType(UntypedType.toTyped(a5))
                v6 <- TypeCodec[T6].fromType(UntypedType.toTyped(a6))
                v7 <- TypeCodec[T7].fromType(UntypedType.toTyped(a7))
                v8 <- TypeCodec[T8].fromType(UntypedType.toTyped(a8))
                v9 <- TypeCodec[T9].fromType(UntypedType.toTyped(a9))
                v10 <- TypeCodec[T10].fromType(UntypedType.toTyped(a10))
                v11 <- TypeCodec[T11].fromType(UntypedType.toTyped(a11))
                v12 <- TypeCodec[T12].fromType(UntypedType.toTyped(a12))
                v13 <- TypeCodec[T13].fromType(UntypedType.toTyped(a13))
                v14 <- TypeCodec[T14].fromType(UntypedType.toTyped(a14))
                v15 <- TypeCodec[T15].fromType(UntypedType.toTyped(a15))
                v16 <- TypeCodec[T16].fromType(UntypedType.toTyped(a16))
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
          UntypedType.toTyped(
            c.universe.appliedType(
              c.mirror.staticClass("scala.Tuple17").toType.typeConstructor,
              scala.List(
                t1.tpe,
                t2.tpe,
                t3.tpe,
                t4.tpe,
                t5.tpe,
                t6.tpe,
                t7.tpe,
                t8.tpe,
                t9.tpe,
                t10.tpe,
                t11.tpe,
                t12.tpe,
                t13.tpe,
                t14.tpe,
                t15.tpe,
                t16.tpe,
                t17.tpe
              )
            )
          )
        }
        override def fromType[B](B: Type[B]): Option[
          Existential.UpperBounded[(T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17), Id]
        ] = {
          val tpe = B.tpe.dealias
          tpe.typeArgs match {
            case scala.List(a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13, a14, a15, a16, a17) =>
              for {
                v1 <- TypeCodec[T1].fromType(UntypedType.toTyped(a1))
                v2 <- TypeCodec[T2].fromType(UntypedType.toTyped(a2))
                v3 <- TypeCodec[T3].fromType(UntypedType.toTyped(a3))
                v4 <- TypeCodec[T4].fromType(UntypedType.toTyped(a4))
                v5 <- TypeCodec[T5].fromType(UntypedType.toTyped(a5))
                v6 <- TypeCodec[T6].fromType(UntypedType.toTyped(a6))
                v7 <- TypeCodec[T7].fromType(UntypedType.toTyped(a7))
                v8 <- TypeCodec[T8].fromType(UntypedType.toTyped(a8))
                v9 <- TypeCodec[T9].fromType(UntypedType.toTyped(a9))
                v10 <- TypeCodec[T10].fromType(UntypedType.toTyped(a10))
                v11 <- TypeCodec[T11].fromType(UntypedType.toTyped(a11))
                v12 <- TypeCodec[T12].fromType(UntypedType.toTyped(a12))
                v13 <- TypeCodec[T13].fromType(UntypedType.toTyped(a13))
                v14 <- TypeCodec[T14].fromType(UntypedType.toTyped(a14))
                v15 <- TypeCodec[T15].fromType(UntypedType.toTyped(a15))
                v16 <- TypeCodec[T16].fromType(UntypedType.toTyped(a16))
                v17 <- TypeCodec[T17].fromType(UntypedType.toTyped(a17))
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
          UntypedType.toTyped(
            c.universe.appliedType(
              c.mirror.staticClass("scala.Tuple18").toType.typeConstructor,
              scala.List(
                t1.tpe,
                t2.tpe,
                t3.tpe,
                t4.tpe,
                t5.tpe,
                t6.tpe,
                t7.tpe,
                t8.tpe,
                t9.tpe,
                t10.tpe,
                t11.tpe,
                t12.tpe,
                t13.tpe,
                t14.tpe,
                t15.tpe,
                t16.tpe,
                t17.tpe,
                t18.tpe
              )
            )
          )
        }
        override def fromType[B](B: Type[B]): Option[Existential.UpperBounded[
          (T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18),
          Id
        ]] = {
          val tpe = B.tpe.dealias
          tpe.typeArgs match {
            case scala.List(a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13, a14, a15, a16, a17, a18) =>
              for {
                v1 <- TypeCodec[T1].fromType(UntypedType.toTyped(a1))
                v2 <- TypeCodec[T2].fromType(UntypedType.toTyped(a2))
                v3 <- TypeCodec[T3].fromType(UntypedType.toTyped(a3))
                v4 <- TypeCodec[T4].fromType(UntypedType.toTyped(a4))
                v5 <- TypeCodec[T5].fromType(UntypedType.toTyped(a5))
                v6 <- TypeCodec[T6].fromType(UntypedType.toTyped(a6))
                v7 <- TypeCodec[T7].fromType(UntypedType.toTyped(a7))
                v8 <- TypeCodec[T8].fromType(UntypedType.toTyped(a8))
                v9 <- TypeCodec[T9].fromType(UntypedType.toTyped(a9))
                v10 <- TypeCodec[T10].fromType(UntypedType.toTyped(a10))
                v11 <- TypeCodec[T11].fromType(UntypedType.toTyped(a11))
                v12 <- TypeCodec[T12].fromType(UntypedType.toTyped(a12))
                v13 <- TypeCodec[T13].fromType(UntypedType.toTyped(a13))
                v14 <- TypeCodec[T14].fromType(UntypedType.toTyped(a14))
                v15 <- TypeCodec[T15].fromType(UntypedType.toTyped(a15))
                v16 <- TypeCodec[T16].fromType(UntypedType.toTyped(a16))
                v17 <- TypeCodec[T17].fromType(UntypedType.toTyped(a17))
                v18 <- TypeCodec[T18].fromType(UntypedType.toTyped(a18))
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
          UntypedType.toTyped(
            c.universe.appliedType(
              c.mirror.staticClass("scala.Tuple19").toType.typeConstructor,
              scala.List(
                t1.tpe,
                t2.tpe,
                t3.tpe,
                t4.tpe,
                t5.tpe,
                t6.tpe,
                t7.tpe,
                t8.tpe,
                t9.tpe,
                t10.tpe,
                t11.tpe,
                t12.tpe,
                t13.tpe,
                t14.tpe,
                t15.tpe,
                t16.tpe,
                t17.tpe,
                t18.tpe,
                t19.tpe
              )
            )
          )
        }
        override def fromType[B](B: Type[B]): Option[Existential.UpperBounded[
          (T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19),
          Id
        ]] = {
          val tpe = B.tpe.dealias
          tpe.typeArgs match {
            case scala.List(a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13, a14, a15, a16, a17, a18, a19) =>
              for {
                v1 <- TypeCodec[T1].fromType(UntypedType.toTyped(a1))
                v2 <- TypeCodec[T2].fromType(UntypedType.toTyped(a2))
                v3 <- TypeCodec[T3].fromType(UntypedType.toTyped(a3))
                v4 <- TypeCodec[T4].fromType(UntypedType.toTyped(a4))
                v5 <- TypeCodec[T5].fromType(UntypedType.toTyped(a5))
                v6 <- TypeCodec[T6].fromType(UntypedType.toTyped(a6))
                v7 <- TypeCodec[T7].fromType(UntypedType.toTyped(a7))
                v8 <- TypeCodec[T8].fromType(UntypedType.toTyped(a8))
                v9 <- TypeCodec[T9].fromType(UntypedType.toTyped(a9))
                v10 <- TypeCodec[T10].fromType(UntypedType.toTyped(a10))
                v11 <- TypeCodec[T11].fromType(UntypedType.toTyped(a11))
                v12 <- TypeCodec[T12].fromType(UntypedType.toTyped(a12))
                v13 <- TypeCodec[T13].fromType(UntypedType.toTyped(a13))
                v14 <- TypeCodec[T14].fromType(UntypedType.toTyped(a14))
                v15 <- TypeCodec[T15].fromType(UntypedType.toTyped(a15))
                v16 <- TypeCodec[T16].fromType(UntypedType.toTyped(a16))
                v17 <- TypeCodec[T17].fromType(UntypedType.toTyped(a17))
                v18 <- TypeCodec[T18].fromType(UntypedType.toTyped(a18))
                v19 <- TypeCodec[T19].fromType(UntypedType.toTyped(a19))
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
          UntypedType.toTyped(
            c.universe.appliedType(
              c.mirror.staticClass("scala.Tuple20").toType.typeConstructor,
              scala.List(
                t1.tpe,
                t2.tpe,
                t3.tpe,
                t4.tpe,
                t5.tpe,
                t6.tpe,
                t7.tpe,
                t8.tpe,
                t9.tpe,
                t10.tpe,
                t11.tpe,
                t12.tpe,
                t13.tpe,
                t14.tpe,
                t15.tpe,
                t16.tpe,
                t17.tpe,
                t18.tpe,
                t19.tpe,
                t20.tpe
              )
            )
          )
        }
        override def fromType[B](B: Type[B]): Option[Existential.UpperBounded[
          (T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19, T20),
          Id
        ]] = {
          val tpe = B.tpe.dealias
          tpe.typeArgs match {
            case scala.List(
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
                  a20
                ) =>
              for {
                v1 <- TypeCodec[T1].fromType(UntypedType.toTyped(a1))
                v2 <- TypeCodec[T2].fromType(UntypedType.toTyped(a2))
                v3 <- TypeCodec[T3].fromType(UntypedType.toTyped(a3))
                v4 <- TypeCodec[T4].fromType(UntypedType.toTyped(a4))
                v5 <- TypeCodec[T5].fromType(UntypedType.toTyped(a5))
                v6 <- TypeCodec[T6].fromType(UntypedType.toTyped(a6))
                v7 <- TypeCodec[T7].fromType(UntypedType.toTyped(a7))
                v8 <- TypeCodec[T8].fromType(UntypedType.toTyped(a8))
                v9 <- TypeCodec[T9].fromType(UntypedType.toTyped(a9))
                v10 <- TypeCodec[T10].fromType(UntypedType.toTyped(a10))
                v11 <- TypeCodec[T11].fromType(UntypedType.toTyped(a11))
                v12 <- TypeCodec[T12].fromType(UntypedType.toTyped(a12))
                v13 <- TypeCodec[T13].fromType(UntypedType.toTyped(a13))
                v14 <- TypeCodec[T14].fromType(UntypedType.toTyped(a14))
                v15 <- TypeCodec[T15].fromType(UntypedType.toTyped(a15))
                v16 <- TypeCodec[T16].fromType(UntypedType.toTyped(a16))
                v17 <- TypeCodec[T17].fromType(UntypedType.toTyped(a17))
                v18 <- TypeCodec[T18].fromType(UntypedType.toTyped(a18))
                v19 <- TypeCodec[T19].fromType(UntypedType.toTyped(a19))
                v20 <- TypeCodec[T20].fromType(UntypedType.toTyped(a20))
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
          UntypedType.toTyped(
            c.universe.appliedType(
              c.mirror.staticClass("scala.Tuple21").toType.typeConstructor,
              scala.List(
                t1.tpe,
                t2.tpe,
                t3.tpe,
                t4.tpe,
                t5.tpe,
                t6.tpe,
                t7.tpe,
                t8.tpe,
                t9.tpe,
                t10.tpe,
                t11.tpe,
                t12.tpe,
                t13.tpe,
                t14.tpe,
                t15.tpe,
                t16.tpe,
                t17.tpe,
                t18.tpe,
                t19.tpe,
                t20.tpe,
                t21.tpe
              )
            )
          )
        }
        override def fromType[B](B: Type[B]): Option[Existential.UpperBounded[
          (T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19, T20, T21),
          Id
        ]] = {
          val tpe = B.tpe.dealias
          tpe.typeArgs match {
            case scala.List(
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
                  a21
                ) =>
              for {
                v1 <- TypeCodec[T1].fromType(UntypedType.toTyped(a1))
                v2 <- TypeCodec[T2].fromType(UntypedType.toTyped(a2))
                v3 <- TypeCodec[T3].fromType(UntypedType.toTyped(a3))
                v4 <- TypeCodec[T4].fromType(UntypedType.toTyped(a4))
                v5 <- TypeCodec[T5].fromType(UntypedType.toTyped(a5))
                v6 <- TypeCodec[T6].fromType(UntypedType.toTyped(a6))
                v7 <- TypeCodec[T7].fromType(UntypedType.toTyped(a7))
                v8 <- TypeCodec[T8].fromType(UntypedType.toTyped(a8))
                v9 <- TypeCodec[T9].fromType(UntypedType.toTyped(a9))
                v10 <- TypeCodec[T10].fromType(UntypedType.toTyped(a10))
                v11 <- TypeCodec[T11].fromType(UntypedType.toTyped(a11))
                v12 <- TypeCodec[T12].fromType(UntypedType.toTyped(a12))
                v13 <- TypeCodec[T13].fromType(UntypedType.toTyped(a13))
                v14 <- TypeCodec[T14].fromType(UntypedType.toTyped(a14))
                v15 <- TypeCodec[T15].fromType(UntypedType.toTyped(a15))
                v16 <- TypeCodec[T16].fromType(UntypedType.toTyped(a16))
                v17 <- TypeCodec[T17].fromType(UntypedType.toTyped(a17))
                v18 <- TypeCodec[T18].fromType(UntypedType.toTyped(a18))
                v19 <- TypeCodec[T19].fromType(UntypedType.toTyped(a19))
                v20 <- TypeCodec[T20].fromType(UntypedType.toTyped(a20))
                v21 <- TypeCodec[T21].fromType(UntypedType.toTyped(a21))
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
          UntypedType.toTyped(
            c.universe.appliedType(
              c.mirror.staticClass("scala.Tuple22").toType.typeConstructor,
              scala.List(
                t1.tpe,
                t2.tpe,
                t3.tpe,
                t4.tpe,
                t5.tpe,
                t6.tpe,
                t7.tpe,
                t8.tpe,
                t9.tpe,
                t10.tpe,
                t11.tpe,
                t12.tpe,
                t13.tpe,
                t14.tpe,
                t15.tpe,
                t16.tpe,
                t17.tpe,
                t18.tpe,
                t19.tpe,
                t20.tpe,
                t21.tpe,
                t22.tpe
              )
            )
          )
        }
        override def fromType[B](B: Type[B]): Option[Existential.UpperBounded[
          (T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19, T20, T21, T22),
          Id
        ]] = {
          val tpe = B.tpe.dealias
          tpe.typeArgs match {
            case scala.List(
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
                ) =>
              for {
                v1 <- TypeCodec[T1].fromType(UntypedType.toTyped(a1))
                v2 <- TypeCodec[T2].fromType(UntypedType.toTyped(a2))
                v3 <- TypeCodec[T3].fromType(UntypedType.toTyped(a3))
                v4 <- TypeCodec[T4].fromType(UntypedType.toTyped(a4))
                v5 <- TypeCodec[T5].fromType(UntypedType.toTyped(a5))
                v6 <- TypeCodec[T6].fromType(UntypedType.toTyped(a6))
                v7 <- TypeCodec[T7].fromType(UntypedType.toTyped(a7))
                v8 <- TypeCodec[T8].fromType(UntypedType.toTyped(a8))
                v9 <- TypeCodec[T9].fromType(UntypedType.toTyped(a9))
                v10 <- TypeCodec[T10].fromType(UntypedType.toTyped(a10))
                v11 <- TypeCodec[T11].fromType(UntypedType.toTyped(a11))
                v12 <- TypeCodec[T12].fromType(UntypedType.toTyped(a12))
                v13 <- TypeCodec[T13].fromType(UntypedType.toTyped(a13))
                v14 <- TypeCodec[T14].fromType(UntypedType.toTyped(a14))
                v15 <- TypeCodec[T15].fromType(UntypedType.toTyped(a15))
                v16 <- TypeCodec[T16].fromType(UntypedType.toTyped(a16))
                v17 <- TypeCodec[T17].fromType(UntypedType.toTyped(a17))
                v18 <- TypeCodec[T18].fromType(UntypedType.toTyped(a18))
                v19 <- TypeCodec[T19].fromType(UntypedType.toTyped(a19))
                v20 <- TypeCodec[T20].fromType(UntypedType.toTyped(a20))
                v21 <- TypeCodec[T21].fromType(UntypedType.toTyped(a21))
                v22 <- TypeCodec[T22].fromType(UntypedType.toTyped(a22))
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
      }

    override def SomeCodec[A: TypeCodec]: TypeCodec[Some[A]] =
      new TypeCodec[Some[A]] {
        override def toType[B <: Some[A]](value: B): Type[B] = {
          val ta = TypeCodec[A].toType(value.value)
          UntypedType.toTyped(
            c.universe.appliedType(c.mirror.staticClass("scala.Some").toType.typeConstructor, scala.List(ta.tpe))
          )
        }
        override def fromType[B](B: Type[B]): Option[Existential.UpperBounded[Some[A], Id]] = {
          val tpe = B.tpe.dealias
          tpe.typeArgs match {
            case scala.List(a) =>
              TypeCodec[A].fromType(UntypedType.toTyped(a)).map { v =>
                Existential.UpperBounded[Some[A], Id, Some[A]](Some(v.value.asInstanceOf[A]))(using
                  B.asInstanceOf[Type[Some[A]]]
                )
              }
            case _ => None
          }
        }
      }

    override lazy val NoneCodec: TypeCodec[None.type] = new TypeCodec[None.type] {
      override def toType[B <: None.type](value: B): Type[B] = Type.of[None.type].asInstanceOf[Type[B]]
      override def fromType[B](B: Type[B]): Option[Existential.UpperBounded[None.type, Id]] =
        if (B.tpe.dealias =:= c.typeOf[None.type])
          Some(Existential.UpperBounded[None.type, Id, None.type](None)(using B.asInstanceOf[Type[None.type]]))
        else None
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
          val tr = Type.of[R]
          UntypedType.toTyped(
            c.universe
              .appliedType(c.mirror.staticClass("scala.util.Left").toType.typeConstructor, scala.List(tl.tpe, tr.tpe))
          )
        }
        override def fromType[B](B: Type[B]): Option[Existential.UpperBounded[Left[L, R], Id]] = {
          val tpe = B.tpe.dealias
          tpe.typeArgs match {
            case scala.List(a, _) =>
              TypeCodec[L].fromType(UntypedType.toTyped(a)).map { v =>
                Existential.UpperBounded[Left[L, R], Id, Left[L, R]](Left(v.value.asInstanceOf[L]))(using
                  B.asInstanceOf[Type[Left[L, R]]]
                )
              }
            case _ => None
          }
        }
      }

    override def RightCodec[L: TypeCodec: Type, R: TypeCodec: Type]: TypeCodec[Right[L, R]] =
      new TypeCodec[Right[L, R]] {
        override def toType[B <: Right[L, R]](value: B): Type[B] = {
          val tl = Type.of[L]
          val tr = TypeCodec[R].toType(value.value)
          UntypedType.toTyped(
            c.universe
              .appliedType(c.mirror.staticClass("scala.util.Right").toType.typeConstructor, scala.List(tl.tpe, tr.tpe))
          )
        }
        override def fromType[B](B: Type[B]): Option[Existential.UpperBounded[Right[L, R], Id]] = {
          val tpe = B.tpe.dealias
          tpe.typeArgs match {
            case scala.List(_, b) =>
              TypeCodec[R].fromType(UntypedType.toTyped(b)).map { v =>
                Existential.UpperBounded[Right[L, R], Id, Right[L, R]](Right(v.value.asInstanceOf[R]))(using
                  B.asInstanceOf[Type[Right[L, R]]]
                )
              }
            case _ => None
          }
        }
      }
  }
}
