package hearth
package std
package extensions

/** Macro extension providing support for Java optional types.
  *
  * Supports [[java.util.Optional]] and its primitive specializations [[java.util.OptionalInt]],
  * [[java.util.OptionalLong]], and [[java.util.OptionalDouble]]. Treats them as types without smart constructors.
  *
  * @since 0.3.0
  */
final class IsOptionProviderForJavaOptional extends StandardMacroExtension { loader =>

  @scala.annotation.nowarn
  override def extend(ctx: MacroCommons & StdExtensions): Unit = {
    import ctx.*

    IsOption.registerProvider(new IsOption.Provider {

      override def name: String = loader.getClass.getName

      private lazy val Optional = Type.Ctor1.of[java.util.Optional]
      private lazy val juOptionalInt = Type.of[java.util.OptionalInt]
      private lazy val juOptionalLong = Type.of[java.util.OptionalLong]
      private lazy val juOptionalDouble = Type.of[java.util.OptionalDouble]

      private lazy val Int = Type.of[Int]
      private lazy val Long = Type.of[Long]
      private lazy val Double = Type.of[Double]

      private def isOption[A: Type, Item: Type](
          toOptional: Expr[A] => Expr[java.util.Optional[Item]],
          fromOptional: Expr[java.util.Optional[Item]] => Expr[A]
      ): IsOption[A] =
        Existential[IsOptionOf[A, *], Item](new IsOptionOf[A, Item] {
          override val empty: Expr[A] =
            fromOptional(Expr.quote(java.util.Optional.empty[Item]))
          override def of(value: Expr[Item]): Expr[A] =
            fromOptional(Expr.quote(java.util.Optional.of(Expr.splice(value))))
          override def fold[B: Type](option: Expr[A])(onEmpty: Expr[B], onSome: Expr[Item] => Expr[B]): Expr[B] =
            Expr.quote {
              Expr
                .splice(toOptional(option))
                .map(item => Expr.splice(onSome(Expr.quote(item))))
                .orElse(Expr.splice(onEmpty))
            }
          override def getOrElse(option: Expr[A])(default: Expr[Item]): Expr[Item] =
            Expr.quote(Expr.splice(toOptional(option)).orElse(Expr.splice(default)))
          override def orElse(option: Expr[A])(default: Expr[A]): Expr[A] =
            fromOptional(Expr.quote(Expr.splice(toOptional(option)).or(() => Expr.splice(toOptional(default)))))
        })

      private def isOptionalInt[A](
          A: Type[A],
          toOptionalInt: Expr[A] => Expr[java.util.OptionalInt],
          fromOptionalInt: Expr[java.util.OptionalInt] => Expr[A]
      ): IsOption[A] =
        Existential[IsOptionOf[A, *], Int](new IsOptionOf[A, Int] {
          override val empty: Expr[A] =
            fromOptionalInt(Expr.quote(java.util.OptionalInt.empty()))
          override def of(value: Expr[Int]): Expr[A] =
            fromOptionalInt(Expr.quote(java.util.OptionalInt.of(Expr.splice(value))))
          override def fold[B: Type](option: Expr[A])(onEmpty: Expr[B], onSome: Expr[Int] => Expr[B]): Expr[B] =
            Expr.quote {
              val opt = Expr.splice(toOptionalInt(option))
              if (opt.isPresent) {
                val item = opt.getAsInt
                Expr.splice(onSome(Expr.quote(item)))
              } else Expr.splice(onEmpty)
            }
          override def getOrElse(option: Expr[A])(default: Expr[Int]): Expr[Int] =
            Expr.quote(Expr.splice(toOptionalInt(option)).orElse(Expr.splice(default)))
          override def orElse(option: Expr[A])(default: Expr[A]): Expr[A] =
            fromOptionalInt(Expr.quote {
              val opt = Expr.splice(toOptionalInt(option))
              if (opt.isPresent) opt
              else Expr.splice(toOptionalInt(default))
            })
        })(using Int)

      private def isOptionalLong[A](
          A: Type[A],
          toOptionalLong: Expr[A] => Expr[java.util.OptionalLong],
          fromOptionalLong: Expr[java.util.OptionalLong] => Expr[A]
      ): IsOption[A] =
        Existential[IsOptionOf[A, *], Long](new IsOptionOf[A, Long] {
          override val empty: Expr[A] =
            fromOptionalLong(Expr.quote(java.util.OptionalLong.empty()))
          override def of(value: Expr[Long]): Expr[A] =
            fromOptionalLong(Expr.quote(java.util.OptionalLong.of(Expr.splice(value))))
          override def fold[B: Type](option: Expr[A])(onEmpty: Expr[B], onSome: Expr[Long] => Expr[B]): Expr[B] =
            Expr.quote {
              val opt = Expr.splice(toOptionalLong(option))
              if (opt.isPresent) {
                val item = opt.getAsLong
                Expr.splice(onSome(Expr.quote(item)))
              } else Expr.splice(onEmpty)
            }
          override def getOrElse(option: Expr[A])(default: Expr[Long]): Expr[Long] =
            Expr.quote(Expr.splice(toOptionalLong(option)).orElse(Expr.splice(default)))
          override def orElse(option: Expr[A])(default: Expr[A]): Expr[A] =
            fromOptionalLong(Expr.quote {
              val opt = Expr.splice(toOptionalLong(option))
              if (opt.isPresent) opt
              else Expr.splice(toOptionalLong(default))
            })
        })(using Long)

      private def isOptionalDouble[A](
          A: Type[A],
          toOptionalDouble: Expr[A] => Expr[java.util.OptionalDouble],
          fromOptionalDouble: Expr[java.util.OptionalDouble] => Expr[A]
      ): IsOption[A] =
        Existential[IsOptionOf[A, *], Double](new IsOptionOf[A, Double] {
          override val empty: Expr[A] =
            fromOptionalDouble(Expr.quote(java.util.OptionalDouble.empty()))
          override def of(value: Expr[Double]): Expr[A] =
            fromOptionalDouble(Expr.quote(java.util.OptionalDouble.of(Expr.splice(value))))
          override def fold[B: Type](option: Expr[A])(onEmpty: Expr[B], onSome: Expr[Double] => Expr[B]): Expr[B] =
            Expr.quote {
              val opt = Expr.splice(toOptionalDouble(option))
              if (opt.isPresent) {
                val item = opt.getAsDouble
                Expr.splice(onSome(Expr.quote(item)))
              } else Expr.splice(onEmpty)
            }
          override def getOrElse(option: Expr[A])(default: Expr[Double]): Expr[Double] =
            Expr.quote(Expr.splice(toOptionalDouble(option)).orElse(Expr.splice(default)))
          override def orElse(option: Expr[A])(default: Expr[A]): Expr[A] =
            fromOptionalDouble(Expr.quote {
              val opt = Expr.splice(toOptionalDouble(option))
              if (opt.isPresent) opt
              else Expr.splice(toOptionalDouble(default))
            })
        })(using Double)

      @scala.annotation.nowarn
      override def parse[A](tpe: Type[A]): ProviderResult[IsOption[A]] = tpe match {
        case _ if tpe =:= juOptionalInt =>
          implicit val A: Type[A] = tpe
          implicit val OptionalInt: Type[java.util.OptionalInt] = juOptionalInt
          ProviderResult.Matched(isOptionalInt[A](A, _.upcast[java.util.OptionalInt], _.upcast[A]))
        case _ if tpe =:= juOptionalLong =>
          implicit val A: Type[A] = tpe
          implicit val OptionalLong: Type[java.util.OptionalLong] = juOptionalLong
          ProviderResult.Matched(isOptionalLong[A](A, _.upcast[java.util.OptionalLong], _.upcast[A]))
        case _ if tpe =:= juOptionalDouble =>
          implicit val A: Type[A] = tpe
          implicit val OptionalDouble: Type[java.util.OptionalDouble] = juOptionalDouble
          ProviderResult.Matched(isOptionalDouble[A](A, _.upcast[java.util.OptionalDouble], _.upcast[A]))
        case Optional(item) =>
          import item.Underlying as Item
          implicit val A: Type[A] = tpe
          implicit val OptionalItem: Type[java.util.Optional[Item]] = Optional[Item]
          ProviderResult.Matched(isOption[A, Item](_.upcast[java.util.Optional[Item]], _.upcast[A]))
        case _ => skipped(s"${tpe.prettyPrint} is not Optional/OptionalInt/OptionalLong/OptionalDouble")
      }
    })
  }
}
