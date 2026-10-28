package hearth
package std
package extensions

/** Macro extension providing support for Scala arrays.
  *
  * Supports all Scala arrays, turns them into [[scala.collection.Iterable]] by scala.Predef.wrapArray, and using the
  * summoned [[scala.collection.Factory]] as Factory implementation. Treats them as types without smart constructors.
  *
  * @since 0.3.0
  */
final class IsCollectionProviderForArray extends StandardMacroExtension { loader =>

  override def extend(ctx: MacroCommons & StdExtensions): Unit = {
    import ctx.*

    IsCollection.registerProvider(new IsCollection.Provider {

      override def name: String = loader.getClass.getName

      private lazy val Array = Type.Ctor1.of[Array]

      private def isCollection[A, Item: Type](
          A: Type[A],
          toIterableExpr: Expr[A] => Expr[Iterable[Item]],
          factoryExpr: Expr[scala.collection.Factory[Item, A]],
          buildExpr: Expr[scala.collection.mutable.Builder[Item, A]] => Expr[A]
      ): IsCollection[A] =
        Existential[IsCollectionOf[A, *], Item](new IsCollectionOf[A, Item] {
          override def asIterable(value: Expr[A]): Expr[Iterable[Item]] = toIterableExpr(value)
          override def sizeHintForBuilder(value: Expr[A]): Option[Expr[Int]] = Some(Expr.quote {
            Expr.splice(value).asInstanceOf[Array[Item]].length
          })
          override def foreach(value: Expr[A])(f: Expr[Item] => Expr[Unit]): Expr[Unit] = Expr.quote {
            val arr = Expr.splice(value).asInstanceOf[Array[Item]]
            var i = 0
            while (i < arr.length) {
              val item = arr(i)
              Expr.splice(f(Expr.quote(item)))
              i += 1
            }
          }
          // Arrays have no smart constructors, so we just return the array itself.
          override type CtorResult = A
          implicit override val CtorResult: Type[CtorResult] = A
          override def factory: Expr[scala.collection.Factory[Item, CtorResult]] = factoryExpr
          override def build: CtorLikeOf[scala.collection.mutable.Builder[Item, CtorResult], A] =
            CtorLikeOf.PlainValue(buildExpr, None)
        })

      // Cheap sound negative gate: T can only match if Array is among T's base classes (covers Array[X] itself and
      // abstract types bounded by an Array); head-symbol compares are far cheaper than the quote-pattern match.
      private lazy val ArrayCtorUntyped = Array.asUntyped
      override def mightMatch[A](tpe: Type[A]): Boolean =
        tpe.asUntyped.baseClasses.exists(_.sameTypeConstructorAs(ArrayCtorUntyped))

      override def parse[A](tpe: Type[A]): ProviderResult[IsCollection[A]] = tpe match {
        case Array(item) =>
          import item.Underlying as Item
          implicit val A: Type[A] = tpe

          // A Factory for an Array needs a ClassTag.
          Expr
            .summonImplicit(using Type.of[scala.reflect.ClassTag[Item]])
            .toOption match {
            case Some(classTagExpr) =>
              val factoryExpr: Expr[scala.collection.Factory[Item, A]] = Expr
                .quote {
                  scala.collection.Factory.arrayFactory(using Expr.splice(classTagExpr))
                }
                .asInstanceOf[Expr[scala.collection.Factory[Item, A]]]
              val buildExpr: Expr[scala.collection.mutable.Builder[Item, A]] => Expr[A] =
                builder => Expr.quote(Expr.splice(builder).result())
              val toIterable = loader.toIterable[A, Item](ctx)(Item)
              ProviderResult.Matched(isCollection(A, toIterable, factoryExpr, buildExpr))
            case None =>
              skippedLazily(s"${tpe.prettyPrint} is Array[${Item.prettyPrint}] but ClassTag not found")
          }

        // Other types are not (Scala built-in) collections - if they should be supported, another extension can take care of it.
        case _ => skippedLazily(s"${tpe.prettyPrint} is not Array[_]")
      }
    })
  }

  // Kept out of `extend`: inlined there, its per-element-type quotes made `extend` too large for coverage instrumentation.
  private def toIterable[A, Item](
      ctx: MacroCommons & StdExtensions
  )(item: ctx.Type[Item]): ctx.Expr[A] => ctx.Expr[Iterable[Item]] = {
    import ctx.*
    implicit val Item: Type[Item] = item

    def adjust[I](thunk: Expr[Array[I]] => Expr[Iterable[I]]): Expr[A] => Expr[Iterable[Item]] =
      thunk.asInstanceOf[Expr[A] => Expr[Iterable[Item]]]
    if (Item <:< Type.of[Int]) {
      adjust[Int] { value =>
        Expr.quote {
          scala.Predef.wrapIntArray(Expr.splice(value))
        }
      }
    } else if (Item <:< Type.of[Short]) {
      adjust[Short] { value =>
        Expr.quote {
          scala.Predef.wrapShortArray(Expr.splice(value))
        }
      }
    } else if (Item <:< Type.of[Long]) {
      adjust[Long] { value =>
        Expr.quote {
          scala.Predef.wrapLongArray(Expr.splice(value))
        }
      }
    } else if (Item <:< Type.of[Float]) {
      adjust[Float] { value =>
        Expr.quote {
          scala.Predef.wrapFloatArray(Expr.splice(value))
        }
      }
    } else if (Item <:< Type.of[Double]) {
      adjust[Double] { value =>
        Expr.quote {
          scala.Predef.wrapDoubleArray(Expr.splice(value))
        }
      }
    } else if (Item <:< Type.of[Char]) {
      adjust[Char] { value =>
        Expr.quote {
          scala.Predef.wrapCharArray(Expr.splice(value))
        }
      }
    } else if (Item <:< Type.of[Boolean]) {
      adjust[Boolean] { value =>
        Expr.quote {
          scala.Predef.wrapBooleanArray(Expr.splice(value))
        }
      }
    } else if (Item <:< Type.of[Byte]) {
      adjust[Byte] { value =>
        Expr.quote {
          scala.Predef.wrapByteArray(Expr.splice(value))
        }
      }
    } else if (Item <:< Type.of[Unit]) {
      adjust[Unit] { value =>
        Expr.quote {
          scala.Predef.wrapUnitArray(Expr.splice(value))
        }
      }
    } else if (Item <:< Type.of[AnyRef]) {
      val anyRefItem = Item.asInstanceOf[Type[AnyRef]].as_??<:[AnyRef]
      import anyRefItem.Underlying as AnyRefItem
      adjust[AnyRefItem] { value =>
        Expr.quote {
          scala.Predef.wrapRefArray(Expr.splice(value))
        }
      }
    } else {
      adjust[Item] { value =>
        Expr.quote {
          scala.Predef.genericWrapArray(Expr.splice(value))
        }
      }
    }
  }
}
