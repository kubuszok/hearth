package hearth
package std
package extensions

/** Macro extension providing support for Scala immutable arrays.
  *
  * Supports all Scala immutable arrays, turns them into [[scala.collection.Iterable]] by scala.Predef.wrapArray, and
  * using the summoned [[scala.collection.Factory]] as Factory implementation. Treats them as types without smart
  * constructors.
  *
  * @since 0.3.0
  */
final class IsCollectionProviderForIArray extends StandardMacroExtension { loader =>

  override def extend(ctx: MacroCommons & StdExtensions): Unit = {
    import ctx.*

    IsCollection.registerProvider(new IsCollection.Provider {

      override def name: String = loader.getClass.getName

      private lazy val IArray = Type.Ctor1.of[IArray]

      private def isCollection[A, Item: Type](
          A: Type[A],
          toIterableExpr: Expr[A] => Expr[Iterable[Item]],
          factoryExpr: Expr[scala.collection.Factory[Item, A]],
          buildExpr: Expr[scala.collection.mutable.Builder[Item, A]] => Expr[A]
      ): IsCollection[A] =
        Existential[IsCollectionOf[A, *], Item](new IsCollectionOf[A, Item] {
          override def asIterable(value: Expr[A]): Expr[Iterable[Item]] = toIterableExpr(value)
          override def sizeHintForBuilder(value: Expr[A]): Option[Expr[Int]] = Some(Expr.quote {
            Expr.splice(value).asInstanceOf[IArray[Item]].length
          })
          override def foreach(value: Expr[A])(f: Expr[Item] => Expr[Unit]): Expr[Unit] = Expr.quote {
            val arr = Expr.splice(value).asInstanceOf[IArray[Item]]
            var i = 0
            while i < arr.length do {
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

      override def parse[A](tpe: Type[A]): ProviderResult[IsCollection[A]] = tpe match {
        case IArray(item) =>
          import item.Underlying as Item
          implicit val A: Type[A] = tpe

          // A Factory for an IArray needs a ClassTag.
          Expr
            .summonImplicit(using Type.of[scala.reflect.ClassTag[Item]])
            .toOption match {
            case Some(classTagExpr) =>
              val factoryExpr: Expr[scala.collection.Factory[Item, A]] = Expr
                .quote {
                  scala.collection.Factory
                    .arrayFactory(using Expr.splice(classTagExpr))
                    .asInstanceOf[scala.collection.Factory[Item, A]]
                }
              val buildExpr: Expr[scala.collection.mutable.Builder[Item, A]] => Expr[A] =
                builder => Expr.quote(Expr.splice(builder).result())
              val toIterable = loader.toIterable[A, Item](ctx)(Item)
              ProviderResult.Matched(isCollection(A, toIterable, factoryExpr, buildExpr))
            case None =>
              skippedLazily(s"${tpe.prettyPrint} is IArray[${Item.prettyPrint}] but ClassTag not found")
          }

        // Other types are not (Scala built-in) collections - if they should be supported, another extension can take care of it.
        case _ => skippedLazily(s"${tpe.prettyPrint} is not IArray[_]")
      }
    })
  }

  // Kept out of `extend`: inlined there, its per-element-type quotes made `extend` too large for coverage instrumentation.
  private def toIterable[A, Item](
      ctx: MacroCommons & StdExtensions
  )(item: ctx.Type[Item]): ctx.Expr[A] => ctx.Expr[Iterable[Item]] = {
    import ctx.*
    implicit val Item: Type[Item] = item

    def adjust[I](thunk: Expr[IArray[I]] => Expr[Iterable[I]]): Expr[A] => Expr[Iterable[Item]] =
      thunk.asInstanceOf[Expr[A] => Expr[Iterable[Item]]]
    if Item <:< Type.of[Int] then {
      adjust[Int] { value =>
        Expr.quote {
          scala.Predef.wrapIntArray(Expr.splice(value).unsafeArray)
        }
      }
    } else if Item <:< Type.of[Short] then {
      adjust[Short] { value =>
        Expr.quote {
          scala.Predef.wrapShortArray(Expr.splice(value).unsafeArray)
        }
      }
    } else if Item <:< Type.of[Long] then {
      adjust[Long] { value =>
        Expr.quote {
          scala.Predef.wrapLongArray(Expr.splice(value).unsafeArray)
        }
      }
    } else if Item <:< Type.of[Float] then {
      adjust[Float] { value =>
        Expr.quote {
          scala.Predef.wrapFloatArray(Expr.splice(value).unsafeArray)
        }
      }
    } else if Item <:< Type.of[Double] then {
      adjust[Double] { value =>
        Expr.quote {
          scala.Predef.wrapDoubleArray(Expr.splice(value).unsafeArray)
        }
      }
    } else if Item <:< Type.of[Char] then {
      adjust[Char] { value =>
        Expr.quote {
          scala.Predef.wrapCharArray(Expr.splice(value).unsafeArray)
        }
      }
    } else if Item <:< Type.of[Boolean] then {
      adjust[Boolean] { value =>
        Expr.quote {
          scala.Predef.wrapBooleanArray(Expr.splice(value).unsafeArray)
        }
      }
    } else if Item <:< Type.of[Byte] then {
      adjust[Byte] { value =>
        Expr.quote {
          scala.Predef.wrapByteArray(Expr.splice(value).unsafeArray)
        }
      }
    } else if Item <:< Type.of[Unit] then {
      adjust[Unit] { value =>
        Expr.quote {
          scala.Predef.wrapUnitArray(Expr.splice(value).unsafeArray)
        }
      }
    } else if Item <:< Type.of[AnyRef] then {
      val anyRefItem = Item.asInstanceOf[Type[AnyRef]].as_??<:[AnyRef]
      import anyRefItem.Underlying as AnyRefItem
      adjust[AnyRefItem] { value =>
        Expr.quote {
          scala.Predef.wrapRefArray(Expr.splice(value).unsafeArray)
        }
      }
    } else {
      adjust[Item] { value =>
        Expr.quote {
          scala.Predef.genericWrapArray(Expr.splice(value).unsafeArray.asInstanceOf[Array[Item]])
        }
      }
    }
  }
}
