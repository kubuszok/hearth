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
final class IsCollectionProviderForIArray extends StandardMacroExtension {

  override def extend(ctx: MacroCommons & StdExtensions): Unit = {
    import ctx.*

    IsCollection.registerProvider(new IsCollection.Provider {

      private lazy val IArray = Type.Ctor1.of[IArray]

      private lazy val Int = Type.of[Int]
      private lazy val Short = Type.of[Short]
      private lazy val Long = Type.of[Long]
      private lazy val Float = Type.of[Float]
      private lazy val Double = Type.of[Double]
      private lazy val Char = Type.of[Char]
      private lazy val Boolean = Type.of[Boolean]
      private lazy val Byte = Type.of[Byte]
      private lazy val Unit = Type.of[Unit]
      private lazy val AnyRef = Type.of[AnyRef]

      private def isCollection[A, Item: Type](
          A: Type[A],
          toIterableExpr: Expr[A] => Expr[Iterable[Item]],
          factoryExpr: Expr[scala.collection.Factory[Item, A]],
          buildExpr: Expr[scala.collection.mutable.Builder[Item, A]] => Expr[A]
      ): IsCollection[A] =
        Existential[IsCollectionOf[A, *], Item](new IsCollectionOf[A, Item] {
          // We will use Array as the collection type, we need to adjust how we convert the collection to Iterable.
          override def asIterable(value: Expr[A]): Expr[Iterable[Item]] = toIterableExpr(value)
          // Arrays have no smart constructors, so we just return the array itself.
          override type PossibleSmartResult = A
          implicit override val PossibleSmartResult: Type[PossibleSmartResult] = A
          override def factory: Expr[scala.collection.Factory[Item, PossibleSmartResult]] = factoryExpr
          override def build: PossibleSmartCtor[scala.collection.mutable.Builder[Item, PossibleSmartResult], A] =
            PossibleSmartCtor.PlainValue(buildExpr)
        })

      override def unapply[A](tpe: Type[A]): Option[IsCollection[A]] = tpe match {
        // All IArrays can be converted to Iterable...
        case IArray(item) =>
          import item.Underlying as Item
          implicit val A: Type[A] = tpe

          // ...but for making Factory we need to access ClassTag...
          Expr
            .summonImplicit(using Type.of[scala.reflect.ClassTag[Item]])
            .toOption
            .map { classTagExpr =>
              // ...and use it to build the collection.
              val factoryExpr: Expr[scala.collection.Factory[Item, A]] = Expr
                .quote {
                  scala.collection.Factory
                    .arrayFactory(using Expr.splice(classTagExpr))
                    .asInstanceOf[scala.collection.Factory[Item, A]]
                }
              val buildExpr: Expr[scala.collection.mutable.Builder[Item, A]] => Expr[A] =
                builder => Expr.quote(Expr.splice(builder).result())
              // Conversion from Array[Item] to Iterable[Item] depends on the item type.
              val toIterable: Expr[A] => Expr[Iterable[Item]] = {
                def adjust[I](thunk: Expr[IArray[I]] => Expr[Iterable[I]]): Expr[A] => Expr[Iterable[Item]] =
                  thunk.asInstanceOf[Expr[A] => Expr[Iterable[Item]]]
                if Item <:< Int then {
                  adjust[Int] { value =>
                    Expr.quote {
                      scala.Predef.wrapIntArray(Expr.splice(value).unsafeArray)
                    }
                  }
                } else if Item <:< Short then {
                  adjust[Short] { value =>
                    Expr.quote {
                      scala.Predef.wrapShortArray(Expr.splice(value).unsafeArray)
                    }
                  }
                } else if Item <:< Long then {
                  adjust[Long] { value =>
                    Expr.quote {
                      scala.Predef.wrapLongArray(Expr.splice(value).unsafeArray)
                    }
                  }
                } else if Item <:< Float then {
                  adjust[Float] { value =>
                    Expr.quote {
                      scala.Predef.wrapFloatArray(Expr.splice(value).unsafeArray)
                    }
                  }
                } else if Item <:< Double then {
                  adjust[Double] { value =>
                    Expr.quote {
                      scala.Predef.wrapDoubleArray(Expr.splice(value).unsafeArray)
                    }
                  }
                } else if Item <:< Char then {
                  adjust[Char] { value =>
                    Expr.quote {
                      scala.Predef.wrapCharArray(Expr.splice(value).unsafeArray)
                    }
                  }
                } else if Item <:< Boolean then {
                  adjust[Boolean] { value =>
                    Expr.quote {
                      scala.Predef.wrapBooleanArray(Expr.splice(value).unsafeArray)
                    }
                  }
                } else if Item <:< Byte then {
                  adjust[Byte] { value =>
                    Expr.quote {
                      scala.Predef.wrapByteArray(Expr.splice(value).unsafeArray)
                    }
                  }
                } else if Item <:< Unit then {
                  adjust[Unit] { value =>
                    Expr.quote {
                      scala.Predef.wrapUnitArray(Expr.splice(value).unsafeArray)
                    }
                  }
                } else if Item <:< AnyRef then {
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
              isCollection(A, toIterable, factoryExpr, buildExpr)
            }

        // Other types are not (Scala built-in) collections - if they should be supported, another extension can take care of it.
        case _ => None
      }
    })
  }
}
