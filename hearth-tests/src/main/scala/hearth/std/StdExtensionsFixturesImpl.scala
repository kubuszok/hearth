package hearth
package std

import hearth.data.Data

/** Fixtures for testing [[StdExtensionsSpec]]. */
trait StdExtensionsFixturesImpl { this: MacroCommons & StdExtensions =>

  private val IntType = Type.of[Int]
  private val StringType = Type.of[String]
  private val DataType = Type.of[Data]
  private val BuilderType = Type.Ctor2.of[scala.collection.mutable.Builder]

  Environment.loadStandardExtensions() match {
    case ExtensionLoadingResult.LoaderFailed(error) =>
      Environment.reportErrorAndAbort("Failed to resolve extensions: " + error.toString)
    case ExtensionLoadingResult.SomeFailed(_, errors) =>
      Environment.reportErrorAndAbort(
        "Failed to load standard extensions: " + errors.toNonEmptyVector.map(_._2).mkString("\n")
      )
    // case ExtensionLoadingResult.AllLoaded(extensions) =>
    //   println("Loaded standard extensions: " + extensions.map(_.getClass.getName).mkString(", "))
    case _ =>
  }

  def testIsCollection[A: Type](value: Expr[A]): Expr[Data] = Type[A] match {
    case IsMap(isMap) =>
      import isMap.{Underlying as Pair, value as isMapOf}
      import isMapOf.{Key, Value, CtorResult}
      // For returning the result
      implicit val dataType: Type[Data] = DataType
      // For upcasting
      implicit val intType: Type[Int] = IntType
      implicit val stringType: Type[String] = StringType
      // For the builder
      implicit val builderType: Type[scala.collection.mutable.Builder[Pair, CtorResult]] =
        BuilderType[Pair, CtorResult]

      val iteration = Expr.quote {
        val it = Expr.splice(isMapOf.asIterable(value))
        Data(it.map { pair =>
          val key = Expr.splice(isMapOf.key(Expr.quote(pair)))
          val value = Expr.splice(isMapOf.value(Expr.quote(pair)))
          Data.map(
            "key" -> Data(key.toString),
            "value" -> Data(value.toString)
          )
        }.toList)
      }

      val handleBuilder = handleSmartConstructor(isMapOf.build) { a =>
        Expr.quote(Data(Expr.splice(a).toString))
      }
      val building = if (Key <:< IntType && Value <:< StringType) Expr.quote {
        val key = Expr.splice(Expr(1).upcast[Key])
        val value = Expr.splice(Expr("one").upcast[Value])
        val pair = Expr.splice(isMapOf.pair(Expr.quote(key), Expr.quote(value)))
        val b = Expr.splice(isMapOf.factory).newBuilder
        b.addOne(pair)
        Expr.splice(handleBuilder(Expr.quote(b)))
      }
      else Expr(Data("<not a map of int and string>"))

      Expr.quote {
        Data.map(
          "iteration" -> Expr.splice(iteration),
          "building" -> Expr.splice(building)
        )
      }
    case IsCollection(isCollection) =>
      import isCollection.{Underlying as Elem, value as isCollectionOf}
      import isCollectionOf.CtorResult
      // For returning the result
      implicit val dataType: Type[Data] = DataType
      // For upcasting
      implicit val stringType: Type[String] = StringType
      // For the builder
      implicit val builderType: Type[scala.collection.mutable.Builder[Elem, CtorResult]] =
        BuilderType[Elem, CtorResult]

      // Build iteration without calling provider for Option/Optional (ctx capture) or EnumSet (pattern var "item" capture) in Scala 2.
      val iteration =
        if (Type[A].plainPrint.startsWith("java.util.EnumSet["))
          Expr.quote {
            val set = Expr.splice(value).asInstanceOf[java.util.Set[Any]]
            val lst = scala.jdk.javaapi.CollectionConverters.asScala(set).toList
            Data(lst.map(elem => Data(elem.toString)).toList)
          }
        else if (Type[A].plainPrint.startsWith("scala.Option["))
          Expr.quote {
            val lst = Expr.splice(value).asInstanceOf[scala.Option[Any]].toList
            Data(lst.map(elem => Data(elem.toString)).toList)
          }
        else if (Type[A].plainPrint.startsWith("java.util.Optional["))
          Expr.quote {
            val o = Expr.splice(value).asInstanceOf[java.util.Optional[Any]]
            val lst = if (o.isPresent) List(o.get()) else Nil
            Data(lst.map(elem => Data(elem.toString)).toList)
          }
        else
          Expr.quote {
            val it = Expr.splice(isCollectionOf.asIterable(value))
            Data(it.map { elem =>
              Data(elem.toString)
            }.toList)
          }

      val handleBuilder = handleSmartConstructor(isCollectionOf.build) { a =>
        if (
          Type[A].isArray || Type[A].plainPrint
            .startsWith("scala.IArray") || Type[A].plainPrint.startsWith("java.util.")
        )
          Expr.quote(Data(Expr.splice(a).toString.takeWhile(c => c != ';' && c != '@' && c != '$')))
        else Expr.quote(Data(Expr.splice(a).toString))
      }
      // For Option/Optional/EnumSet, build without using provider's factory/build to avoid free term capture in Scala 2.
      val building =
        if (Type[A].plainPrint.startsWith("java.util.EnumSet["))
          Expr(Data("<not a collection of string>"))
        else if (Type[A].plainPrint.startsWith("scala.Option[") && Elem <:< StringType)
          Expr.quote(Data(Option(Expr.splice(Expr("one"))).toString))
        else if (Type[A].plainPrint.startsWith("java.util.Optional[") && Elem <:< StringType)
          Expr.quote(Data(java.util.Optional.of(Expr.splice(Expr("one"))).toString))
        else if (Elem <:< StringType) Expr.quote {
          val b = Expr.splice(isCollectionOf.factory).newBuilder
          val one = Expr.splice(Expr("one").upcast[Elem])
          b.addOne(one)
          Expr.splice(handleBuilder(Expr.quote(b)))
        }
        else Expr(Data("<not a collection of string>"))

      Expr.quote {
        Data.map(
          "iteration" -> Expr.splice(iteration),
          "building" -> Expr.splice(building)
        )
      }
    case _ => Expr(Data("<no collection>"))
  }

  def testIsOption[A: Type](value: Expr[A]): Expr[Data] = Type[A] match {
    case IsOption(isOption) =>
      import isOption.Underlying as Item
      // For returning the result
      implicit val dataType: Type[Data] = DataType
      // For upcasting
      implicit val stringType: Type[String] = StringType

      // FIXME: when it had a form:
      // val folding = Expr.quote {
      //   val opt = Expr.splice(value)
      //   Expr.splice(
      //     isOption.value.fold[Data](Expr.quote(opt))(
      //       onEmpty = Expr.quote(Data("empty")),
      //       onSome = (item: Expr[Item]) =>
      //         Expr.quote {
      //           val i = Expr.splice(item)
      //           Data(s"some: " + i.toString)
      //         }
      //     )
      //   )
      // }
      // it would fail with:
      //   Exception occurred while executing macro expansion.
      //   scala.quoted.runtime.impl.ScopeException: Expression created in a splice was used outside of that splice.
      // so we have to fix something about our ctx manipulation logic.
      val folding = isOption.value.fold(value)(
        Expr(Data("empty")),
        (item: Expr[Item]) =>
          Expr.quote {
            val i = Expr.splice(item)
            Data(s"some: " + i.toString)
          }
      )

      val getOrElseTest = if (Item <:< StringType) Expr.quote {
        val opt = Expr.splice(value)
        Data(
          Expr
            .splice(
              isOption.value.getOrElse(Expr.quote(opt))(
                Expr("default").upcast[Item]
              )
            )
            .toString
        )
      }
      else Expr(Data("<not an option of string>"))

      val building = if (Item <:< StringType) Expr.quote {
        val some = Expr.splice(isOption.value.of(Expr("test").upcast[Item]))
        val empty = Expr.splice(isOption.value.empty)
        Data.map(
          "some" -> Data(some.toString),
          "empty" -> Data(empty.toString)
        )
      }
      else Expr(Data("<not an option of string>"))

      Expr.quote {
        Data.map(
          "folding" -> Expr.splice(folding),
          "getOrElse" -> Expr.splice(getOrElseTest),
          "building" -> Expr.splice(building)
        )
      }
    case _ => Expr(Data("<no option>"))
  }

  def testIsEither[A: Type](value: Expr[A]): Expr[Data] = Type[A] match {
    case IsEither(isEither) =>
      import isEither.{LeftValue, RightValue}
      // For returning the result
      implicit val dataType: Type[Data] = DataType
      // For upcasting
      implicit val stringType: Type[String] = StringType
      implicit val intType: Type[Int] = IntType

      val folding = Expr.quote {
        val either = Expr.splice(value)
        Expr.splice(
          isEither.value.fold[Data](Expr.quote(either))(
            onLeft = (left: Expr[LeftValue]) =>
              Expr.quote {
                Data(s"left: ${Expr.splice(left).toString}")
              },
            onRight = (right: Expr[RightValue]) =>
              Expr.quote {
                Data(s"right: ${Expr.splice(right).toString}")
              }
          )
        )
      }

      val getOrElseTest = if (RightValue <:< StringType) Expr.quote {
        val either = Expr.splice(value)
        Data(
          Expr
            .splice(
              isEither.value.getOrElse(Expr.quote(either))(
                Expr("default").upcast[RightValue]
              )
            )
            .toString
        )
      }
      else Expr(Data("<not an either with string right>"))

      val building = if (LeftValue <:< StringType && RightValue <:< IntType) Expr.quote {
        val left = Expr.splice(isEither.value.left(Expr("error").upcast[LeftValue]))
        val right = Expr.splice(isEither.value.right(Expr(42).upcast[RightValue]))
        Data.map(
          "left" -> Data(left.toString),
          "right" -> Data(right.toString)
        )
      }
      else
        Expr(
          Data.map(
            "left" -> Data("<not an either of string and int>"),
            "right" -> Data("<not an either of string and int>")
          )
        )

      Expr.quote {
        Data.map(
          "folding" -> Expr.splice(folding),
          "getOrElse" -> Expr.splice(getOrElseTest),
          "building" -> Expr.splice(building)
        )
      }
    case _ => Expr(Data("<no either>"))
  }

  def testIsValueType[A: Type](value: Expr[A]): Expr[Data] = Type[A] match {
    case IsValueType(isValueType) =>
      import isValueType.Underlying as Inner
      // For returning the result
      implicit val dataType: Type[Data] = DataType
      // For upcasting
      implicit val stringType: Type[String] = StringType

      val unwrapping = Expr.quote {
        val outer = Expr.splice(value)
        val inner = Expr.splice(isValueType.value.unwrap(Expr.quote(outer)))
        Data(s"unwrapped: ${inner.toString}")
      }

      val handleWrap = handleSmartConstructor(isValueType.value.wrap) { a =>
        Expr.quote(Data(Expr.splice(a).toString))
      }
      val wrapping = if (Inner =:= StringType) Expr.quote {
        val inner = Expr.splice(Expr("test").upcast[Inner])
        Expr.splice(handleWrap(Expr.quote(inner)))
      }
      else Expr(Data("<not a value type of string>"))

      Expr.quote {
        Data.map(
          "unwrapping" -> Expr.splice(unwrapping),
          "wrapping" -> Expr.splice(wrapping)
        )
      }
    case _ => Expr(Data("<no value type>"))
  }

  private def handleSmartConstructor[Input: Type, Output: Type](
      build: CtorLikeOf[Input, Output]
  )(onValid: Expr[Output] => Expr[Data]): Expr[Input] => Expr[Data] = build match {
    case CtorLikeOf.PlainValue(ctor, _) =>
      (in: Expr[Input]) => onValid(ctor(in))
    case CtorLikeOf.EitherStringOrValue(ctor, _) =>
      (in: Expr[Input]) =>
        Expr.quote {
          Expr.splice(ctor(in)) match {
            case Right(value) => Expr.splice(onValid(Expr.quote(value)))
            case Left(err)    => Data(s"error: $err")
          }
        }
    // TODO: the rest of known smart constructors
    case _ => _ => Expr(Data("<unhandled smart constructor>"))
  }

  def testCtorLikes[A: Type]: Expr[Data] =
    CtorLikes.unapply(Type[A]) match {
      case Some(ctors) =>
        val ctorInfos = ctors.toList.map { existential =>
          import existential.Underlying as Input
          val ctorType = existential.value match {
            case _: CtorLikeOf.PlainValue[?, ?]                     => "PlainValue"
            case _: CtorLikeOf.EitherStringOrValue[?, ?]            => "EitherStringOrValue"
            case _: CtorLikeOf.EitherIterableStringOrValue[?, ?]    => "EitherIterableStringOrValue"
            case _: CtorLikeOf.EitherThrowableOrValue[?, ?]         => "EitherThrowableOrValue"
            case _: CtorLikeOf.EitherIterableThrowableOrValue[?, ?] => "EitherIterableThrowableOrValue"
            case other                                              => s"Unknown(${other.getClass.getSimpleName})"
          }
          val inputType = Input.prettyPrint
          val methodName = existential.value.method.map(_.value.name).getOrElse("<no method>")
          (ctorType, inputType, methodName)
        }

        // Build ctors list as compile-time constant Data
        val ctorDataList: List[Data] = ctorInfos.map { case (ctorType, inputType, methodName) =>
          Data.map(
            "type" -> Data(ctorType),
            "input" -> Data(inputType),
            "method" -> Data(methodName)
          )
        }

        Expr(
          Data.map(
            "count" -> Data(ctors.size.toString),
            "ctors" -> Data(ctorDataList)
          )
        )
      case None =>
        Expr(Data("<no ctors>"))
    }

  def testParse[A: Type]: Expr[Data] = {
    def resultToData[B](result: ProviderResult[B]): Data = result match {
      case _: ProviderResult.Matched[?]    => Data("matched")
      case ProviderResult.Skipped(reasons) =>
        Data.map(
          "status" -> Data("skipped"),
          "count" -> Data(reasons.toList.size)
        )
    }

    Expr(
      Data.map(
        "IsCollection" -> resultToData(IsCollection.parse[A]),
        "IsOption" -> resultToData(IsOption.parse[A]),
        "IsEither" -> resultToData(IsEither.parse[A]),
        "IsValueType" -> resultToData(IsValueType.parse[A]),
        "CtorLikes" -> resultToData(CtorLikes.parse[A])
      )
    )
  }

  def testLastUnapplyFailure[A: Type]: Expr[Data] = {
    val _ = IsCollection.unapply(Type[A])
    val failure = Option(IsCollection.lastUnapplyFailure)

    failure match {
      case None =>
        Expr(Data.map("status" -> Data("matched"), "hasFailure" -> Data(false)))
      case Some(reasons) =>
        Expr(
          Data.map(
            "status" -> Data("skipped"),
            "hasFailure" -> Data(true),
            "providerCount" -> Data(reasons.toList.size)
          )
        )
    }
  }
}
