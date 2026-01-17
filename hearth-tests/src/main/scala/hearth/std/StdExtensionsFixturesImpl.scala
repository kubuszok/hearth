package hearth
package std

import hearth.data.Data

/** Fixtured for testing [[StdExtensionsSpec]]. */
@scala.annotation.nowarn // TODO: remove later?
trait StdExtensionsFixturesImpl { this: MacroCommons & StdExtensions =>

  private val IntType = Type.of[Int]
  private val StringType = Type.of[String]
  private val DataType = Type.of[Data]
  private val BuilderType = Type.Ctor2.of[scala.collection.mutable.Builder]

  Environment.loadStandardExtensions() match {
    case ExtensionLoadingResult.LoaderFailed(error) =>
      Environment.reportErrorAndAbort("Failed to resolve extensions: " + error.toString)
    case ExtensionLoadingResult.SomeFailed(extensions, errors) =>
      Environment.reportErrorAndAbort(
        "Failed to load standard extensions: " + errors.toNonEmptyVector.map(_._2).mkString("\n")
      )
    // case ExtensionLoadingResult.AllLoaded(extensions) =>
    //   Environment.reportInfo("Loaded standard extensions: " + extensions.map(_.getClass.getName).mkString("\n"))
    case _ =>
  }

  def testIsCollection[A: Type](value: Expr[A]): Expr[Data] = Type[A] match {
    case IsMap(isMap) =>
      // TODO: nested imports should be supported in Scala 2 (better printer returns isMap.isMapOf.x instead of isMap.value.x OR isMap.x)
      // import isMap.{Underlying as Pair, value as isMapOf}
      // import isMapOf.{Key, Value, PossibleSmartResult}
      import isMap.Underlying as Pair
      import isMap.value.{Key, Value, PossibleSmartResult}
      // For returning the result
      implicit val dataType: Type[Data] = DataType
      // For upcasting
      implicit val intType: Type[Int] = IntType
      implicit val stringType: Type[String] = StringType
      // For the builder
      implicit val builderType: Type[scala.collection.mutable.Builder[Pair, PossibleSmartResult]] =
        BuilderType[Pair, PossibleSmartResult]

      val iteration = Expr.quote {
        val it = Expr.splice(isMap.value.asIterable(value))
        Data(it.map { pair =>
          val key = Expr.splice(isMap.value.key(Expr.quote(pair)))
          val value = Expr.splice(isMap.value.value(Expr.quote(pair)))
          Data.map(
            "key" -> Data(key.toString),
            "value" -> Data(value.toString)
          )
        }.toList)
      }

      val handleBuilder = handleSmartConstructor(isMap.value.build) { a =>
        Expr.quote(Data(Expr.splice(a).toString))
      }
      val building = if (Key <:< IntType && Value <:< StringType) Expr.quote {
        val key = Expr.splice(Expr(1).upcast[Key])
        val value = Expr.splice(Expr("one").upcast[Value])
        val pair = Expr.splice(isMap.value.pair(Expr.quote(key), Expr.quote(value)))
        val b = Expr.splice(isMap.value.factory).newBuilder
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
      // TODO: same as for IsMap, nested imports should be supported in Scala 2 (better printer returns isCollection.isCollectionOf.x instead of isCollection.value.x OR isCollection.x)
      // import isCollection.{Underlying as Item, value as isCollectionOf}
      // import isCollectionOf.{PossibleSmartResult}
      import isCollection.Underlying as Item
      import isCollection.value.PossibleSmartResult
      // For returning the result
      implicit val dataType: Type[Data] = DataType
      // For upcasting
      implicit val stringType: Type[String] = StringType
      // For the builder
      implicit val builderType: Type[scala.collection.mutable.Builder[Item, PossibleSmartResult]] =
        BuilderType[Item, PossibleSmartResult]

      val iteration = Expr.quote {
        val it = Expr.splice(isCollection.value.asIterable(value))
        Data(it.map { item =>
          Data(item.toString)
        }.toList)
      }

      val handleBuilder = handleSmartConstructor(isCollection.value.build) { a =>
        if (Type[A].isArray)
          Expr.quote(Data(Expr.splice(a).toString.takeWhile(c => c != ';' && c != '@')))
        else Expr.quote(Data(Expr.splice(a).toString))
      }
      val building = if (Item <:< StringType) Expr.quote {
        val b = Expr.splice(isCollection.value.factory).newBuilder
        val item = Expr.splice(Expr("one").upcast[Item])
        b.addOne(item)
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

      val folding = Expr.quote {
        val opt = Expr.splice(value)
        Expr.splice(
          isOption.value.fold[Data](Expr.quote(opt))(
            onEmpty = Expr(Data("empty")),
            onSome = (item: Expr[Item]) =>
              Expr.quote {
                Data(s"some: ${Expr.splice(item).toString}")
              }
          )
        )
      }

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
      val wrapping = if (Inner <:< StringType) Expr.quote {
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
      build: PossibleSmartCtor[Input, Output]
  )(onValid: Expr[Output] => Expr[Data]): Expr[Input] => Expr[Data] = build match {
    case plain @ PossibleSmartCtor.PlainValue(ctor) =>
      (in: Expr[Input]) => onValid(ctor(in))
    // TODO: the rest of known smart constructors
    case _ => _ => Expr(Data("<unhandled smart constructor>"))
  }
}
