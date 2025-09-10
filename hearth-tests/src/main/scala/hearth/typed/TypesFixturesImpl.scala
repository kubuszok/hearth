package hearth
package typed

import hearth.data.Data

/** Fixtured for testing [[TypesSpec]] and [[TypedJvmSpec]]. */
trait TypesFixturesImpl { this: MacroTypedCommons =>

  def testNamesPrinters[A: Type]: Expr[Data] = Expr(
    Data.map(
      "Type.shortName" -> Data(Type.shortName[A]),
      "Type.fcqn" -> Data(Type.fcqn[A]),
      "Type.plainPrint" -> Data(Type.plainPrint[A]),
      "Type.prettyPrint" -> Data(removeAnsiColors(Type.prettyPrint[A]))
    )
  )

  def testClassOfType[A: Type]: Expr[Data] = Expr(
    Data.map(
      "Type.classOfType" -> Data(Type.classOfType[A].fold("not on classpath")(_.toString))
    )
  )

  def testPosition[A: Type]: Expr[Data] = Expr(
    Data.map(
      "Type.position" -> Data(Type.position[A].map(_.prettyPrint).getOrElse("<no position>"))
    )
  )

  def testChildren[A: Type]: Expr[Data] = Expr(
    Data.map(
      "Type.directChildren" -> Type
        .directChildren[A]
        .map(children => Data(children.view.mapValues(value => Data(value.plainPrint)).toMap))
        .getOrElse(Data("<no direct children>")),
      "Type.exhaustiveChildren" -> Type
        .exhaustiveChildren[A]
        .map(children => Data(children.view.mapValues(value => Data(value.plainPrint)).toMap))
        .getOrElse(Data("<no exhaustive children>"))
    )
  )

  def testAnnotations[A: Type]: Expr[Data] = Expr(
    Data.map(
      "Type.annotations" -> Data.list(Type.annotations[A].map(value => Data(value.prettyPrint))*)
    )
  )

  def testFlags[A: Type]: Expr[Data] = try
    Expr(
      Data.map(
        "Type.isPrimitive" -> Data(Type.isPrimitive[A]),
        "Type.isArray" -> Data(Type.isArray[A]),
        "Type.isBuiltIn" -> Data(Type.isBuiltIn[A]),
        "Type.isAbstract" -> Data(Type.isAbstract[A]),
        "Type.isFinal" -> Data(Type.isFinal[A]),
        "Type.isClass" -> Data(Type.isClass[A]),
        "Type.notBuiltInClass" -> Data(Type.notBuiltInClass[A]),
        "Type.isPlainOldJavaObject" -> Data(Type.isPlainOldJavaObject[A]),
        "Type.isJavaBean" -> Data(Type.isJavaBean[A]),
        "Type.isSealed" -> Data(Type.isSealed[A]),
        "Type.isJavaEnum" -> Data(Type.isJavaEnum[A]),
        "Type.isJavaEnumValue" -> Data(Type.isJavaEnumValue[A]),
        "Type.isCase" -> Data(Type.isCase[A]),
        "Type.isObject" -> Data(Type.isObject[A]),
        "Type.isVal" -> Data(Type.isVal[A]),
        "Type.isCaseClass" -> Data(Type.isCaseClass[A]),
        "Type.isCaseObject" -> Data(Type.isCaseObject[A]),
        "Type.isCaseVal" -> Data(Type.isCaseVal[A]),
        "Type.isAvailable(Everywhere)" -> Data(Type.isAvailable[A](Everywhere))
      )
    )
  catch {
    case e: Throwable =>
      e.printStackTrace()
      throw e
  }

  def testComparisons[A: Type, B: Type]: Expr[Data] = Expr(
    Data.map(
      "<:<" -> Data(Type[A] <:< Type[B]),
      "=:=" -> Data(Type[A] =:= Type[B])
    )
  )

  def testBidirectionalCodecs: Expr[Data] = {
    def roundtrip[A: TypeCodec](value: A): Data = {
      val encoded = Type(value)
      val decoded = Type.unapply(encoded)
      Data.map(
        "encoded" -> Data(encoded.prettyPrint),
        "decoded" -> Data(decoded.fold("not decoded")(_.toString))
      )
    }
    Expr(
      Data.map(
        "null" -> roundtrip(null),
        "unit" -> roundtrip(()),
        "boolean" -> roundtrip(true),
        "byte" -> roundtrip(1.toByte),
        "short" -> roundtrip(1.toShort),
        "int" -> roundtrip(1),
        "long" -> roundtrip(1L),
        "float" -> roundtrip(1.toFloat),
        "double" -> roundtrip(1.toDouble),
        "char" -> roundtrip('a'),
        "string" -> roundtrip("a")
      )
    )
  }

  val aStringType = Type.of["a"]
  val bStringType = Type.of["b"]

  def testOneWayCodecs: Expr[Data] = {
    def oneWay[A: TypeCodec](value: A): Data = {
      val encoded = Type(value)
      Data.map(
        "encoded" -> Data(encoded.prettyPrint)
      )
    }
    implicit val a: Type["a"] = aStringType
    implicit val b: Type["b"] = bStringType
    Expr(
      Data.map(
        """Array["a"]""" -> oneWay[Array["a"]](Array["a"]("a")),
        """Seq["a"]""" -> oneWay[Seq["a"]](Seq["a"]("a")),
        """List["a"]""" -> oneWay[List["a"]](List["a"]("a")),
        "Nil" -> oneWay[Nil.type](Nil),
        """Vector["a"]""" -> oneWay[Vector["a"]](Vector["a"]("a")),
        """Map["a", "b"]""" -> oneWay[Map["a", "b"]](Map["a", "b"](("a": "a", "b": "b"))),
        """Set["a"]""" -> oneWay[Set["a"]](Set["a"]("a")),
        """Option["a"]""" -> oneWay[Option["a"]](Option["a"]("a")),
        """Some["a"]""" -> oneWay[Some["a"]](Some["a"]("a")),
        "None" -> oneWay[None.type](None),
        """Either["a", "b"]""" -> oneWay[Either["a", "b"]](Left["a", "b"]("a")),
        """Left["a", "b"]""" -> oneWay[Left["a", "b"]](Left["a", "b"]("a")),
        """Right["a", "b"]""" -> oneWay[Right["a", "b"]](Right["a", "b"]("b"))
      )
    )
  }
}
