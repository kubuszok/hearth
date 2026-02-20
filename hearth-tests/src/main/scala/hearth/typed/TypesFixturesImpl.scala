package hearth
package typed

import hearth.data.Data

/** Fixtures for testing [[TypesSpec]] and [[TypesJvmSpec]]. */
trait TypesFixturesImpl { this: MacroTypedCommons =>

  // Type methods

  def testNamesPrinters[A: Type]: Expr[Data] = Expr(
    Data.map(
      "Type.shortName" -> Data(Type[A].shortName),
      "Type.fqcn" -> Data(Type[A].fqcn),
      "Type.plainPrint" -> Data(Type[A].plainPrint),
      "Type.prettyPrint" -> Data(removeAnsiColors(Type[A].prettyPrint))
    )
  )

  def testClassOfType[A: Type]: Expr[Data] = Expr(
    Data.map(
      "Type.classOfType" -> Data(Type[A].getRuntimeClass.fold("not on classpath")(_.toString))
    )
  )

  def testPosition[A: Type]: Expr[Data] = Expr(
    Data.map(
      "Type.position" -> Data(Type[A].position.map(_.prettyPrint).getOrElse("<no position>"))
    )
  )

  def testChildren[A: Type]: Expr[Data] = Expr(
    Data.map(
      "Type.directChildren" -> Type[A].directChildren
        .map(children => Data(children.view.mapValues(value => Data(value.plainPrint)).toMap))
        .getOrElse(Data("<no direct children>")),
      "Type.exhaustiveChildren" -> Type[A].exhaustiveChildren
        .map(children => Data(children.toListMap.view.mapValues(value => Data(value.plainPrint)).toMap))
        .getOrElse(Data("<no exhaustive children>"))
    )
  )

  def testAnnotations[A: Type]: Expr[Data] = Expr(
    Data.map(
      "Type.annotations" -> Data.list(Type.annotations[A].map(value => Data(value.prettyPrint))*)
    )
  )

  def testFlags[A: Type]: Expr[Data] = Expr(
    Data.map(
      "Type.isPrimitive" -> Data(Type[A].isPrimitive),
      "Type.isArray" -> Data(Type[A].isArray),
      "Type.isIArray" -> Data(Type[A].isIArray),
      "Type.isJvmBuiltIn" -> Data(Type[A].isJvmBuiltIn),
      "Type.isAbstract" -> Data(Type[A].isAbstract),
      "Type.isFinal" -> Data(Type[A].isFinal),
      "Type.isClass" -> Data(Type[A].isClass),
      "Type.isTypeSystemSpecial" -> Data(Type[A].isTypeSystemSpecial),
      "Type.isOpaqueType" -> Data(Type[A].isOpaqueType),
      "Type.isTuple" -> Data(Type[A].isTuple),
      "Type.isNamedTuple" -> Data(Type[A].isNamedTuple),
      "Type.notJvmBuiltInClass" -> Data(Type[A].notJvmBuiltInClass),
      "Type.isPlainOldJavaObject" -> Data(Type[A].isPlainOldJavaObject),
      "Type.isJavaBean" -> Data(Type[A].isJavaBean),
      "Type.isSealed" -> Data(Type[A].isSealed),
      "Type.isJavaEnum" -> Data(Type[A].isJavaEnum),
      "Type.isJavaEnumValue" -> Data(Type[A].isJavaEnumValue),
      "Type.isEnumeration" -> Data(Type[A].isEnumeration),
      "Type.isCase" -> Data(Type[A].isCase),
      "Type.isObject" -> Data(Type[A].isObject),
      "Type.isVal" -> Data(Type[A].isVal),
      "Type.isCaseClass" -> Data(Type[A].isCaseClass),
      "Type.isCaseObject" -> Data(Type[A].isCaseObject),
      "Type.isCaseVal" -> Data(Type[A].isCaseVal),
      "Type.isAvailable(Everywhere)" -> Data(Type[A].isAvailable(Everywhere)),
      "Type.isAvailable(AtCallSite)" -> Data(Type[A].isAvailable(AtCallSite))
    )
  )

  def testChildrenFlags[A: Type]: Expr[Data] = Expr(
    Type[A].directChildren
      .map { children =>
        Data(children.view.map { case (name, child) =>
          import child.Underlying as Child
          name -> Data.map(
            "Type.isSealed" -> Data(Child.isSealed),
            "Type.isJavaEnum" -> Data(Child.isJavaEnum),
            "Type.isJavaEnumValue" -> Data(Child.isJavaEnumValue),
            "Type.isEnumeration" -> Data(Child.isEnumeration),
            "Type.isCase" -> Data(Child.isCase),
            "Type.isObject" -> Data(Child.isObject),
            "Type.isVal" -> Data(Child.isVal),
            "Type.isCaseClass" -> Data(Child.isCaseClass),
            "Type.isCaseObject" -> Data(Child.isCaseObject),
            "Type.isCaseVal" -> Data(Child.isCaseVal),
            "Type.isAvailable(Everywhere)" -> Data(Child.isAvailable(Everywhere)),
            "Type.isAvailable(AtCallSite)" -> Data(Child.isAvailable(AtCallSite))
          )
        }.toMap)
      }
      .getOrElse(Data("<no direct children>"))
  )

  def testComparisons[A: Type, B: Type]: Expr[Data] = Expr(
    Data.map(
      "<:<" -> Data(Type[A] <:< Type[B]),
      "=:=" -> Data(Type[A] =:= Type[B])
    )
  )

  // TypeCodecs

  def testBidirectionalCodecs: Expr[Data] = {
    implicit val it: Type[Int] = intType
    implicit val st: Type[String] = stringType
    def roundtrip[A: TypeCodec](value: A): Data = {
      val encoded = Type(value)
      val decoded = Type.unapply(encoded)
      Data.map(
        "encoded" -> Data(encoded.plainPrint),
        "decoded" -> Data(decoded.fold("not decoded") { s =>
          val result = s"$s"
          result.indexOf('@') match {
            case -1    => result
            case index => result.substring(0, index)
          }
        })
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
        "string" -> roundtrip("a"),
        "module" -> roundtrip(scala.Predef)(using TypeCodec.ModuleCodec),
        "Tuple1(1)" -> roundtrip(Tuple1(1)),
        "(1, a)" -> roundtrip((1, "a")),
        "(1, a, true)" -> roundtrip((1, "a", true)),
        "(1, a, true, 2L)" -> roundtrip((1, "a", true, 2L)),
        "(1, a, true, 2L, 3)" -> roundtrip((1, "a", true, 2L, 3)),
        "(1, a, true, 2L, 3, b)" -> roundtrip((1, "a", true, 2L, 3, "b")),
        "(1, a, true, 2L, 3, b, false)" -> roundtrip((1, "a", true, 2L, 3, "b", false)),
        "(1, a, true, 2L, 3, b, false, 4L)" -> roundtrip((1, "a", true, 2L, 3, "b", false, 4L)),
        "(1, a, true, 2L, 3, b, false, 4L, 5)" -> roundtrip((1, "a", true, 2L, 3, "b", false, 4L, 5)),
        "(1, a, true, 2L, 3, b, false, 4L, 5, c)" ->
          roundtrip((1, "a", true, 2L, 3, "b", false, 4L, 5, "c")),
        "(1, a, true, 2L, 3, b, false, 4L, 5, c, true)" ->
          roundtrip((1, "a", true, 2L, 3, "b", false, 4L, 5, "c", true)),
        "(1, a, true, 2L, 3, b, false, 4L, 5, c, true, 6L)" ->
          roundtrip((1, "a", true, 2L, 3, "b", false, 4L, 5, "c", true, 6L)),
        "(1, a, true, 2L, 3, b, false, 4L, 5, c, true, 6L, 7)" ->
          roundtrip((1, "a", true, 2L, 3, "b", false, 4L, 5, "c", true, 6L, 7)),
        "(1, a, true, 2L, 3, b, false, 4L, 5, c, true, 6L, 7, d)" ->
          roundtrip((1, "a", true, 2L, 3, "b", false, 4L, 5, "c", true, 6L, 7, "d")),
        "(1, a, true, 2L, 3, b, false, 4L, 5, c, true, 6L, 7, d, false)" ->
          roundtrip((1, "a", true, 2L, 3, "b", false, 4L, 5, "c", true, 6L, 7, "d", false)),
        "(1, a, true, 2L, 3, b, false, 4L, 5, c, true, 6L, 7, d, false, 8L)" ->
          roundtrip((1, "a", true, 2L, 3, "b", false, 4L, 5, "c", true, 6L, 7, "d", false, 8L)),
        "(1, a, true, 2L, 3, b, false, 4L, 5, c, true, 6L, 7, d, false, 8L, 9)" ->
          roundtrip((1, "a", true, 2L, 3, "b", false, 4L, 5, "c", true, 6L, 7, "d", false, 8L, 9)),
        "(1, a, true, 2L, 3, b, false, 4L, 5, c, true, 6L, 7, d, false, 8L, 9, e)" ->
          roundtrip((1, "a", true, 2L, 3, "b", false, 4L, 5, "c", true, 6L, 7, "d", false, 8L, 9, "e")),
        "(1, a, true, 2L, 3, b, false, 4L, 5, c, true, 6L, 7, d, false, 8L, 9, e, true)" ->
          roundtrip(
            (1, "a", true, 2L, 3, "b", false, 4L, 5, "c", true, 6L, 7, "d", false, 8L, 9, "e", true)
          ),
        "(1, a, true, 2L, 3, b, false, 4L, 5, c, true, 6L, 7, d, false, 8L, 9, e, true, 10L)" ->
          roundtrip(
            (1, "a", true, 2L, 3, "b", false, 4L, 5, "c", true, 6L, 7, "d", false, 8L, 9, "e", true, 10L)
          ),
        "(1, a, true, 2L, 3, b, false, 4L, 5, c, true, 6L, 7, d, false, 8L, 9, e, true, 10L, 11)" ->
          roundtrip(
            (1, "a", true, 2L, 3, "b", false, 4L, 5, "c", true, 6L, 7, "d", false, 8L, 9, "e", true, 10L, 11)
          ),
        "(1, a, true, 2L, 3, b, false, 4L, 5, c, true, 6L, 7, d, false, 8L, 9, e, true, 10L, 11, f)" ->
          roundtrip(
            (1, "a", true, 2L, 3, "b", false, 4L, 5, "c", true, 6L, 7, "d", false, 8L, 9, "e", true, 10L, 11, "f")
          ),
        "Some(1)" -> roundtrip(Some(1)),
        "None" -> roundtrip[Option[Int]](None),
        "Left(1)" -> roundtrip[Either[Int, String]](Left(1)),
        "Right(a)" -> roundtrip[Either[Int, String]](Right("a")),
        "Nil" -> roundtrip[Nil.type](Nil),
        "Class[Int]" -> roundtrip(classOf[Int]),
        "ClassTag[Int]" -> roundtrip(scala.reflect.classTag[Int])
      )
    )
  }

  def testNilAsCollectionCodecs: Expr[Data] = {
    implicit val it: Type[Int] = intType
    val nilTpe = Type.of[Nil.type]
    def decodeNilAs[A: TypeCodec](name: String): (String, Data) = {
      val result = TypeCodec[A].fromType(nilTpe.asInstanceOf[Type[A]])
      name -> Data(result.isDefined.toString)
    }
    Expr(
      Data.map(
        decodeNilAs[Seq[Int]]("Seq[Int]"),
        decodeNilAs[List[Int]]("List[Int]")
      )
    )
  }

  def testOneWayCodecs: Expr[Data] = {
    def oneWay[A: TypeCodec](value: A): Data = {
      val encoded = Type(value)
      Data.map(
        "encoded" -> Data(encoded.plainPrint)
      )
    }
    implicit val a: Type["a"] = aStringType
    implicit val b: Type["b"] = bStringType
    implicit val aCodec: TypeCodec["a"] = TypeCodec.StringCodec.asInstanceOf[TypeCodec["a"]]
    implicit val bCodec: TypeCodec["b"] = TypeCodec.StringCodec.asInstanceOf[TypeCodec["b"]]
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
        """Right["a", "b"]""" -> oneWay[Right["a", "b"]](Right["a", "b"]("b")),
        """Class["a"]""" -> oneWay[java.lang.Class["a"]](classOf[String].asInstanceOf[java.lang.Class["a"]]),
        """ClassTag["a"]""" -> oneWay[scala.reflect.ClassTag["a"]](
          scala.reflect.ClassTag(classOf[String]).asInstanceOf[scala.reflect.ClassTag["a"]]
        ),
        """Tuple1["a"]""" -> oneWay[Tuple1["a"]](Tuple1("a": "a")),
        """("a", "b")""" -> oneWay[("a", "b")](("a": "a", "b": "b")),
        """("a", "b", "a")""" -> oneWay[("a", "b", "a")](("a": "a", "b": "b", "a": "a")),
        """("a", "b", "a", "b")""" -> oneWay[("a", "b", "a", "b")](("a": "a", "b": "b", "a": "a", "b": "b")),
        """("a", "b", "a", "b", "a")""" ->
          oneWay[("a", "b", "a", "b", "a")](("a": "a", "b": "b", "a": "a", "b": "b", "a": "a")),
        """("a", "b", "a", "b", "a", "b")""" ->
          oneWay[("a", "b", "a", "b", "a", "b")](("a": "a", "b": "b", "a": "a", "b": "b", "a": "a", "b": "b")),
        """("a", "b", "a", "b", "a", "b", "a")""" ->
          oneWay[("a", "b", "a", "b", "a", "b", "a")](
            ("a": "a", "b": "b", "a": "a", "b": "b", "a": "a", "b": "b", "a": "a")
          ),
        """("a", "b", "a", "b", "a", "b", "a", "b")""" ->
          oneWay[("a", "b", "a", "b", "a", "b", "a", "b")](
            ("a": "a", "b": "b", "a": "a", "b": "b", "a": "a", "b": "b", "a": "a", "b": "b")
          ),
        """("a", "b", "a", "b", "a", "b", "a", "b", "a")""" ->
          oneWay[("a", "b", "a", "b", "a", "b", "a", "b", "a")](
            ("a": "a", "b": "b", "a": "a", "b": "b", "a": "a", "b": "b", "a": "a", "b": "b", "a": "a")
          ),
        """("a", "b", "a", "b", "a", "b", "a", "b", "a", "b")""" ->
          oneWay[("a", "b", "a", "b", "a", "b", "a", "b", "a", "b")](
            ("a": "a", "b": "b", "a": "a", "b": "b", "a": "a", "b": "b", "a": "a", "b": "b", "a": "a", "b": "b")
          ),
        """("a", "b", "a", "b", "a", "b", "a", "b", "a", "b", "a")""" ->
          oneWay[("a", "b", "a", "b", "a", "b", "a", "b", "a", "b", "a")](
            (
              "a": "a",
              "b": "b",
              "a": "a",
              "b": "b",
              "a": "a",
              "b": "b",
              "a": "a",
              "b": "b",
              "a": "a",
              "b": "b",
              "a": "a"
            )
          ),
        """("a", "b", "a", "b", "a", "b", "a", "b", "a", "b", "a", "b")""" ->
          oneWay[("a", "b", "a", "b", "a", "b", "a", "b", "a", "b", "a", "b")](
            (
              "a": "a",
              "b": "b",
              "a": "a",
              "b": "b",
              "a": "a",
              "b": "b",
              "a": "a",
              "b": "b",
              "a": "a",
              "b": "b",
              "a": "a",
              "b": "b"
            )
          ),
        """("a", "b", "a", "b", "a", "b", "a", "b", "a", "b", "a", "b", "a")""" ->
          oneWay[("a", "b", "a", "b", "a", "b", "a", "b", "a", "b", "a", "b", "a")](
            (
              "a": "a",
              "b": "b",
              "a": "a",
              "b": "b",
              "a": "a",
              "b": "b",
              "a": "a",
              "b": "b",
              "a": "a",
              "b": "b",
              "a": "a",
              "b": "b",
              "a": "a"
            )
          ),
        """("a", "b", "a", "b", "a", "b", "a", "b", "a", "b", "a", "b", "a", "b")""" ->
          oneWay[("a", "b", "a", "b", "a", "b", "a", "b", "a", "b", "a", "b", "a", "b")](
            (
              "a": "a",
              "b": "b",
              "a": "a",
              "b": "b",
              "a": "a",
              "b": "b",
              "a": "a",
              "b": "b",
              "a": "a",
              "b": "b",
              "a": "a",
              "b": "b",
              "a": "a",
              "b": "b"
            )
          ),
        """("a", "b", "a", "b", "a", "b", "a", "b", "a", "b", "a", "b", "a", "b", "a")""" ->
          oneWay[("a", "b", "a", "b", "a", "b", "a", "b", "a", "b", "a", "b", "a", "b", "a")](
            (
              "a": "a",
              "b": "b",
              "a": "a",
              "b": "b",
              "a": "a",
              "b": "b",
              "a": "a",
              "b": "b",
              "a": "a",
              "b": "b",
              "a": "a",
              "b": "b",
              "a": "a",
              "b": "b",
              "a": "a"
            )
          ),
        """("a", "b", "a", "b", "a", "b", "a", "b", "a", "b", "a", "b", "a", "b", "a", "b")""" ->
          oneWay[("a", "b", "a", "b", "a", "b", "a", "b", "a", "b", "a", "b", "a", "b", "a", "b")](
            (
              "a": "a",
              "b": "b",
              "a": "a",
              "b": "b",
              "a": "a",
              "b": "b",
              "a": "a",
              "b": "b",
              "a": "a",
              "b": "b",
              "a": "a",
              "b": "b",
              "a": "a",
              "b": "b",
              "a": "a",
              "b": "b"
            )
          ),
        """("a", "b", "a", "b", "a", "b", "a", "b", "a", "b", "a", "b", "a", "b", "a", "b", "a")""" ->
          oneWay[("a", "b", "a", "b", "a", "b", "a", "b", "a", "b", "a", "b", "a", "b", "a", "b", "a")](
            (
              "a": "a",
              "b": "b",
              "a": "a",
              "b": "b",
              "a": "a",
              "b": "b",
              "a": "a",
              "b": "b",
              "a": "a",
              "b": "b",
              "a": "a",
              "b": "b",
              "a": "a",
              "b": "b",
              "a": "a",
              "b": "b",
              "a": "a"
            )
          ),
        """("a", "b", "a", "b", "a", "b", "a", "b", "a", "b", "a", "b", "a", "b", "a", "b", "a", "b")""" ->
          oneWay[("a", "b", "a", "b", "a", "b", "a", "b", "a", "b", "a", "b", "a", "b", "a", "b", "a", "b")](
            (
              "a": "a",
              "b": "b",
              "a": "a",
              "b": "b",
              "a": "a",
              "b": "b",
              "a": "a",
              "b": "b",
              "a": "a",
              "b": "b",
              "a": "a",
              "b": "b",
              "a": "a",
              "b": "b",
              "a": "a",
              "b": "b",
              "a": "a",
              "b": "b"
            )
          ),
        """("a", "b", "a", "b", "a", "b", "a", "b", "a", "b", "a", "b", "a", "b", "a", "b", "a", "b", "a")""" ->
          oneWay[
            ("a", "b", "a", "b", "a", "b", "a", "b", "a", "b", "a", "b", "a", "b", "a", "b", "a", "b", "a")
          ](
            (
              "a": "a",
              "b": "b",
              "a": "a",
              "b": "b",
              "a": "a",
              "b": "b",
              "a": "a",
              "b": "b",
              "a": "a",
              "b": "b",
              "a": "a",
              "b": "b",
              "a": "a",
              "b": "b",
              "a": "a",
              "b": "b",
              "a": "a",
              "b": "b",
              "a": "a"
            )
          ),
        """("a", "b", "a", "b", "a", "b", "a", "b", "a", "b", "a", "b", "a", "b", "a", "b", "a", "b", "a", "b")""" ->
          oneWay[
            ("a", "b", "a", "b", "a", "b", "a", "b", "a", "b", "a", "b", "a", "b", "a", "b", "a", "b", "a", "b")
          ](
            (
              "a": "a",
              "b": "b",
              "a": "a",
              "b": "b",
              "a": "a",
              "b": "b",
              "a": "a",
              "b": "b",
              "a": "a",
              "b": "b",
              "a": "a",
              "b": "b",
              "a": "a",
              "b": "b",
              "a": "a",
              "b": "b",
              "a": "a",
              "b": "b",
              "a": "a",
              "b": "b"
            )
          ),
        """("a", "b", "a", "b", "a", "b", "a", "b", "a", "b", "a", "b", "a", "b", "a", "b", "a", "b", "a", "b", "a")""" ->
          oneWay[
            (
                "a",
                "b",
                "a",
                "b",
                "a",
                "b",
                "a",
                "b",
                "a",
                "b",
                "a",
                "b",
                "a",
                "b",
                "a",
                "b",
                "a",
                "b",
                "a",
                "b",
                "a"
            )
          ](
            (
              "a": "a",
              "b": "b",
              "a": "a",
              "b": "b",
              "a": "a",
              "b": "b",
              "a": "a",
              "b": "b",
              "a": "a",
              "b": "b",
              "a": "a",
              "b": "b",
              "a": "a",
              "b": "b",
              "a": "a",
              "b": "b",
              "a": "a",
              "b": "b",
              "a": "a",
              "b": "b",
              "a": "a"
            )
          ),
        """("a", "b", "a", "b", "a", "b", "a", "b", "a", "b", "a", "b", "a", "b", "a", "b", "a", "b", "a", "b", "a", "b")""" ->
          oneWay[
            (
                "a",
                "b",
                "a",
                "b",
                "a",
                "b",
                "a",
                "b",
                "a",
                "b",
                "a",
                "b",
                "a",
                "b",
                "a",
                "b",
                "a",
                "b",
                "a",
                "b",
                "a",
                "b"
            )
          ](
            (
              "a": "a",
              "b": "b",
              "a": "a",
              "b": "b",
              "a": "a",
              "b": "b",
              "a": "a",
              "b": "b",
              "a": "a",
              "b": "b",
              "a": "a",
              "b": "b",
              "a": "a",
              "b": "b",
              "a": "a",
              "b": "b",
              "a": "a",
              "b": "b",
              "a": "a",
              "b": "b",
              "a": "a",
              "b": "b"
            )
          )
      )
    )
  }

  // types using in fixtures

  private val aStringType = Type.of["a"]
  private val bStringType = Type.of["b"]
  private val intType = Type.of[Int]
  private val stringType = Type.of[String]
}
