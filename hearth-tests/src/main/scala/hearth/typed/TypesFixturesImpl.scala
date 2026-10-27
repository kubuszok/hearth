package hearth
package typed

import hearth.data.Data

trait TypesFixturesImpl { this: MacroCommons =>

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

  /** The exact parent lists differ across platforms/versions (Object/Any/Serializable/Product orderings), so only facts
    * stable everywhere are reported.
    */
  def testTypeHierarchy[A: Type, Parent: Type]: Expr[Data] = {
    val parents = Type.parents[A]
    val baseClasses = Type.baseClasses[A]
    def containsType(types: List[??], needle: ??): Boolean = types.exists(_.Underlying =:= needle.Underlying)
    Expr(
      Data.map(
        "isTrait" -> Data(Type.isTrait[A]),
        "parentContainsParent" -> Data(containsType(parents, Type[Parent].as_??)),
        "baseClassesContainsParent" -> Data(containsType(baseClasses, Type[Parent].as_??)),
        "baseClassesContainsSelf" -> Data(containsType(baseClasses, Type[A].as_??)),
        "baseClassesContainsAny" -> Data(containsType(baseClasses, Type.of[Any].as_??)),
        "baseClassesAtLeastParents" -> Data(baseClasses.size >= parents.size)
      )
    )
  }

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

  def testAnnotationTypeComparison[A: Type]: Expr[Data] = {
    val exampleAnnotationType = Type.of[examples.methods.ExampleAnnotation]
    val exampleAnnotation2Type = Type.of[examples.methods.ExampleAnnotation2]
    val anns = Type.annotations[A]
    val results = anns.map { ann =>
      import ann.Underlying as AnnType
      Data.map(
        "prettyPrint" -> Data(removeAnsiColors(ann.prettyPrint)),
        "type" -> Data(AnnType.plainPrint),
        "<:< ExampleAnnotation" -> Data(AnnType <:< exampleAnnotationType),
        "=:= ExampleAnnotation" -> Data(AnnType =:= exampleAnnotationType),
        "<:< ExampleAnnotation2" -> Data(AnnType <:< exampleAnnotation2Type),
        "=:= ExampleAnnotation2" -> Data(AnnType =:= exampleAnnotation2Type)
      )
    }
    Expr(Data.map("annotations" -> Data.list(results*)))
  }

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
      "Type.isUnionType" -> Data(Type[A].isUnionType),
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

  def testOpaqueUnderlyingType[A: Type]: Expr[Data] = Expr(
    Data.map(
      "Type.isOpaqueType" -> Data(Type[A].isOpaqueType),
      "Type.opaqueUnderlyingType" -> Type[A].opaqueUnderlyingType
        .map(underlying => Data(underlying.plainPrint))
        .getOrElse(Data("<no underlying type>"))
    )
  )

  def testUnionMembers[A: Type]: Expr[Data] = Expr(
    Data.map(
      "Type.isUnionType" -> Data(Type[A].isUnionType),
      "Type.directChildren" -> Type[A].directChildren
        .map(children => Data(children.view.mapValues(value => Data(value.plainPrint)).toMap))
        .getOrElse(Data("<no direct children>")),
      "Type.exhaustiveChildren" -> Type[A].exhaustiveChildren
        .map(children => Data(children.toListMap.view.mapValues(value => Data(value.plainPrint)).toMap))
        .getOrElse(Data("<no exhaustive children>"))
    )
  )

  def testChildrenNames[A: Type]: Expr[Data] = Expr(
    Type[A].directChildren
      .map { children =>
        Data(children.view.map { case (name, child) =>
          import child.Underlying as Child
          name -> Data(Child.shortName)
        }.toMap)
      }
      .getOrElse(Data("<no direct children>"))
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

  def testTypeArguments[A: Type]: Expr[Data] = Expr(
    Data.map(
      "Type.typeArguments" -> Data.list(Type.typeArguments[A].map(arg => Data(arg.plainPrint))*)
    )
  )

  def testDecompose1[A: Type]: Expr[Data] = {
    val listCtor = Type.Ctor1.of[List]
    val optionCtor = Type.Ctor1.of[Option]
    Type.decompose1[A] match {
      case Some((ctor, arg)) =>
        implicit val StringType: Type[String] = stringType
        Expr(
          Data.map(
            "decomposed" -> Data(true),
            "arg" -> Data(arg.plainPrint),
            "reappliedToString" -> Data(ctor.apply[String].plainPrint),
            "sameCtorAsList" -> Data(ctor.sameTypeConstructorAs(listCtor.asUntyped)),
            "sameCtorAsOption" -> Data(ctor.sameTypeConstructorAs(optionCtor.asUntyped)),
            "sameCtorAsSelf" -> Data(ctor.sameTypeConstructorAs(UntypedType.fromTyped[A]))
          )
        )
      case None => Expr(Data.map("decomposed" -> Data(false)))
    }
  }

  def testDecompose2[A: Type]: Expr[Data] = {
    val mapCtor = Type.Ctor2.of[Map]
    val eitherCtor = Type.Ctor2.of[Either]
    Type.decompose2[A] match {
      case Some((ctor, (arg1, arg2))) =>
        implicit val StringType: Type[String] = stringType
        implicit val IntType: Type[Int] = intType
        Expr(
          Data.map(
            "decomposed" -> Data(true),
            "arg1" -> Data(arg1.plainPrint),
            "arg2" -> Data(arg2.plainPrint),
            "reappliedToStringInt" -> Data(ctor.apply[String, Int].plainPrint),
            "sameCtorAsMap" -> Data(ctor.sameTypeConstructorAs(mapCtor.asUntyped)),
            "sameCtorAsEither" -> Data(ctor.sameTypeConstructorAs(eitherCtor.asUntyped))
          )
        )
      case None => Expr(Data.map("decomposed" -> Data(false)))
    }
  }

  def testMiniFunctorDerivation[A: Type]: Expr[Data] = {
    import hearth.examples.kinds.MyTC
    implicit val StringType: Type[String] = stringType
    val listCtor = Type.Ctor1.of[List]
    val optionCtor = Type.Ctor1.of[Option]
    val myTCCtor = Type.CtorK1.of[MyTC]
    CaseClass.parse[A].toOption.fold(Expr(Data.map("error" -> Data("not a case class")))) { caseClass =>
      val fields = caseClass.primaryConstructor.parameters.flatten.toList.map { case (name, param) =>
        import param.tpe.Underlying as FieldType
        val fieldData = Type.decompose1[FieldType] match {
          case Some((gCtor, arg)) =>
            implicit val MyTCofG: Type[MyTC[AnyK1]] = myTCCtor.apply(using gCtor)
            Data.map(
              "arg" -> Data(arg.plainPrint),
              "reappliedToString" -> Data(gCtor.apply[String].plainPrint),
              "isList" -> Data(gCtor.sameTypeConstructorAs(listCtor.asUntyped)),
              "isOption" -> Data(gCtor.sameTypeConstructorAs(optionCtor.asUntyped)),
              "myTCInstanceFound" -> Data(Expr.summonImplicit[MyTC[AnyK1]].isDefined)
            )
          case None => Data("not an applied type")
        }
        name -> fieldData
      }
      Expr(Data.map(fields*))
    }
  }

  /** Returns the instance's `name`, to tell that the RIGHT instance was summoned, not just some instance. */
  def testDecomposeSummonName[A: Type]: Expr[String] = {
    import hearth.examples.kinds.{HasName, MyTC}
    val myTCCtor = Type.CtorK1.of[MyTC]
    Type.decompose1[A] match {
      case Some((gCtor, _)) =>
        implicit val MyTCofG: Type[MyTC[AnyK1]] = myTCCtor.apply(using gCtor)
        Expr.summonImplicit[MyTC[AnyK1]].toOption match {
          case Some(instance) =>
            implicit val HasNameType: Type[HasName] = hasNameType
            val upcasted = Expr.upcast[MyTC[AnyK1], HasName](instance)
            Expr.quote(Expr.splice(upcasted).name)
          case None => Expr("<no instance found>")
        }
      case None => Expr("<not an applied type>")
    }
  }

  def testCtorK1FromDiscoveredParts[A: Type]: Expr[Data] = {
    import hearth.examples.kinds.HigherKinded1
    val optionCtor = Type.Ctor1.of[Option]
    val appliedHK1: Type[HigherKinded1[Option]] = Type.CtorK1.of[HigherKinded1].apply[Option](using optionCtor)
    val algUntyped = UntypedType.typeConstructor(UntypedType.fromTyped(using appliedHK1))
    val alg = Type.CtorK1.fromUntyped[HigherKinded1](algUntyped)
    Type.decompose1[A] match {
      case Some((gCtor, _)) =>
        val algOfG = alg.apply(using gCtor)
        Expr(
          Data.map(
            "appliedSameCtorAsAlg" -> Data(alg.sameTypeConstructorAs(UntypedType.fromTyped(using algOfG))),
            "memberCtorMatches" -> Data(
              alg.unapply(algOfG).fold(false)(g => UntypedType.sameTypeConstructorAs(g, gCtor.asUntyped))
            )
          )
        )
      case None => Expr(Data.map("error" -> Data("not an applied type")))
    }
  }

  /** Every type-constructor primitive applied to `A` - used to check that singleton types (`x.type`) are seen through
    * just like their widened types are.
    */
  def testTypeConstructorPrimitives[A: Type]: Expr[Data] = {
    import hearth.examples.kinds.HigherKinded1
    def print(tpe: UntypedType): Data = Data(tpe.as_??.plainPrint)
    def printOpt(opt: Option[Data]): Data = opt.getOrElse(Data("<none>"))
    val untyped = UntypedType.fromTyped[A]
    Expr(
      Data.map(
        "typeConstructor" -> print(UntypedType.typeConstructor(untyped)),
        "typeArguments" -> Data.list(UntypedType.typeArguments(untyped).map(print)*),
        "Type.typeArguments" -> Data.list(Type.typeArguments[A].map(arg => Data(arg.plainPrint))*),
        "decompose1" -> printOpt(Type.decompose1[A].map { case (ctor, arg) =>
          Data.list(print(ctor.asUntyped), Data(arg.plainPrint))
        }),
        "decompose2" -> printOpt(Type.decompose2[A].map { case (ctor, (arg1, arg2)) =>
          Data.list(print(ctor.asUntyped), Data(arg1.plainPrint), Data(arg2.plainPrint))
        }),
        "sameCtorAsOption" -> Data(Type.Ctor1.of[Option].sameTypeConstructorAs(untyped)),
        "sameCtorAsEither" -> Data(Type.Ctor2.of[Either].sameTypeConstructorAs(untyped)),
        "Ctor1.of[Option].unapply" -> printOpt(Type.Ctor1.of[Option].unapply(Type[A]).map(arg => Data(arg.plainPrint))),
        "Ctor2.of[Either].unapply" -> printOpt(Type.Ctor2.of[Either].unapply(Type[A]).map { case (arg1, arg2) =>
          Data.list(Data(arg1.plainPrint), Data(arg2.plainPrint))
        }),
        "CtorK1.of[HigherKinded1].unapply" -> printOpt(Type.CtorK1.of[HigherKinded1].unapply(Type[A]).map(print))
      )
    )
  }

  /** [[testTypeConstructorPrimitives]] on the static type of an argument expression (`Expr.typeOf`): for a plain
    * reference `x` it is the singleton `x.type` on Scala 3 and the widened type on Scala 2.
    */
  def testTypeConstructorPrimitivesOfExpr[A](expr: Expr[A]): Expr[Data] =
    testTypeConstructorPrimitives[A](using Expr.typeOf(expr))

  /** Checks that `A`'s decomposition rebuilds exactly `Expected` (compared with `=:=`, so higher-kinded type arguments,
    * which Scala 3 prints eta-expanded, need no platform-specific expectations).
    */
  def testDecomposesTo[A: Type, Expected: Type]: Expr[Data] = {
    val expected = UntypedType.fromTyped[Expected]
    def rebuilt(ctor: UntypedType, args: List[??]): Data =
      Data(UntypedType.applyTypeArgs(ctor, args.map(_.Underlying.asUntyped)) =:= expected)
    Expr(
      Data.map(
        "decompose1RebuildsExpected" -> Type.decompose1[A].fold(Data("<none>")) { case (ctor, arg) =>
          rebuilt(ctor.asUntyped, List(arg))
        },
        "decompose2RebuildsExpected" -> Type.decompose2[A].fold(Data("<none>")) { case (ctor, (arg1, arg2)) =>
          rebuilt(ctor.asUntyped, List(arg1, arg2))
        },
        "typeConstructorAndArgumentsRebuildExpected" -> rebuilt(
          UntypedType.typeConstructor(UntypedType.fromTyped[A]),
          Type.typeArguments[A]
        ),
        "hasSameTypeConstructor" -> Data(Type.hasSameTypeConstructor[A, Expected])
      )
    )
  }

  def testSameTypeConstructorAndReapply[A: Type, B: Type]: Expr[Data] = {
    val a = UntypedType.fromTyped[A]
    val b = UntypedType.fromTyped[B]
    Expr(
      Data.map(
        "hasSameTypeConstructor" -> Data(Type.hasSameTypeConstructor[A, B]),
        "reappliedWithArgsOfB" -> Data(UntypedType.applyTypeArgs(a, UntypedType.typeArguments(b)).as_??.plainPrint)
      )
    )
  }

  def testFunctionType[A: Type]: Expr[Data] = {
    def components(c: Option[(List[??], ??)]): Data = c.fold(Data("<none>")) { case (params, result) =>
      Data.map("params" -> Data.list(params.map(p => Data(p.plainPrint))*), "result" -> Data(result.plainPrint))
    }
    Expr(
      Data.map(
        "isFunction" -> Data(Type.isFunction[A]),
        "isContextFunction" -> Data(Type.isContextFunction[A]),
        "functionComponents" -> components(Type.functionComponents[A]),
        "contextFunctionComponents" -> components(Type.contextFunctionComponents[A]),
        "Ctor2.of[Function1].unapply" -> Type.Ctor2.of[Function1].unapply(Type[A]).fold(Data("<none>")) { case (a, b) =>
          Data.list(Data(a.plainPrint), Data(b.plainPrint))
        }
      )
    )
  }

  def testPseudoCompanionObject[A: Type]: Expr[Data] = {
    val NewtypeCtor = Type.Ctor1.of[examples.newtypes.Newtype]
    Expr(Type.pseudoCompanionObject[A].fold(Data("<none>")) { obj =>
      Data.map(
        "object" -> Data(obj.Underlying.plainPrint),
        "Newtype[_] argument" -> NewtypeCtor.unapply(obj.Underlying).fold(Data("<none>"))(a => Data(a.plainPrint))
      )
    })
  }

  def testPseudoCompanionObjectValue[A: Type]: Expr[Any] = {
    implicit val AnyType: Type[Any] = Type.of[Any]
    Type.pseudoCompanionObject[A].fold(Expr.quote(null: Any)) { obj =>
      import obj.Underlying as O
      obj.value.upcast[Any]
    }
  }

  /** Applies a `Type.Ctor1` / `Type.CtorK1` that the platform bridge built with `.of` from the macro's OWN type
    * parameter (`F` / `Alg`), as derivation entry points (`Functor.derived[F]`, `FunctorK.derived[Alg]`) do.
    */
  def testCtor1OfOwnTypeParameter[F[_]](ctor: Type.Ctor1[F]): Expr[String] = {
    implicit val IntType: Type[Int] = intType
    Expr(ctor.apply[Int].plainPrint)
  }
  def testCtor2OfOwnTypeParameter[F[_, _]](ctor: Type.Ctor2[F]): Expr[String] = {
    implicit val IntType: Type[Int] = intType
    implicit val StringType: Type[String] = stringType
    Expr(ctor.apply[Int, String].plainPrint)
  }
  def testCtorK1OfOwnTypeParameter[Alg[_[_]]](ctor: Type.CtorK1[Alg]): Expr[String] =
    Expr(ctor.apply[Option](using Type.Ctor1.of[Option]).plainPrint)

  // The deferred type must behave like an eagerly-initialized one even when its FIRST touch happens
  // inside an `Expr.splice` (a fresh nested Quotes on Scala 3) and it is then reused OUTSIDE that splice - the
  // pattern that breaks a plain `lazy val` with "Expression created in a splice was used outside of that splice".
  def testLazyTypeScopeSafety: Expr[Data] = {
    val lazyString: Type.Lazy[String] = Type.Lazy(Type.of[String])

    // First touch INSIDE a splice (nested ctx on Scala 3):
    val innerExpr: Expr[String] = Expr.quote {
      Expr.splice {
        Expr(lazyString.value.plainPrint)
      } + "!"
    }

    // Reuse OUTSIDE that splice (entry ctx), via the implicit conversion:
    val outerName: String = (lazyString: Type[String]).plainPrint

    Expr.quote {
      Data.map(
        "inner" -> Data(Expr.splice(innerExpr)),
        "outer" -> Data(Expr.splice(Expr(outerName)))
      )
    }
  }

  def testBuiltInPredicates[A: Type]: Expr[Data] = {
    def listed(types: List[??]): Data = Data(types.exists(_.Underlying =:= Type[A]))
    Expr(
      Data.map(
        "Type.isPrimitive" -> Data(Type[A].isPrimitive),
        "Type.isArray" -> Data(Type[A].isArray),
        "Type.isJvmBuiltIn" -> Data(Type[A].isJvmBuiltIn),
        "Type.isInJavaLangPackage" -> Data(Type[A].isInJavaLangPackage),
        "Type.isTypeSystemSpecial" -> Data(Type[A].isTypeSystemSpecial),
        "Type.isTuple" -> Data(Type[A].isTuple),
        "in Type.primitiveTypes" -> listed(Type.primitiveTypes),
        "in Type.jvmBuiltInTypes" -> listed(Type.jvmBuiltInTypes),
        "in Type.typeSystemSpecialTypes" -> listed(Type.typeSystemSpecialTypes)
      )
    )
  }

  /** `Object`/`Any`/`AnyRef`/`Matchable` appear in the hierarchies of every class, at platform-dependent positions, so
    * they are left out to keep the expectations identical everywhere.
    */
  def testHierarchyLists[A: Type]: Expr[Data] = {
    val universal = Set("java.lang.Object", "scala.Any", "scala.AnyRef", "scala.Matchable")
    def printed(types: List[??]): Data =
      Data.list(types.map(_.plainPrint).filterNot(universal).map(Data(_))*)
    Expr(
      Data.map(
        "Type.isTrait" -> Data(Type[A].isTrait),
        "Type.isAbstract" -> Data(Type[A].isAbstract),
        "Type.parents" -> printed(Type[A].parents),
        "Type.baseClasses" -> printed(Type[A].baseClasses)
      )
    )
  }

  def testChildrenList[A: Type]: Expr[Data] = Expr(
    Data.map(
      "Type.directChildren keys" -> Type[A].directChildren
        .fold(Data("<no direct children>"))(children => Data.list(children.keys.toList.map(Data(_))*)),
      "Type.directChildrenList" -> Type
        .directChildrenList[A]
        .fold(Data("<no direct children>")) { children =>
          Data.list(children.map { case (name, child) => Data.list(Data(name), Data(child.plainPrint)) }*)
        }
    )
  )

  def testExhaustiveChildren[A: Type]: Expr[Data] = Expr(
    Type[A].exhaustiveChildren.fold(Data("<no exhaustive children>")) { children =>
      Data(children.toListMap.view.mapValues(child => Data(child.plainPrint)).toMap)
    }
  )

  def testCompanionObject[A: Type]: Expr[Data] = Expr(
    Type[A].companionObject.fold(Data("<none>"))(companion => Data(companion.Underlying.plainPrint))
  )

  def testCompanionObjectValue[A: Type]: Expr[Any] = {
    implicit val AnyType: Type[Any] = Type.of[Any]
    Type[A].companionObject.fold(Expr.quote(null: Any)) { companion =>
      import companion.Underlying as Companion
      companion.value.upcast[Any]
    }
  }

  def testJavaBeanVisibility[A: Type]: Expr[Data] = Expr(
    Data.map(
      "Type.isPlainOldJavaObject" -> Data(Type[A].isPlainOldJavaObject),
      "Type.isJavaBean" -> Data(Type[A].isJavaBean),
      "Type.isJavaBean(Everywhere)" -> Data(Type[A].isJavaBean(Everywhere)),
      "Type.isJavaBean(AtCallSite)" -> Data(Type[A].isJavaBean(AtCallSite))
    )
  )

  def testTypeCacheAndLazy: Expr[Data] = {
    val cache = new Type.Cache[TypesFixturesImpl.Label]
    val missOnEmptyCache = cache.get(Type.of[Int]).isEmpty
    cache.put(Type.of[Int], "int")
    cache.put(Type.of[List[Int]], "list of int")
    var computations = 0
    val firstGetOrPut = cache.getOrPut(Type.of[String]) { computations += 1; "string" }
    val secondGetOrPut = cache.getOrPut(Type.of[String]) { computations += 1; "recomputed" }

    var lazyComputations = 0
    val lazyLong = Type.Lazy { lazyComputations += 1; Type.of[Long] }
    val lazyComputationsBeforeUse = lazyComputations
    val firstUse = lazyLong.value.plainPrint
    val secondUse = (lazyLong: Type[Long]).plainPrint

    def lookup[B](tpe: Type[B]): Data = Data(cache.get(tpe).getOrElse("<miss>"))
    val static = Data.map(
      "miss on empty cache" -> Data(missOnEmptyCache),
      "get(Int)" -> lookup(Type.of[Int]),
      "get(IntAlias)" -> lookup(Type.of[examples.typesedge.aliases.IntAlias]),
      "get(Long)" -> lookup(Type.of[Long]),
      "get(List[Int])" -> lookup(Type.of[List[Int]]),
      "get(ListAlias[Int])" -> lookup(Type.of[examples.typesedge.aliases.ListAlias[Int]]),
      "get(List[String])" -> lookup(Type.of[List[String]]),
      "getOrPut first" -> Data(firstGetOrPut),
      "getOrPut second" -> Data(secondGetOrPut),
      "getOrPut computations" -> Data(computations),
      "Type.Lazy computations before use" -> Data(lazyComputationsBeforeUse),
      "Type.Lazy uses" -> Data.list(Data(firstUse), Data(secondUse)),
      "Type.Lazy computations after 2 uses" -> Data(lazyComputations)
    )

    val lookupInsideSplice: Expr[String] = Expr.quote {
      "found: " + Expr.splice(Expr(cache.get(Type.of[Int]).getOrElse("<miss>")))
    }
    val staticExpr: Expr[Data] = Expr(static)
    Expr.quote {
      Data.map(
        "outside splice" -> Expr.splice(staticExpr),
        "get(Int) inside a splice" -> Data(Expr.splice(lookupInsideSplice))
      )
    }
  }

  def testTypeCodecDecoding: Expr[Data] = {
    implicit val IntType: Type[Int] = intType
    implicit val StringType: Type[String] = stringType
    def decoded[B, C](codec: TypeCodec[B], tpe: Type[C]): Data =
      Data(codec.fromType(tpe).fold("<not decoded>")(_.value.toString))
    def roundtrip[B: TypeCodec](value: B): Data = {
      val encoded = Type(value)
      Data.map(
        "encoded" -> Data(encoded.plainPrint),
        "decoded" -> Data(Type.unapply(encoded).fold("<not decoded>")(_.toString))
      )
    }
    Expr(
      Data.map(
        "IntCodec on Int" -> decoded(TypeCodec.IntCodec, Type.of[Int]),
        "IntCodec on String literal" -> decoded(TypeCodec.IntCodec, aStringType),
        "StringCodec on String" -> decoded(TypeCodec.StringCodec, Type.of[String]),
        "BooleanCodec on Boolean" -> decoded(TypeCodec.BooleanCodec, Type.of[Boolean]),
        "OptionCodec on Option[Int]" -> decoded(TypeCodec.OptionCodec[Int], Type.of[Option[Int]]),
        "ModuleCodec on a case object" -> decoded(
          TypeCodec.ModuleCodec[examples.typesedge.TopLevelCaseObject.type],
          Type.of[examples.typesedge.TopLevelCaseObject.type]
        ),
        "ModuleCodec on a nested case object" -> decoded(
          TypeCodec.ModuleCodec[examples.typesedge.SealedBase.Second.type],
          Type.of[examples.typesedge.SealedBase.Second.type]
        ),
        "ModuleCodec on a class" -> decoded(
          TypeCodec.ModuleCodec[examples.typesedge.TopLevelCaseObject.type],
          Type.of[examples.typesedge.WithoutCompanion]
        ),
        "None.type" -> roundtrip[None.type](None),
        "Some[Int]" -> roundtrip[Some[Int]](Some(1)),
        "Left[Int, String]" -> roundtrip[Left[Int, String]](Left(1)),
        "Right[Int, String]" -> roundtrip[Right[Int, String]](Right("a"))
      )
    )
  }

  def testUnionTypeTests[A: Type, Member: Type]: Expr[Data] = Expr(
    Data.map(
      "Type.unionMemberRequiresTypeTest" -> Data(Type.unionMemberRequiresTypeTest[A, Member]),
      "Type.unionRefusalReason" -> Type
        .unionRefusalReason[A]
        .fold(Data("<none>")) { reason =>
          Data.map(
            "mentions TypeTest" -> Data(reason.contains("TypeTest")),
            "mentions member" -> Data(reason.contains(Type[Member].plainPrint))
          )
        }
    )
  )

  private val aStringType = Type.of["a"]
  private val bStringType = Type.of["b"]
  private val intType = Type.of[Int]
  private val stringType = Type.of[String]
  private val hasNameType = Type.of[hearth.examples.kinds.HasName]
}
object TypesFixturesImpl {

  type Label[A] = String
}
