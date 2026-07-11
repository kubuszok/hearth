package hearth
package typed

import hearth.data.Data

import scala.quoted.*

final private class MethodsFixtures(q: Quotes) extends MacroCommonsScala3(using q), MethodsFixturesImpl

object MethodsFixtures {

  // Chimney #673
  inline def testExposesNameAndAge[A]: Data = ${ testExposesNameAndAgeImpl[A] }
  private def testExposesNameAndAgeImpl[A: Type](using q: Quotes): Expr[Data] =
    new MethodsFixtures(q).testExposesNameAndAge[A]

  // [hearth#331]
  inline def testFoldSubstitutesTypeArgs: Data = ${ testFoldSubstitutesTypeArgsImpl }
  private def testFoldSubstitutesTypeArgsImpl(using q: Quotes): Expr[Data] =
    new MethodsFixtures(q).testFoldSubstitutesTypeArgs

  // [hearth#176]

  inline def testConstructorsExtraction[A]: Data = ${ testConstructorsExtractionImpl[A] }
  private def testConstructorsExtractionImpl[A: Type](using q: Quotes): Expr[Data] =
    new MethodsFixtures(q).testConstructorsExtraction[A]

  inline def testMethodsExtraction[A](inline excluding: String*): Data = ${
    testMethodsExtractionImpl[A]('excluding)
  }
  private def testMethodsExtractionImpl[A: Type](excluding: Expr[Seq[String]])(using q: Quotes): Expr[Data] =
    new MethodsFixtures(q).testMethodsExtraction[A](excluding)

  inline def testMethodDefaults[A](inline methodName: String): Data = ${
    testMethodDefaultsImpl[A]('methodName)
  }
  private def testMethodDefaultsImpl[A: Type](methodName: Expr[String])(using q: Quotes): Expr[Data] =
    new MethodsFixtures(q).testMethodDefaults[A](methodName)

  inline def testConstructWithDefaults[A](inline params: Int*): Data = ${
    testConstructWithDefaultsImpl[A]('params)
  }
  private def testConstructWithDefaultsImpl[A: Type](params: Expr[Seq[Int]])(using q: Quotes): Expr[Data] =
    new MethodsFixtures(q).testConstructWithDefaults[A](params)

  inline def testCallNoInstanceMethodWithDefaults[A](inline methodName: String)(inline params: Int*): Data = ${
    testCallNoInstanceMethodWithDefaultsImpl[A]('methodName, 'params)
  }
  private def testCallNoInstanceMethodWithDefaultsImpl[A: Type](methodName: Expr[String], params: Expr[Seq[Int]])(using
      q: Quotes
  ): Expr[Data] =
    new MethodsFixtures(q).testCallNoInstanceMethodWithDefaults[A](methodName)(params)

  inline def testCallInstanceMethodWithDefaults[A](inline instance: A)(inline methodName: String)(
      inline params: Int*
  ): Data = ${
    testCallInstanceMethodWithDefaultsImpl[A]('instance, 'methodName, 'params)
  }
  private def testCallInstanceMethodWithDefaultsImpl[A: Type](
      instance: Expr[A],
      methodName: Expr[String],
      params: Expr[Seq[Int]]
  )(using q: Quotes): Expr[Data] =
    new MethodsFixtures(q).testCallInstanceMethodWithDefaults[A](instance)(methodName)(params)

  inline def testCallNoInstanceIntMethod[A](inline methodName: String)(inline params: Int*): Int = ${
    testCallNoInstanceIntMethodImpl[A]('methodName, 'params)
  }
  private def testCallNoInstanceIntMethodImpl[A: Type](methodName: Expr[String], params: Expr[Seq[Int]])(using
      q: Quotes
  ): Expr[Int] =
    new MethodsFixtures(q).testCallNoInstanceIntMethod[A](methodName)(params)

  inline def testCallInstanceIntMethod[A](inline instance: A)(inline methodName: String)(inline params: Int*): Int = ${
    testCallInstanceIntMethodImpl[A]('instance, 'methodName, 'params)
  }
  private def testCallInstanceIntMethodImpl[A: Type](
      instance: Expr[A],
      methodName: Expr[String],
      params: Expr[Seq[Int]]
  )(using q: Quotes): Expr[Int] =
    new MethodsFixtures(q).testCallInstanceIntMethod[A](instance)(methodName)(params)

  inline def testParameterProperties[A](inline methodName: String): Data = ${
    testParameterPropertiesImpl[A]('methodName)
  }
  private def testParameterPropertiesImpl[A: Type](methodName: Expr[String])(using q: Quotes): Expr[Data] =
    new MethodsFixtures(q).testParameterProperties[A](methodName)

  inline def testCallVarargIntMethod[A](inline instance: A)(inline methodName: String)(inline params: Int*): Int = ${
    testCallVarargIntMethodImpl[A]('instance, 'methodName, 'params)
  }
  private def testCallVarargIntMethodImpl[A: Type](
      instance: Expr[A],
      methodName: Expr[String],
      params: Expr[Seq[Int]]
  )(using q: Quotes): Expr[Int] =
    new MethodsFixtures(q).testCallVarargIntMethod[A](instance)(methodName)(params)

  inline def testConstructVarargCtor[A](inline params: String*): Data = ${
    testConstructVarargCtorImpl[A]('params)
  }
  private def testConstructVarargCtorImpl[A: Type](params: Expr[Seq[String]])(using q: Quotes): Expr[Data] =
    new MethodsFixtures(q).testConstructVarargCtor[A](params)

  inline def testAccessorMetadata[A](inline methodName: String): Data = ${ testAccessorMetadataImpl[A]('methodName) }
  private def testAccessorMetadataImpl[A: Type](methodName: Expr[String])(using q: Quotes): Expr[Data] =
    new MethodsFixtures(q).testAccessorMetadata[A](methodName)

  inline def testMethodProperties[A](inline methodName: String): Data = ${
    testMethodPropertiesImpl[A]('methodName)
  }
  private def testMethodPropertiesImpl[A: Type](methodName: Expr[String])(using q: Quotes): Expr[Data] =
    new MethodsFixtures(q).testMethodProperties[A](methodName)

  inline def testMethodVisibility[A](inline methodName: String): Data = ${
    testMethodVisibilityImpl[A]('methodName)
  }
  private def testMethodVisibilityImpl[A: Type](methodName: Expr[String])(using q: Quotes): Expr[Data] =
    new MethodsFixtures(q).testMethodVisibility[A](methodName)

  inline def testMethodExpectations[A](inline methodName: String): Data = ${
    testMethodExpectationsImpl[A]('methodName)
  }
  private def testMethodExpectationsImpl[A: Type](methodName: Expr[String])(using q: Quotes): Expr[Data] =
    new MethodsFixtures(q).testMethodExpectations[A](methodName)

  inline def testConstructorExpectations[A]: Data = ${ testConstructorExpectationsImpl[A] }
  private def testConstructorExpectationsImpl[A: Type](using q: Quotes): Expr[Data] =
    new MethodsFixtures(q).testConstructorExpectations[A]

  inline def testMethodAppliedToString[A](inline instance: A)(inline methodName: String): Data = ${
    testMethodAppliedToStringImpl[A]('instance, 'methodName)
  }
  private def testMethodAppliedToStringImpl[A: Type](instance: Expr[A], methodName: Expr[String])(using
      q: Quotes
  ): Expr[Data] =
    new MethodsFixtures(q).testMethodAppliedToString[A](instance)(methodName)

  inline def testConstructorAppliedToString[A]: Data = ${ testConstructorAppliedToStringImpl[A] }
  private def testConstructorAppliedToStringImpl[A: Type](using q: Quotes): Expr[Data] =
    new MethodsFixtures(q).testConstructorAppliedToString[A]

  inline def testMethodOrdering[A]: Data = ${ testMethodOrderingImpl[A] }
  private def testMethodOrderingImpl[A: Type](using q: Quotes): Expr[Data] =
    new MethodsFixtures(q).testMethodOrdering[A]

  inline def testMethodPrettyPrint[A](inline methodName: String): Data = ${
    testMethodPrettyPrintImpl[A]('methodName)
  }
  private def testMethodPrettyPrintImpl[A: Type](methodName: Expr[String])(using q: Quotes): Expr[Data] =
    new MethodsFixtures(q).testMethodPrettyPrint[A](methodName)

  inline def testCallInstanceViaFold[A](inline instance: A)(inline methodName: String)(inline params: Int*): Data = ${
    testCallInstanceViaFoldImpl[A]('instance, 'methodName, 'params)
  }
  private def testCallInstanceViaFoldImpl[A: Type](
      instance: Expr[A],
      methodName: Expr[String],
      params: Expr[Seq[Int]]
  )(using q: Quotes): Expr[Data] =
    new MethodsFixtures(q).testCallInstanceViaFold[A](instance)(methodName)(params)

  inline def testCallConstructorViaFold[A](inline params: Int*): Data = ${
    testCallConstructorViaFoldImpl[A]('params)
  }
  private def testCallConstructorViaFoldImpl[A: Type](params: Expr[Seq[Int]])(using q: Quotes): Expr[Data] =
    new MethodsFixtures(q).testCallConstructorViaFold[A](params)

  inline def testCallInstanceViaFoldF[A](inline instance: A)(inline methodName: String)(inline params: Int*): Data = ${
    testCallInstanceViaFoldFImpl[A]('instance, 'methodName, 'params)
  }
  private def testCallInstanceViaFoldFImpl[A: Type](
      instance: Expr[A],
      methodName: Expr[String],
      params: Expr[Seq[Int]]
  )(using q: Quotes): Expr[Data] =
    new MethodsFixtures(q).testCallInstanceViaFoldF[A](instance)(methodName)(params)

  inline def testCallInstanceViaFoldMissingArgs[A](inline instance: A)(inline methodName: String): Data = ${
    testCallInstanceViaFoldMissingArgsImpl[A]('instance, 'methodName)
  }
  private def testCallInstanceViaFoldMissingArgsImpl[A: Type](
      instance: Expr[A],
      methodName: Expr[String]
  )(using q: Quotes): Expr[Data] =
    new MethodsFixtures(q).testCallInstanceViaFoldMissingArgs[A](instance)(methodName)

  inline def testConstructNamedTuple[A]: Data = ${ testConstructNamedTupleImpl[A] }
  private def testConstructNamedTupleImpl[A: Type](using q: Quotes): Expr[Data] =
    new MethodsFixtures(q).testConstructNamedTuple[A]

  inline def testNamedTupleFieldExtraction[A](inline instance: A): Data = ${
    testNamedTupleFieldExtractionImpl[A]('instance)
  }
  private def testNamedTupleFieldExtractionImpl[A: Type](instance: Expr[A])(using q: Quotes): Expr[Data] =
    new MethodsFixtures(q).testNamedTupleFieldExtraction[A](instance)

  inline def testAnnotationDestructuring[A]: Data = ${ testAnnotationDestructuringImpl[A] }
  private def testAnnotationDestructuringImpl[A: Type](using q: Quotes): Expr[Data] =
    new MethodsFixtures(q).testAnnotationDestructuring[A]

  inline def testTypeAnnotations[A]: Data = ${ testTypeAnnotationsImpl[A] }
  private def testTypeAnnotationsImpl[A: Type](using q: Quotes): Expr[Data] =
    new MethodsFixtures(q).testTypeAnnotations[A]

  inline def testParameterAnnotations[A](inline methodName: String): Data = ${
    testParameterAnnotationsImpl[A]('methodName)
  }
  private def testParameterAnnotationsImpl[A: Type](methodName: Expr[String])(using q: Quotes): Expr[Data] =
    new MethodsFixtures(q).testParameterAnnotations[A](methodName)

  inline def testConstructorParameterAnnotations[A]: Data = ${ testConstructorParameterAnnotationsImpl[A] }
  private def testConstructorParameterAnnotationsImpl[A: Type](using q: Quotes): Expr[Data] =
    new MethodsFixtures(q).testConstructorParameterAnnotations[A]

  inline def testAnnotationsOfType[A](inline methodName: String): Data = ${
    testAnnotationsOfTypeImpl[A]('methodName)
  }
  private def testAnnotationsOfTypeImpl[A: Type](methodName: Expr[String])(using q: Quotes): Expr[Data] =
    new MethodsFixtures(q).testAnnotationsOfType[A](methodName)

  inline def testAnnotationValueDecoded[A]: Data = ${ testAnnotationValueDecodedImpl[A] }
  private def testAnnotationValueDecodedImpl[A: Type](using q: Quotes): Expr[Data] =
    new MethodsFixtures(q).testAnnotationValueDecoded[A]

  inline def testFieldNameReproducer[A]: Data = ${ testFieldNameReproducerImpl[A] }
  private def testFieldNameReproducerImpl[A: Type](using q: Quotes): Expr[Data] =
    new MethodsFixtures(q).testFieldNameReproducer[A]

  inline def testSplicedAnnotationValue[A](inline methodName: String): Data = ${
    testSplicedAnnotationValueImpl[A]('methodName)
  }
  private def testSplicedAnnotationValueImpl[A: Type](methodName: Expr[String])(using q: Quotes): Expr[Data] =
    new MethodsFixtures(q).testSplicedAnnotationValue[A](methodName)

  inline def testFoldAnonymousInstanceMethod[A](inline instance: A, inline methodName: String): Data = ${
    testFoldAnonymousInstanceMethodImpl[A]('instance, 'methodName)
  }
  private def testFoldAnonymousInstanceMethodImpl[A: Type](
      instance: Expr[A],
      methodName: Expr[String]
  )(using q: Quotes): Expr[Data] =
    new MethodsFixtures(q).testFoldAnonymousInstanceMethod[A](instance, methodName)

  inline def testDefaultValueOnGenericMethod[A](inline instance: A, inline methodName: String): Data = ${
    testDefaultValueOnGenericMethodImpl[A]('instance, 'methodName)
  }
  private def testDefaultValueOnGenericMethodImpl[A: Type](
      instance: Expr[A],
      methodName: Expr[String]
  )(using q: Quotes): Expr[Data] =
    new MethodsFixtures(q).testDefaultValueOnGenericMethod[A](instance, methodName)
}
