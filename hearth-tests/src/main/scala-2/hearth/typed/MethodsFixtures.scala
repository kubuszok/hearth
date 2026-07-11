package hearth
package typed

import hearth.data.Data

import scala.language.experimental.macros
import scala.reflect.macros.blackbox

final private class MethodsFixtures(val c: blackbox.Context) extends MacroCommonsScala2 with MethodsFixturesImpl {

  // [hearth#176]

  def testFoldSubstitutesTypeArgsImpl: c.Expr[Data] = testFoldSubstitutesTypeArgs

  def testConstructorsExtractionImpl[A: c.WeakTypeTag]: c.Expr[Data] =
    testConstructorsExtraction[A]

  def testMethodsExtractionImpl[A: c.WeakTypeTag](excluding: c.Expr[String]*): c.Expr[Data] =
    testMethodsExtraction[A](excluding)

  def testMethodDefaultsImpl[A: c.WeakTypeTag](methodName: c.Expr[String]): c.Expr[Data] =
    testMethodDefaults[A](methodName)

  def testConstructWithDefaultsImpl[A: c.WeakTypeTag](params: c.Expr[Int]*): c.Expr[Data] =
    testConstructWithDefaults[A](params)

  def testCallNoInstanceMethodWithDefaultsImpl[A: c.WeakTypeTag](methodName: c.Expr[String])(
      params: c.Expr[Int]*
  ): c.Expr[Data] =
    testCallNoInstanceMethodWithDefaults[A](methodName)(params)

  def testCallInstanceMethodWithDefaultsImpl[A: c.WeakTypeTag](instance: c.Expr[A])(methodName: c.Expr[String])(
      params: c.Expr[Int]*
  ): c.Expr[Data] =
    testCallInstanceMethodWithDefaults[A](instance)(methodName)(params)

  def testCallNoInstanceIntMethodImpl[A: c.WeakTypeTag](methodName: c.Expr[String])(params: c.Expr[Int]*): c.Expr[Int] =
    testCallNoInstanceIntMethod[A](methodName)(params)

  def testCallInstanceIntMethodImpl[A: c.WeakTypeTag](instance: c.Expr[A])(methodName: c.Expr[String])(
      params: c.Expr[Int]*
  ): c.Expr[Int] =
    testCallInstanceIntMethod[A](instance)(methodName)(params)

  def testParameterPropertiesImpl[A: c.WeakTypeTag](methodName: c.Expr[String]): c.Expr[Data] =
    testParameterProperties[A](methodName)

  def testMethodPropertiesImpl[A: c.WeakTypeTag](methodName: c.Expr[String]): c.Expr[Data] =
    testMethodProperties[A](methodName)

  def testAccessorMetadataImpl[A: c.WeakTypeTag](methodName: c.Expr[String]): c.Expr[Data] =
    testAccessorMetadata[A](methodName)

  def testMethodVisibilityImpl[A: c.WeakTypeTag](methodName: c.Expr[String]): c.Expr[Data] =
    testMethodVisibility[A](methodName)

  def testCallVarargIntMethodImpl[A: c.WeakTypeTag](instance: c.Expr[A])(methodName: c.Expr[String])(
      params: c.Expr[Int]*
  ): c.Expr[Int] =
    testCallVarargIntMethod[A](instance)(methodName)(params)

  def testConstructVarargCtorImpl[A: c.WeakTypeTag](params: c.Expr[String]*): c.Expr[Data] =
    testConstructVarargCtor[A](params)

  def testMethodOrderingImpl[A: c.WeakTypeTag]: c.Expr[Data] = testMethodOrdering[A]

  def testMethodExpectationsImpl[A: c.WeakTypeTag](methodName: c.Expr[String]): c.Expr[Data] =
    testMethodExpectations[A](methodName)

  def testConstructorExpectationsImpl[A: c.WeakTypeTag]: c.Expr[Data] =
    testConstructorExpectations[A]

  def testMethodAppliedToStringImpl[A: c.WeakTypeTag](instance: c.Expr[A])(methodName: c.Expr[String]): c.Expr[Data] =
    testMethodAppliedToString[A](instance)(methodName)

  def testConstructorAppliedToStringImpl[A: c.WeakTypeTag]: c.Expr[Data] =
    testConstructorAppliedToString[A]

  def testMethodPrettyPrintImpl[A: c.WeakTypeTag](methodName: c.Expr[String]): c.Expr[Data] =
    testMethodPrettyPrint[A](methodName)

  def testCallInstanceViaFoldImpl[A: c.WeakTypeTag](instance: c.Expr[A])(methodName: c.Expr[String])(
      params: c.Expr[Int]*
  ): c.Expr[Data] =
    testCallInstanceViaFold[A](instance)(methodName)(params)

  def testCallConstructorViaFoldImpl[A: c.WeakTypeTag](params: c.Expr[Int]*): c.Expr[Data] =
    testCallConstructorViaFold[A](params)

  def testCallInstanceViaFoldFImpl[A: c.WeakTypeTag](instance: c.Expr[A])(methodName: c.Expr[String])(
      params: c.Expr[Int]*
  ): c.Expr[Data] =
    testCallInstanceViaFoldF[A](instance)(methodName)(params)

  def testCallInstanceViaFoldMissingArgsImpl[A: c.WeakTypeTag](instance: c.Expr[A])(
      methodName: c.Expr[String]
  ): c.Expr[Data] =
    testCallInstanceViaFoldMissingArgs[A](instance)(methodName)

  def testAnnotationDestructuringImpl[A: c.WeakTypeTag]: c.Expr[Data] =
    testAnnotationDestructuring[A]

  def testTypeAnnotationsImpl[A: c.WeakTypeTag]: c.Expr[Data] =
    testTypeAnnotations[A]

  def testParameterAnnotationsImpl[A: c.WeakTypeTag](methodName: c.Expr[String]): c.Expr[Data] =
    testParameterAnnotations[A](methodName)

  def testConstructorParameterAnnotationsImpl[A: c.WeakTypeTag]: c.Expr[Data] =
    testConstructorParameterAnnotations[A]

  def testAnnotationsOfTypeImpl[A: c.WeakTypeTag](methodName: c.Expr[String]): c.Expr[Data] =
    testAnnotationsOfType[A](methodName)

  def testAnnotationValueDecodedImpl[A: c.WeakTypeTag]: c.Expr[Data] =
    testAnnotationValueDecoded[A]

  def testFieldNameReproducerImpl[A: c.WeakTypeTag]: c.Expr[Data] =
    testFieldNameReproducer[A]

  def testSplicedAnnotationValueImpl[A: c.WeakTypeTag](methodName: c.Expr[String]): c.Expr[Data] =
    testSplicedAnnotationValue[A](methodName)

  def testDefaultValueOnGenericMethodImpl[A: c.WeakTypeTag](
      instance: c.Expr[A],
      methodName: c.Expr[String]
  ): c.Expr[Data] =
    testDefaultValueOnGenericMethod[A](instance, methodName)

  def testFoldAnonymousInstanceMethodImpl[A: c.WeakTypeTag](
      instance: c.Expr[A],
      methodName: c.Expr[String]
  ): c.Expr[Data] =
    testFoldAnonymousInstanceMethod[A](instance, methodName)

  def testExposesNameAndAgeImpl[A: c.WeakTypeTag]: c.Expr[Data] = testExposesNameAndAge[A]
}

object MethodsFixtures {

  // Chimney #673
  def testExposesNameAndAge[A]: Data = macro MethodsFixtures.testExposesNameAndAgeImpl[A]

  def testFoldSubstitutesTypeArgs: Data = macro MethodsFixtures.testFoldSubstitutesTypeArgsImpl

  def testConstructorsExtraction[A]: Data = macro MethodsFixtures.testConstructorsExtractionImpl[A]

  def testMethodsExtraction[A](excluding: String*): Data = macro MethodsFixtures.testMethodsExtractionImpl[A]

  def testMethodDefaults[A](methodName: String): Data =
    macro MethodsFixtures.testMethodDefaultsImpl[A]

  def testConstructWithDefaults[A](params: Int*): Data =
    macro MethodsFixtures.testConstructWithDefaultsImpl[A]

  def testCallNoInstanceMethodWithDefaults[A](methodName: String)(params: Int*): Data =
    macro MethodsFixtures.testCallNoInstanceMethodWithDefaultsImpl[A]

  def testCallInstanceMethodWithDefaults[A](instance: A)(methodName: String)(params: Int*): Data =
    macro MethodsFixtures.testCallInstanceMethodWithDefaultsImpl[A]

  def testCallNoInstanceIntMethod[A](methodName: String)(params: Int*): Int =
    macro MethodsFixtures.testCallNoInstanceIntMethodImpl[A]

  def testCallInstanceIntMethod[A](instance: A)(methodName: String)(params: Int*): Int =
    macro MethodsFixtures.testCallInstanceIntMethodImpl[A]

  def testParameterProperties[A](methodName: String): Data =
    macro MethodsFixtures.testParameterPropertiesImpl[A]

  def testMethodProperties[A](methodName: String): Data =
    macro MethodsFixtures.testMethodPropertiesImpl[A]

  def testAccessorMetadata[A](methodName: String): Data =
    macro MethodsFixtures.testAccessorMetadataImpl[A]

  def testMethodVisibility[A](methodName: String): Data =
    macro MethodsFixtures.testMethodVisibilityImpl[A]

  def testCallVarargIntMethod[A](instance: A)(methodName: String)(params: Int*): Int =
    macro MethodsFixtures.testCallVarargIntMethodImpl[A]

  def testConstructVarargCtor[A](params: String*): Data =
    macro MethodsFixtures.testConstructVarargCtorImpl[A]

  def testMethodOrdering[A]: Data = macro MethodsFixtures.testMethodOrderingImpl[A]

  def testMethodExpectations[A](methodName: String): Data =
    macro MethodsFixtures.testMethodExpectationsImpl[A]

  def testConstructorExpectations[A]: Data = macro MethodsFixtures.testConstructorExpectationsImpl[A]

  def testMethodAppliedToString[A](instance: A)(methodName: String): Data =
    macro MethodsFixtures.testMethodAppliedToStringImpl[A]

  def testConstructorAppliedToString[A]: Data =
    macro MethodsFixtures.testConstructorAppliedToStringImpl[A]

  def testMethodPrettyPrint[A](methodName: String): Data =
    macro MethodsFixtures.testMethodPrettyPrintImpl[A]

  def testCallInstanceViaFold[A](instance: A)(methodName: String)(params: Int*): Data =
    macro MethodsFixtures.testCallInstanceViaFoldImpl[A]

  def testCallConstructorViaFold[A](params: Int*): Data =
    macro MethodsFixtures.testCallConstructorViaFoldImpl[A]

  def testCallInstanceViaFoldF[A](instance: A)(methodName: String)(params: Int*): Data =
    macro MethodsFixtures.testCallInstanceViaFoldFImpl[A]

  def testCallInstanceViaFoldMissingArgs[A](instance: A)(methodName: String): Data =
    macro MethodsFixtures.testCallInstanceViaFoldMissingArgsImpl[A]

  def testAnnotationDestructuring[A]: Data =
    macro MethodsFixtures.testAnnotationDestructuringImpl[A]

  def testTypeAnnotations[A]: Data =
    macro MethodsFixtures.testTypeAnnotationsImpl[A]

  def testParameterAnnotations[A](methodName: String): Data =
    macro MethodsFixtures.testParameterAnnotationsImpl[A]

  def testConstructorParameterAnnotations[A]: Data =
    macro MethodsFixtures.testConstructorParameterAnnotationsImpl[A]

  def testAnnotationsOfType[A](methodName: String): Data =
    macro MethodsFixtures.testAnnotationsOfTypeImpl[A]

  def testAnnotationValueDecoded[A]: Data =
    macro MethodsFixtures.testAnnotationValueDecodedImpl[A]

  def testFieldNameReproducer[A]: Data =
    macro MethodsFixtures.testFieldNameReproducerImpl[A]

  def testSplicedAnnotationValue[A](methodName: String): Data =
    macro MethodsFixtures.testSplicedAnnotationValueImpl[A]

  def testDefaultValueOnGenericMethod[A](instance: A, methodName: String): Data =
    macro MethodsFixtures.testDefaultValueOnGenericMethodImpl[A]

  def testFoldAnonymousInstanceMethod[A](instance: A, methodName: String): Data =
    macro MethodsFixtures.testFoldAnonymousInstanceMethodImpl[A]
}
