package hearth
package typed

import hearth.data.Data

import scala.language.experimental.macros
import scala.reflect.macros.blackbox

final private class MethodsFixtures(val c: blackbox.Context) extends MacroCommonsScala2 with MethodsFixturesImpl {

  // [hearth#176]

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

  def testMethodPrettyPrintImpl[A: c.WeakTypeTag](methodName: c.Expr[String]): c.Expr[Data] =
    testMethodPrettyPrint[A](methodName)

  def testCallInstanceViaFoldImpl[A: c.WeakTypeTag](instance: c.Expr[A])(methodName: c.Expr[String])(
      params: c.Expr[Int]*
  ): c.Expr[Data] =
    testCallInstanceViaFold[A](instance)(methodName)(params)

  def testCallConstructorViaFoldImpl[A: c.WeakTypeTag](params: c.Expr[Int]*): c.Expr[Data] =
    testCallConstructorViaFold[A](params)

  def testAnnotationDestructuringImpl[A: c.WeakTypeTag]: c.Expr[Data] =
    testAnnotationDestructuring[A]

  def testParameterAnnotationsImpl[A: c.WeakTypeTag](methodName: c.Expr[String]): c.Expr[Data] =
    testParameterAnnotations[A](methodName)

  def testConstructorParameterAnnotationsImpl[A: c.WeakTypeTag]: c.Expr[Data] =
    testConstructorParameterAnnotations[A]

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
}

object MethodsFixtures {

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

  def testCallVarargIntMethod[A](instance: A)(methodName: String)(params: Int*): Int =
    macro MethodsFixtures.testCallVarargIntMethodImpl[A]

  def testConstructVarargCtor[A](params: String*): Data =
    macro MethodsFixtures.testConstructVarargCtorImpl[A]

  def testMethodOrdering[A]: Data = macro MethodsFixtures.testMethodOrderingImpl[A]

  def testMethodExpectations[A](methodName: String): Data =
    macro MethodsFixtures.testMethodExpectationsImpl[A]

  def testConstructorExpectations[A]: Data = macro MethodsFixtures.testConstructorExpectationsImpl[A]

  def testMethodPrettyPrint[A](methodName: String): Data =
    macro MethodsFixtures.testMethodPrettyPrintImpl[A]

  def testCallInstanceViaFold[A](instance: A)(methodName: String)(params: Int*): Data =
    macro MethodsFixtures.testCallInstanceViaFoldImpl[A]

  def testCallConstructorViaFold[A](params: Int*): Data =
    macro MethodsFixtures.testCallConstructorViaFoldImpl[A]

  def testAnnotationDestructuring[A]: Data =
    macro MethodsFixtures.testAnnotationDestructuringImpl[A]

  def testParameterAnnotations[A](methodName: String): Data =
    macro MethodsFixtures.testParameterAnnotationsImpl[A]

  def testConstructorParameterAnnotations[A]: Data =
    macro MethodsFixtures.testConstructorParameterAnnotationsImpl[A]

  def testDefaultValueOnGenericMethod[A](instance: A, methodName: String): Data =
    macro MethodsFixtures.testDefaultValueOnGenericMethodImpl[A]

  def testFoldAnonymousInstanceMethod[A](instance: A, methodName: String): Data =
    macro MethodsFixtures.testFoldAnonymousInstanceMethodImpl[A]
}
