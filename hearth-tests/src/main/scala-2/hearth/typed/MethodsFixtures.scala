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

  def testMethodOrderingImpl[A: c.WeakTypeTag]: c.Expr[Data] = testMethodOrdering[A]
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

  def testMethodOrdering[A]: Data = macro MethodsFixtures.testMethodOrderingImpl[A]
}
