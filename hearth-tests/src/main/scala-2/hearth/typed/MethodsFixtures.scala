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

  def testCallNoInstanceIntMethodImpl[A: c.WeakTypeTag](methodName: c.Expr[String])(params: c.Expr[Int]*): c.Expr[Int] =
    testCallNoInstanceIntMethod[A](methodName)(params)

  def testCallInstanceIntMethodImpl[A: c.WeakTypeTag](instance: c.Expr[A])(methodName: c.Expr[String])(
      params: c.Expr[Int]*
  ): c.Expr[Int] =
    testCallInstanceIntMethod[A](instance)(methodName)(params)
}

object MethodsFixtures {

  def testConstructorsExtraction[A]: Data = macro MethodsFixtures.testConstructorsExtractionImpl[A]

  def testMethodsExtraction[A](excluding: String*): Data = macro MethodsFixtures.testMethodsExtractionImpl[A]

  def testMethodDefaults[A](methodName: String): Data =
    macro MethodsFixtures.testMethodDefaultsImpl[A]

  def testCallNoInstanceIntMethod[A](methodName: String)(params: Int*): Int =
    macro MethodsFixtures.testCallNoInstanceIntMethodImpl[A]

  def testCallInstanceIntMethod[A](instance: A)(methodName: String)(params: Int*): Int =
    macro MethodsFixtures.testCallInstanceIntMethodImpl[A]
}
