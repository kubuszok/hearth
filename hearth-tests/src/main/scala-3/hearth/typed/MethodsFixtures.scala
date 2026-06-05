package hearth
package typed

import hearth.data.Data

import scala.quoted.*

final private class MethodsFixtures(q: Quotes) extends MacroCommonsScala3(using q), MethodsFixturesImpl

object MethodsFixtures {

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

  inline def testMethodProperties[A](inline methodName: String): Data = ${
    testMethodPropertiesImpl[A]('methodName)
  }
  private def testMethodPropertiesImpl[A: Type](methodName: Expr[String])(using q: Quotes): Expr[Data] =
    new MethodsFixtures(q).testMethodProperties[A](methodName)

  inline def testMethodExpectations[A](inline methodName: String): Data = ${
    testMethodExpectationsImpl[A]('methodName)
  }
  private def testMethodExpectationsImpl[A: Type](methodName: Expr[String])(using q: Quotes): Expr[Data] =
    new MethodsFixtures(q).testMethodExpectations[A](methodName)

  inline def testConstructorExpectations[A]: Data = ${ testConstructorExpectationsImpl[A] }
  private def testConstructorExpectationsImpl[A: Type](using q: Quotes): Expr[Data] =
    new MethodsFixtures(q).testConstructorExpectations[A]

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
