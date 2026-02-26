package hearth
package treeprinter

trait RuntimeAwareTypePrinterFixturesImpl { this: MacroCommons =>

  def testNoOverride[A: Type]: Expr[String] =
    Type.runtimePlainPrint[A](_ => None)

  def testWithOverride[A: Type]: Expr[String] = {
    val stringType = Type.of[String]
    val intType = Type.of[Int]
    Type.runtimePlainPrint[A] { tpe =>
      import tpe.Underlying
      if (Underlying =:= stringType) Some(Expr("RUNTIME_STRING"))
      else if (Underlying =:= intType) Some(Expr("RUNTIME_INT"))
      else None
    }
  }

  def testPrettyWithOverride[A: Type]: Expr[String] = {
    val stringType = Type.of[String]
    Type.runtimePrettyPrint[A] { tpe =>
      import tpe.Underlying
      if (Underlying =:= stringType) Some(Expr("RUNTIME_STRING"))
      else None
    }
  }

  def testShortWithOverride[A: Type]: Expr[String] = {
    val stringType = Type.of[String]
    Type.runtimeShortPrint[A] { tpe =>
      import tpe.Underlying
      if (Underlying =:= stringType) Some(Expr("RUNTIME_STRING"))
      else None
    }
  }
}
