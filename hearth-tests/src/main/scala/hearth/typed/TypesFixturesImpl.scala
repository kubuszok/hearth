package hearth
package typed

import hearth.testdata.{Data, DataSupports}

trait TypesFixturesImpl { this: MacroTypedCommons & DataSupports =>

  def testNamesPrinters[A: Type]: Expr[Data] = Expr(
    Data.map(
      "Type.shortName" -> Data(Type.shortName[A]),
      "Type.fcqn" -> Data(Type.fcqn[A]),
      "Type.plainPrint" -> Data(Type.plainPrint[A]),
      "Type.prettyPrint" -> Data(removeAnsiColors(Type.prettyPrint[A]))
    )
  )

  def testFlags[A: Type]: Expr[Data] = try
    Expr(
      Data.map(
        "Type.isPrimitive" -> Data(Type.isPrimitive[A]),
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
        "Type.isAvailable(Everywhere)" -> Data(Type.isAvailable[A](Everywhere))
      )
    )
  catch {
    case e: Throwable =>
      e.printStackTrace()
      throw e
  }
}
