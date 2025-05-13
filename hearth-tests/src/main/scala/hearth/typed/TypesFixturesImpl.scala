package hearth
package typed

trait TypesFixturesImpl { this: MacroTypedCommons =>

  def testNamesPrinters[A: Type]: Expr[String] =
    Expr(
      s"""Type.shortName:   ${Type.shortName[A]}
         |Type.fcqn:        ${Type.fcqn[A]}
         |Type.plainPrint:  ${Type.plainPrint[A]}
         |Type.prettyPrint: ${Type.prettyPrint[A]}""".stripMargin
    )

  def testFlags[A: Type]: Expr[String] = try
    Expr(
      s"""Type.isPrimitive: ${Type.isPrimitive[A]}
         |Type.isBuiltIn:   ${Type.isBuiltIn[A]}
         |
         |Type.isAbstract: ${Type.isAbstract[A]}
         |Type.isFinal:    ${Type.isFinal[A]}
         |
         |Type.isClass:              ${Type.isClass[A]}
         |Type.notBuiltInClass:      ${Type.notBuiltInClass[A]}
         |Type.isPlainOldJavaObject: ${Type.isPlainOldJavaObject[A]}
         |Type.isJavaBean:           ${Type.isJavaBean[A]}
         |
         |Type.isSealed:        ${Type.isSealed[A]}
         |Type.isJavaEnum:      ${Type.isJavaEnum[A]}
         |Type.isJavaEnumValue: ${Type.isJavaEnumValue[A]}
         |
         |Type.isCase:   ${Type.isCase[A]}
         |Type.isObject: ${Type.isObject[A]}
         |Type.isVal:    ${Type.isVal[A]}
         |
         |Type.isPublic:        ${Type.isPublic[A]}
         |Type.isAvailableHere: ${Type.isAvailableHere[A]}""".stripMargin
    )
  catch {
    case e: Throwable =>
      e.printStackTrace()
      throw e
  }
}
