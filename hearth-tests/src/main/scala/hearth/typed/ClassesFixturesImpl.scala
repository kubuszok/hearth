package hearth
package typed

import hearth.data.Data

/** Fixtured for testing [[ClassesSpec]]. */
trait ClassesFixturesImpl { this: MacroCommons =>

  def testClass[A: Type](excluding: VarArgs[String]): Expr[Data] = {
    val excluded = excluding.toIterable.map {
      case Expr(excluding) => excluding
      case unmatched       =>
        Environment.reportErrorAndAbort(
          s"Excluded methods names must be a sequence of strings literals, got ${unmatched.prettyPrint}"
        )
    }.toSet

    val clazz = Class[A]

    val common = Data.map(
      "constructors" -> Data(clazz.constructors.map(renderConstructor(_))),
      "methods" -> Data(clazz.methods.filterNot(m => excluded(m.value.name)).sortBy(_.value.name).map(renderMethod(_)))
    )
    val asCaseClass = clazz.asCaseClass
      .map(cc =>
        Data.map(
          "primaryConstructor" -> renderConstructor(cc.primaryConstructor),
          "nonPrimaryConstructors" -> Data(cc.nonPrimaryConstructors.map(renderConstructor(_))),
          "caseFields" -> Data(cc.caseFields.map(renderMethod(_)))
        )
      )
      .getOrElse(Data("<no case class>"))
    val asEnum = clazz.asEnum
      .map(e =>
        Data.map(
          "directChildren" -> Data(
            e.directChildren
              .map { case (name, child) => s"$name: ${child.Underlying.plainPrint}" }
              .mkString("(", ", ", ")")
          ),
          "exhaustiveChildren" -> Data(
            e.exhaustiveChildren.fold("<no exhaustive children>")(
              _.toList
                .map { case (name, child) =>
                  s"$name: ${child.Underlying.plainPrint}"
                }
                .mkString("(", ", ", ")")
            )
          )
        )
      )
      .getOrElse(Data("<no enum>"))
    val asJavaBean = clazz.asJavaBean
      .map(jb =>
        Data.map(
          "defaultConstructor" -> renderConstructor(jb.defaultConstructor)
        )
      )
      .getOrElse(Data("<no java bean>"))

    Expr(
      Data.map(
        "commons" -> common,
        "asCaseClass" -> asCaseClass,
        "asEnum" -> asEnum,
        "asJavaBean" -> asJavaBean
      )
    )
  }

  private def renderConstructor[A](constructor: Method.NoInstance[A]): Data =
    renderParameters(constructor.parameters)

  private def renderMethod[A](method: Method.Of[A]): Data = Data.map(
    "name" -> Data(method.value.name),
    "parameters" -> renderParameters(method.value.parameters)
  )

  private def renderParameters[A](parameters: Parameters): Data =
    Data(
      parameters
        .map(params =>
          params.view.map { case (name, param) => s"$name: ${param.tpe.plainPrint}" }.mkString("(", ", ", ")")
        )
        .mkString
    )

  // TODO: case class construct and parConstruct

  // TODO: case class caseFieldValuesAt

  // TODO: enum matchOn and parMatchOn
}
