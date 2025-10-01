package hearth
package typed

import hearth.data.Data
import hearth.fp.effect.MIO

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

  private val intType: Type[Int] = Type.of[Int]
  private val stringType: Type[String] = Type.of[String]

  def testCaseClassConstructAndParConstruct[A: Type]: Expr[String] = Expr {
    CaseClass.parse[A].fold("<no case class>") { caseClass =>
      // Why it fails to work as a SAM?
      val makeArgument: CaseClass.ConstructField[MIO] = new CaseClass.ConstructField[MIO] {
        def apply(field: Parameter): MIO[Expr[field.tpe.Underlying]] = {
          import field.tpe.Underlying as FieldType
          implicit val IntType: Type[Int] = intType
          implicit val StringType: Type[String] = stringType
          if (FieldType <:< Type[Int]) MIO.pure(Expr(0).upcast[FieldType])
          else if (FieldType <:< Type[String]) MIO.pure(Expr(field.name).upcast[FieldType])
          else MIO.fail(new Exception(s"Field $field.name has wrong type: ${field.tpe.plainPrint}"))
        }
      }
      val sequential = caseClass.construct(makeArgument).map { result =>
        result.fold("<failed to construct sequentail>")(_.plainPrint)
      }
      val parallel = caseClass.parConstruct(makeArgument).map { result =>
        result.fold("<failed to construct parallel>")(_.plainPrint)
      }
      sequential
        .parMap2(parallel) { (sequential, parallel) =>
          s"sequential: $sequential, parallel: $parallel"
        }
        .unsafe
        .runSync
        ._2
        .getOrElse("<failed to construct>")
    }
  }

  def testCaseClassCaseFieldValuesAt[A: Type](expr: Expr[A]): Expr[String] = Expr {
    CaseClass.parse[A].fold("<no case class>") { caseClass =>
      caseClass
        .caseFieldValuesAt(expr)
        .toList
        .sortBy(_._1)
        .map { case (name, value) =>
          s"$name: ${value.plainPrint}"
        }
        .mkString("(", ", ", ")")
    }
  }

  def testEnumMatchOnAndParMatchOn[A: Type](expr: Expr[A]): Expr[String] =
    Enum.parse[A].fold(Expr("<no enum>")) { enumm =>
      implicit val StringType: Type[String] = stringType
      val sequential = enumm
        .matchOn(expr) { matched =>
          import matched.{Underlying as Subtype, value as matchedExpr}
          MIO.pure(Expr(s"subtype name: ${Subtype.plainPrint}, expr: ${matchedExpr.plainPrint}"))
        }
        .map(_.getOrElse(Expr("<failed to perform exhaustive match>")))
      val parallel = enumm
        .parMatchOn(expr) { matched =>
          import matched.{Underlying as Subtype, value as matchedExpr}
          MIO.pure(Expr(s"subtype name: ${Subtype.plainPrint}, expr: ${matchedExpr.plainPrint}"))
        }
        .map(_.getOrElse(Expr("<failed to perform exhaustive match>")))
      sequential
        .parMap2(parallel) { (sequential, parallel) =>
          Expr.quote {
            s"sequential: ${Expr.splice(sequential)}, parallel: ${Expr.splice(parallel)}"
          }
        }
        .unsafe
        .runSync
        ._2
        .getOrElse(Expr("<failed to construct>"))
    }

  def testJavaBeanConstructWithSettersAndParConstructWithSetters[A: Type]: Expr[String] = Expr {
    JavaBean.parse[A].fold("<no java bean>") { javaBean =>
      val setField: JavaBean.SetField[MIO] = new JavaBean.SetField[MIO] {
        def apply(name: String, input: Parameter): MIO[Expr[input.tpe.Underlying]] = {
          import input.tpe.Underlying as FieldType
          implicit val IntType: Type[Int] = intType
          implicit val StringType: Type[String] = stringType
          if (FieldType <:< Type[Int]) MIO.pure(Expr(0).upcast[FieldType])
          else if (FieldType <:< Type[String]) MIO.pure(Expr(name).upcast[FieldType])
          else MIO.fail(new Exception(s"Field $name has wrong type: ${input.tpe.plainPrint}"))
        }
      }
      val sequential = javaBean.constructWithSetters(setField).map { result =>
        result.fold("<failed to construct with setters>")(_.plainPrint)
      }
      val parallel = javaBean.parConstructWithSetters(setField).map { result =>
        result.fold("<failed to construct with setters>")(_.plainPrint)
      }
      sequential
        .parMap2(parallel) { (sequential, parallel) =>
          s"sequential: $sequential, parallel: $parallel"
        }
        .unsafe
        .runSync
        ._2
        .getOrElse("<failed to construct>")
    }
  }
}
