package hearth.demo.allfeatures.internal.compiletime

import hearth.MacroCommons
import hearth.fp.data.NonEmptyList
import hearth.fp.effect.*
import hearth.fp.syntax.*
import hearth.std.*

import hearth.demo.allfeatures.{FastShowPretty, RenderConfig}
import hearth.demo.allfeatures.internal.runtime.FastShowPrettyUtils

trait FastShowPrettyMacrosImpl { this: MacroCommons & StdExtensions =>

  // Entrypoints to the macro

  def deriveInline[A: Type](valueExpr: Expr[A], configExpr: Expr[RenderConfig], levelExpr: Expr[Int]): Expr[String] = {
    implicit val StringBuilder: Type[StringBuilder] = Types.StringBuilder
    implicit val RenderConfig: Type[RenderConfig] = Types.RenderConfig
    implicit val String: Type[String] = Types.String
    implicit val Int: Type[Int] = Types.Int

    deriveFromCtxAndAdaptForEntrypoint[A, String]("FastShowPretty.render") { fromCtx =>
      ValDefs.createVal[StringBuilder](Expr.quote(new StringBuilder)).use { sbVal =>
        ValDefs.createVal[A](valueExpr).use { valueVal =>
          ValDefs.createVal[RenderConfig](configExpr).use { configVal =>
            ValDefs.createVal[Int](levelExpr).use { levelVal =>
              Expr.quote {
                Expr.splice(fromCtx(DerivationCtx.from(sbVal, valueVal, configVal, levelVal))).toString
              }
            }
          }
        }
      }
    }
  }

  def deriveTypeClass[A: Type]: Expr[FastShowPretty[A]] = {
    implicit val FastShowPretty: Type[FastShowPretty[A]] = Types.FastShowPretty[A]
    implicit val RenderConfigType: Type[RenderConfig] = Types.RenderConfig

    deriveFromCtxAndAdaptForEntrypoint[A, FastShowPretty[A]]("FastShowPretty.derived") { fromCtx =>
      Expr.quote {
        new FastShowPretty[A] {

          def render(sb: StringBuilder, config: RenderConfig, level: Int)(value: A): StringBuilder = Expr.splice {
            fromCtx(
              DerivationCtx.from(
                Expr.quote(sb),
                Expr.quote(value),
                Expr.quote(config),
                Expr.quote(level)
              )
            )
          }
        }
      }
    }
  }

  // Handles logging, error reporting and prepending "cached" defs and vals to the result.
  // We used a continuation passing style, to allow sharing the same code between:
  //  - the case that inlines the whole logic to return a String, and
  //  - the case that returns a FastShowPretty instance.

  def deriveFromCtxAndAdaptForEntrypoint[A: Type, Out: Type](macroName: String)(
      provideCtxAndAdapt: (DerivationCtx[A] => Expr[StringBuilder]) => Expr[Out]
  ): Expr[Out] = Log
    .namedScope(
      s"Deriving the value ${Type[A].prettyPrint} for ${Type[Out].prettyPrint} at: ${Environment.currentPosition.prettyPrint}"
    ) {
      MIO.scoped { runSafe =>
        val fromCtx: (DerivationCtx[A] => Expr[StringBuilder]) = (ctx: DerivationCtx[A]) =>
          runSafe {
            for {
              // Enables usage of IsCollection, IsMap, etc.
              _ <- Environment.loadStandardExtensions().toMIO(allowFailures = false)
              result <- deriveResultRecursively[A](using ctx)
              cache <- ctx.cache.get
            } yield cache.toValDefs.use(_ => result)
          }

        provideCtxAndAdapt(fromCtx)
      }
    }
    .flatTap { result =>
      Log.info(s"Derived final result for: ${result.prettyPrint}")
    }
    .runToExprOrFail(
      macroName,
      infoRendering = if (shouldWeLogDerivation) RenderFrom(Log.Level.Info) else DontRender,
      errorRendering = RenderFrom(Log.Level.Info)
    ) { (errorLogs, errors) =>
      val errorsRendered = errors
        .map { e =>
          e.getMessage.split("\n").toList match {
            case head :: tail => (("  - " + head) :: tail.map("    " + _)).mkString("\n")
            case _            => "  - " + e.getMessage
          }
        }
        .mkString("\n")
      if (errorLogs.nonEmpty)
        s"""Macro derivation failed with the following errors:
           |$errorsRendered
           |and the following logs:
           |$errorLogs""".stripMargin
      else
        s"""Macro derivation failed with the following errors:
           |$errorsRendered""".stripMargin
    }

  /** Enables logging if we either:
    *   - import [[hearth.demo.allfeatures.debug.logDerivationForFastShowPretty]] in the scope
    *   - have set scalac option `-Xmacro-settings:fastShowPretty.logDerivation=true`
    */
  def shouldWeLogDerivation: Boolean = {
    implicit val LogDerivation: Type[FastShowPretty.LogDerivation] = Types.LogDerivation
    def logDerivationImported = Expr.summonImplicit[FastShowPretty.LogDerivation].isDefined

    def logDerivationSetGlobally = (for {
      data <- Environment.typedSettings.toOption
      fastShowPretty <- data.get("fastShowPretty")
      shouldLog <- fastShowPretty.get("logDerivation").flatMap(_.asBoolean)
    } yield shouldLog).getOrElse(false)

    logDerivationImported || logDerivationSetGlobally
  }

  // Context utilities - instead of passing around multiple types, expressions, helpers,
  // maybe some config options in the future - we can just pass around a single context object.
  // If we would have to pass more things, we can just modify it instead of changing every single method signature.

  final case class DerivationCtx[A](
      tpe: Type[A],
      sb: Expr[StringBuilder],
      value: Expr[A],
      config: Expr[RenderConfig],
      level: Expr[Int],
      cache: MLocal[ValDefsCache]
  ) {

    def nest[B: Type](newValue: Expr[B]): DerivationCtx[B] = copy[B](
      tpe = Type[B],
      value = newValue
    )

    def nestInCache(
        newSb: Expr[StringBuilder],
        newValue: Expr[A],
        newConfig: Expr[RenderConfig],
        newLevel: Expr[Int]
    ): DerivationCtx[A] = copy(
      sb = newSb,
      value = newValue,
      config = newConfig,
      level = newLevel
    )

    def incrementLevel: DerivationCtx[A] = copy(
      level = Expr.quote(Expr.splice(level) + 1)
    )

    // Let us reuse type class instance by "caching" it in a lazy val.
    def getInstance[B: Type]: MIO[Option[Expr[FastShowPretty[B]]]] = {
      implicit val FastShowPrettyB: Type[FastShowPretty[B]] = Types.FastShowPretty[B]
      cache.get0Ary[FastShowPretty[B]]("cached-fast-show-pretty-instance")
    }
    def setInstance[B: Type](instance: Expr[FastShowPretty[B]]): MIO[Unit] = {
      implicit val FastShowPrettyB: Type[FastShowPretty[B]] = Types.FastShowPretty[B]
      cache.buildCachedWith(
        "cached-fast-show-pretty-instance",
        ValDefBuilder.ofLazy[FastShowPretty[B]](s"instance_${Type[B].shortName}")
      )(_ => instance)
    }

    // Let us reuse code derived for some type, by putting all: case class handling or enum handling into a local def.
    def getHelper[B: Type]
        : MIO[Option[(Expr[StringBuilder], Expr[RenderConfig], Expr[Int], Expr[B]) => Expr[StringBuilder]]] = {
      implicit val StringBuilderT: Type[StringBuilder] = Types.StringBuilder
      implicit val RenderConfigT: Type[RenderConfig] = Types.RenderConfig
      implicit val IntT: Type[Int] = Types.Int
      cache.get4Ary[StringBuilder, RenderConfig, Int, B, StringBuilder]("cached-render-method")
    }
    def setHelper[B: Type](
        helper: (Expr[StringBuilder], Expr[RenderConfig], Expr[Int], Expr[B]) => MIO[Expr[StringBuilder]]
    ): MIO[Unit] = {
      implicit val StringBuilderT: Type[StringBuilder] = Types.StringBuilder
      implicit val RenderConfigT: Type[RenderConfig] = Types.RenderConfig
      implicit val IntT: Type[Int] = Types.Int
      val defBuilder =
        ValDefBuilder.ofDef4[StringBuilder, RenderConfig, Int, B, StringBuilder](s"render_${Type[B].shortName}")
      for {
        _ <- cache.forwardDeclare("cached-render-method", defBuilder)
        _ <- MIO.scoped { runSafe =>
          runSafe(cache.buildCachedWith("cached-render-method", defBuilder) { case (_, (sb, config, level, value)) =>
            runSafe(helper(sb, config, level, value))
          })
        }
      } yield ()
    }

    override def toString: String =
      s"render[${tpe.prettyPrint}](sb = ${sb.prettyPrint}, config = ${config.prettyPrint}, level = ${level.prettyPrint})(value = ${value.prettyPrint})"
  }
  object DerivationCtx {

    def from[A: Type](
        sb: Expr[StringBuilder],
        value: Expr[A],
        config: Expr[RenderConfig],
        level: Expr[Int]
    ): DerivationCtx[A] = DerivationCtx(
      tpe = Type[A],
      sb = sb,
      value = value,
      config = config,
      level = level,
      cache = ValDefsCache.mlocal
    )
  }

  def ctx[A](implicit A: DerivationCtx[A]): DerivationCtx[A] = A

  implicit def currentValueType[A: DerivationCtx]: Type[A] = ctx.tpe

  abstract class DerivationRule(val name: String) extends Rule {

    def apply[A: DerivationCtx]: MIO[Rule.Applicability[Expr[StringBuilder]]]
  }

  // Reusable components

  private object Types {

    def FastShowPretty: Type.Ctor1[FastShowPretty] = Type.Ctor1.of[FastShowPretty]
    val LogDerivation: Type[hearth.demo.allfeatures.FastShowPretty.LogDerivation] =
      Type.of[hearth.demo.allfeatures.FastShowPretty.LogDerivation]
    val StringBuilder: Type[StringBuilder] = Type.of[StringBuilder]
    val RenderConfig: Type[RenderConfig] = Type.of[RenderConfig]

    val Boolean: Type[Boolean] = Type.of[Boolean]
    val Byte: Type[Byte] = Type.of[Byte]
    val Short: Type[Short] = Type.of[Short]
    val Int: Type[Int] = Type.of[Int]
    val Long: Type[Long] = Type.of[Long]
    val Float: Type[Float] = Type.of[Float]
    val Double: Type[Double] = Type.of[Double]
    val Char: Type[Char] = Type.of[Char]
    val String: Type[String] = Type.of[String]
  }

  // The actual derivation logic in the form of DerivationCtx[A] ?=> MIO[Expr[StringBuilder]].

  def deriveResultRecursively[A: DerivationCtx]: MIO[Expr[StringBuilder]] =
    Log
      .namedScope(s"Deriving for type ${Type[A].prettyPrint}") {
        Rules(
          UseCachedDefWhenAvailableRule,
          UseImplicitWhenAvailableRule,
          UseBuiltInSupportRule,
          HandleAsValueTypeRule,
          HandleAsMapRule,
          HandleAsCollectionRule,
          HandleAsCaseClassRule,
          HandleAsEnumRule
        )(_[A]).flatMap {
          case Right(result) =>
            Log.info(s"Derived result for ${Type[A].prettyPrint}: ${result.prettyPrint}") >>
              MIO.pure(result)
          case Left(reasons) =>
            val reasonsStrings = reasons.toListMap
              .removed(UseCachedDefWhenAvailableRule)
              .view
              .map { case (rule, reasons) =>
                if (reasons.isEmpty) s"The rule ${rule.name} was not applicable"
                else
                  s" - The rule ${rule.name} was not applicable, for the following reasons: ${reasons.mkString(", ")}"
              }
              .toList
            Log.info(s"Failed to derive result for ${Type[A].prettyPrint}:\n${reasonsStrings.mkString("\n")}") >>
              MIO.fail(DerivationError.UnsupportedType(Type[A].prettyPrint, reasonsStrings))
        }
      }

  // Particular derivation rules - the first one that applies (succeeding OR failing) is used.

  object UseCachedDefWhenAvailableRule extends DerivationRule("use cached def when available") {

    def apply[A: DerivationCtx]: MIO[Rule.Applicability[Expr[StringBuilder]]] =
      Log.info(s"Attempting to use cached definition for ${Type[A].prettyPrint}") >>
        ctx.getInstance[A].flatMap {
          case Some(instance) => callCachedInstance[A](instance)
          case None           =>
            ctx.getHelper[A].flatMap {
              case Some(helperCall) => callCachedHelper[A](helperCall)
              case None             => yieldUnsupportedType[A]
            }
        }

    private def callCachedInstance[A: DerivationCtx](
        instance: Expr[FastShowPretty[A]]
    ): MIO[Rule.Applicability[Expr[StringBuilder]]] =
      Log.info(s"Found cached instance for ${Type[A].prettyPrint}, using it") >> MIO.pure(Rule.matched(Expr.quote {
        Expr
          .splice(instance)
          .render(Expr.splice(ctx.sb), Expr.splice(ctx.config), Expr.splice(ctx.level))(Expr.splice(ctx.value))
      }))

    private def callCachedHelper[A: DerivationCtx](
        helperCall: (Expr[StringBuilder], Expr[RenderConfig], Expr[Int], Expr[A]) => Expr[StringBuilder]
    ): MIO[Rule.Applicability[Expr[StringBuilder]]] =
      Log.info(s"Found cached helper call for ${Type[A].prettyPrint}, using it") >> MIO.pure(
        Rule.matched(helperCall(ctx.sb, ctx.config, ctx.level, ctx.value))
      )

    private def yieldUnsupportedType[A: DerivationCtx]: MIO[Rule.Applicability[Expr[StringBuilder]]] =
      MIO.pure(Rule.yielded(s"The type ${Type[A].prettyPrint} does not have a cached definition"))

  }

  object UseImplicitWhenAvailableRule extends DerivationRule("use implicit when available") {

    lazy val ignoredImplicits = Type.of[FastShowPretty.type].methods.collect {
      case method if method.value.name == "derived" => method.value.asUntyped
    }

    def apply[A: DerivationCtx]: MIO[Rule.Applicability[Expr[StringBuilder]]] =
      Log.info(s"Attempting to use implicit support for ${Type[A].prettyPrint}") >> {
        Types.FastShowPretty[A].summonExprIgnoring(ignoredImplicits*).toEither match {
          case Right(instanceExpr) => cacheImplicitAndUseIt[A](instanceExpr)
          case Left(reason)        => yieldUnsupportedType[A](reason)
        }
      }

    private def cacheImplicitAndUseIt[A: DerivationCtx](
        instanceExpr: Expr[FastShowPretty[A]]
    ): MIO[Rule.Applicability[Expr[StringBuilder]]] =
      Log.info(s"Found implicit ${instanceExpr.prettyPrint}, caching it and using a cached value") >>
        ctx.setInstance[A](instanceExpr) >> UseCachedDefWhenAvailableRule[A]

    private def yieldUnsupportedType[A: DerivationCtx](reason: String): MIO[Rule.Applicability[Expr[StringBuilder]]] =
      MIO.pure(
        Rule.yielded(
          s"The type ${Type[A].prettyPrint} does not have an implicit FastShowPretty instance: $reason"
        )
      )
  }

  object UseBuiltInSupportRule extends DerivationRule("use built-in support when handling primitive types") {

    implicit val Boolean: Type[Boolean] = Types.Boolean
    implicit val Byte: Type[Byte] = Types.Byte
    implicit val Short: Type[Short] = Types.Short
    implicit val Int: Type[Int] = Types.Int
    implicit val Long: Type[Long] = Types.Long
    implicit val Float: Type[Float] = Types.Float
    implicit val Double: Type[Double] = Types.Double
    implicit val Char: Type[Char] = Types.Char
    implicit val String: Type[String] = Types.String

    def apply[A: DerivationCtx]: MIO[Rule.Applicability[Expr[StringBuilder]]] =
      Log.info(s"Attempting to use built-in support for ${Type[A].prettyPrint}") >> MIO {
        if (Type[A] <:< Type[Boolean]) Rule.matched(Expr.quote {
          FastShowPrettyUtils.renderBoolean(Expr.splice(ctx.sb))(Expr.splice(ctx.value.upcast[Boolean]))
        })
        else if (Type[A] <:< Type[Byte]) Rule.matched(Expr.quote {
          FastShowPrettyUtils.renderByte(Expr.splice(ctx.sb))(Expr.splice(ctx.value.upcast[Byte]))
        })
        else if (Type[A] <:< Type[Short]) Rule.matched(Expr.quote {
          FastShowPrettyUtils.renderShort(Expr.splice(ctx.sb))(Expr.splice(ctx.value.upcast[Short]))
        })
        else if (Type[A] <:< Type[Int]) Rule.matched(Expr.quote {
          FastShowPrettyUtils.renderInt(Expr.splice(ctx.sb))(Expr.splice(ctx.value.upcast[Int]))
        })
        else if (Type[A] <:< Type[Long]) Rule.matched(Expr.quote {
          FastShowPrettyUtils.renderLong(Expr.splice(ctx.sb))(Expr.splice(ctx.value.upcast[Long]))
        })
        else if (Type[A] <:< Type[Float]) Rule.matched(Expr.quote {
          FastShowPrettyUtils.renderFloat(Expr.splice(ctx.sb))(Expr.splice(ctx.value.upcast[Float]))
        })
        else if (Type[A] <:< Type[Double]) Rule.matched(Expr.quote {
          FastShowPrettyUtils.renderDouble(Expr.splice(ctx.sb))(Expr.splice(ctx.value.upcast[Double]))
        })
        else if (Type[A] <:< Type[Char]) Rule.matched(Expr.quote {
          FastShowPrettyUtils.renderChar(Expr.splice(ctx.sb))(Expr.splice(ctx.value.upcast[Char]))
        })
        else if (Type[A] <:< Type[String]) Rule.matched(Expr.quote {
          FastShowPrettyUtils.renderString(Expr.splice(ctx.sb))(Expr.splice(ctx.value.upcast[String]))
        })
        else Rule.yielded(s"The type ${Type[A].prettyPrint} is not considered to be a built-in type")
      }
  }

  object HandleAsValueTypeRule extends DerivationRule("handle as value type when possible") {
    implicit val StringBuilder: Type[StringBuilder] = Types.StringBuilder

    def apply[A: DerivationCtx]: MIO[Rule.Applicability[Expr[StringBuilder]]] =
      Log.info(s"Attempting to handle ${Type[A].prettyPrint} as a value type") >> {
        Type[A] match {
          case IsValueType(isValueType) =>
            import isValueType.Underlying as Inner
            deriveValueTypeUnwrapped[A, Inner](isValueType.value)

          case _ =>
            yieldUnsupportedType[A]
        }
      }

    private def deriveValueTypeUnwrapped[A: DerivationCtx, Inner: Type](
        isValueType: IsValueTypeOf[A, Inner]
    ): MIO[Rule.Applicability[Expr[StringBuilder]]] = {
      val unwrappedExpr = isValueType.unwrap(ctx.value)

      for {
        innerResult <- deriveResultRecursively[Inner](using ctx.nest(unwrappedExpr))
      } yield Rule.matched(innerResult)
    }

    private def yieldUnsupportedType[A: DerivationCtx]: MIO[Rule.Applicability[Expr[StringBuilder]]] =
      MIO.pure(Rule.yielded(s"The type ${Type[A].prettyPrint} is not considered to be a value type"))
  }

  object HandleAsMapRule extends DerivationRule("handle as map when possible") {
    implicit val StringBuilder: Type[StringBuilder] = Types.StringBuilder

    def apply[A: DerivationCtx]: MIO[Rule.Applicability[Expr[StringBuilder]]] =
      Log.info(s"Attempting to handle ${Type[A].prettyPrint} as a map") >> {
        Type[A] match {
          case IsMap(isMap) =>
            import isMap.Underlying as Pair
            import isMap.value.{Key, Value}
            deriveMapItems[A, Pair](isMap.value)

          case _ =>
            yieldUnsupportedType[A]
        }
      }

    private def deriveMapItems[A: DerivationCtx, Pair: Type](
        isMap: IsMapOf[A, Pair]
    ): MIO[Rule.Applicability[Expr[StringBuilder]]] = {
      import isMap.{Key, Value}
      val name = Expr(Type[A].shortName)
      val iterableExpr = isMap.asIterable(ctx.value)

      LambdaBuilder
        .of1[Pair]("pair")
        .traverse { pairExpr =>
          val keyExpr = isMap.key(pairExpr)
          val valueExpr = isMap.value(pairExpr)
          for {
            keyResult <- deriveResultRecursively[Key](using ctx.incrementLevel.nest(keyExpr))
            valueResult <- deriveResultRecursively[Value](using ctx.incrementLevel.nest(valueExpr))
          } yield Expr.quote {
            val _ = FastShowPrettyUtils.appendIndent(
              Expr.splice(ctx.sb),
              Expr.splice(ctx.config).indentString,
              Expr.splice(ctx.level) + 1
            )
            val _ = FastShowPrettyUtils.openMapEntry(Expr.splice(ctx.sb))
            val _ = Expr.splice(keyResult)
            val _ = FastShowPrettyUtils.appendMapArrow(Expr.splice(ctx.sb))
            val _ = Expr.splice(valueResult)
            FastShowPrettyUtils.closeMapEntry(Expr.splice(ctx.sb))
          }
        }
        .map { builder =>
          val lambda = builder.build[StringBuilder]
          Rule.matched(Expr.quote {
            val _ = FastShowPrettyUtils.openCollection(Expr.splice(ctx.sb), Expr.splice(name))
            val _ = FastShowPrettyUtils.fillCollection(
              Expr.splice(ctx.sb),
              Expr.splice(iterableExpr),
              Expr.splice(ctx.config).indentString,
              Expr.splice(ctx.level)
            )(Expr.splice(lambda))
            FastShowPrettyUtils.closeCollection(Expr.splice(ctx.sb))
          })
        }
    }

    private def yieldUnsupportedType[A: DerivationCtx]: MIO[Rule.Applicability[Expr[StringBuilder]]] =
      MIO.pure(Rule.yielded(s"The type ${Type[A].prettyPrint} is not considered to be a map"))
  }

  object HandleAsCollectionRule extends DerivationRule("handle as collection when possible") {
    implicit val StringBuilder: Type[StringBuilder] = Types.StringBuilder

    def apply[A: DerivationCtx]: MIO[Rule.Applicability[Expr[StringBuilder]]] =
      Log.info(s"Attempting to handle ${Type[A].prettyPrint} as a collection") >> {
        Type[A] match {
          case IsCollection(isCollection) =>
            import isCollection.Underlying as Item
            deriveCollectionItems(isCollection.value)

          case _ =>
            yieldUnsupportedType[A]
        }
      }

    private def deriveCollectionItems[A: DerivationCtx, Item: Type](
        isCollection: IsCollectionOf[A, Item]
    ): MIO[Rule.Applicability[Expr[StringBuilder]]] = {
      val name = Expr(Type[A].shortName)
      val iterableExpr = isCollection.asIterable(ctx.value)

      LambdaBuilder
        .of1[Item]("item")
        .traverse { itemExpr =>
          deriveResultRecursively[Item](using ctx.incrementLevel.nest(itemExpr)).map { result =>
            Expr.quote {
              val _ = FastShowPrettyUtils.appendIndent(
                Expr.splice(ctx.sb),
                Expr.splice(ctx.config).indentString,
                Expr.splice(ctx.level) + 1
              )
              Expr.splice(result)
            }
          }
        }
        .map { builder =>
          val lambda = builder.build[StringBuilder]
          Rule.matched(Expr.quote {
            val _ = FastShowPrettyUtils.openCollection(Expr.splice(ctx.sb), Expr.splice(name))
            val _ = FastShowPrettyUtils.fillCollection(
              Expr.splice(ctx.sb),
              Expr.splice(iterableExpr),
              Expr.splice(ctx.config).indentString,
              Expr.splice(ctx.level)
            )(Expr.splice(lambda))
            FastShowPrettyUtils.closeCollection(Expr.splice(ctx.sb))
          })
        }
    }

    private def yieldUnsupportedType[A: DerivationCtx]: MIO[Rule.Applicability[Expr[StringBuilder]]] =
      MIO.pure(Rule.yielded(s"The type ${Type[A].prettyPrint} is not considered to be a collection"))
  }

  object HandleAsCaseClassRule extends DerivationRule("handle as case class when possible") {

    def apply[A: DerivationCtx]: MIO[Rule.Applicability[Expr[StringBuilder]]] =
      Log.info(s"Attempting to handle ${Type[A].prettyPrint} as a case class") >> {
        CaseClass.parse[A] match {
          case Some(caseClass) =>
            for {
              _ <- ctx.setHelper[A] { (sb, config, level, value) =>
                deriveCaseClassFields[A](caseClass)(using ctx.nestInCache(sb, value, config, level))
              }
              result <- ctx.getHelper[A].flatMap {
                case Some(helperCall) => MIO.pure(Rule.matched(helperCall(ctx.sb, ctx.config, ctx.level, ctx.value)))
                case None             => yieldUnsupportedType[A]
              }
            } yield result

          case None =>
            yieldUnsupportedType[A]
        }
      }

    private def deriveCaseClassFields[A: DerivationCtx](
        caseClass: CaseClass[A]
    ): MIO[Expr[StringBuilder]] = {
      val name = Expr(Type[A].shortName)

      NonEmptyList.fromList(caseClass.caseFieldValuesAt(ctx.value).toList) match {
        case Some(fieldValues) =>
          fieldValues
            .parTraverse { case (fieldName, fieldValue) =>
              import fieldValue.{Underlying as Field, value as fieldExpr}
              Log.namedScope(s"Deriving the value ${ctx.value.prettyPrint}.$fieldName: ${Field.prettyPrint}") {
                // Use incrementLevel so nested case classes are indented properly
                deriveResultRecursively[Field](using ctx.incrementLevel.nest(fieldExpr)).map { fieldResult =>
                  (fieldName, fieldResult)
                }
              }
            }
            .map { toAppend =>
              val renderLeftParenthesisAndHeadField = toAppend.head match {
                case (fieldName, fieldResult) =>
                  Expr.quote {
                    val _ = Expr
                      .splice(ctx.sb)
                      .append(Expr.splice(name))
                      .append("(\n")
                    val _ = FastShowPrettyUtils
                      .appendIndent(
                        Expr.splice(ctx.sb),
                        Expr.splice(ctx.config).indentString,
                        Expr.splice(ctx.level) + 1
                      )
                      .append(Expr.splice(Expr(fieldName)))
                      .append(" = ")
                    Expr.splice(fieldResult)
                  }
              }
              val renderAllFields = toAppend.tail.foldLeft(renderLeftParenthesisAndHeadField) {
                case (renderPreviousFields, (fieldName, fieldResult)) =>
                  Expr.quote {
                    val _ = Expr
                      .splice(renderPreviousFields)
                      .append(",\n")
                    val _ = FastShowPrettyUtils
                      .appendIndent(
                        Expr.splice(ctx.sb),
                        Expr.splice(ctx.config).indentString,
                        Expr.splice(ctx.level) + 1
                      )
                      .append(Expr.splice(Expr(fieldName)))
                      .append(" = ")
                    Expr.splice(fieldResult)
                  }
              }

              Expr.quote {
                val _ = Expr.splice(renderAllFields).append("\n")
                FastShowPrettyUtils
                  .appendIndent(
                    Expr.splice(ctx.sb),
                    Expr.splice(ctx.config).indentString,
                    Expr.splice(ctx.level)
                  )
                  .append(")")
              }
            }
        case None =>
          MIO.pure {
            Expr.quote {
              Expr.splice(ctx.sb).append(Expr.splice(name)).append("()")
            }
          }
      }
    }

    private def yieldUnsupportedType[A: DerivationCtx]: MIO[Rule.Applicability[Expr[StringBuilder]]] =
      MIO.pure(Rule.yielded(s"The type ${Type[A].prettyPrint} is not considered to be a case class"))
  }

  object HandleAsEnumRule extends DerivationRule("handle as enum when possible") {

    def apply[A: DerivationCtx]: MIO[Rule.Applicability[Expr[StringBuilder]]] =
      Log.info(s"Attempting to handle ${Type[A].prettyPrint} as an enum") >> {
        Enum.parse[A] match {
          case Some(enumm) =>
            for {
              _ <- ctx.setHelper[A] { (sb, config, level, value) =>
                deriveEnumCases[A](enumm)(using ctx.nestInCache(sb, value, config, level))
              }
              result <- ctx.getHelper[A].flatMap {
                case Some(helperCall) => MIO.pure(Rule.matched(helperCall(ctx.sb, ctx.config, ctx.level, ctx.value)))
                case None             => yieldUnsupportedType[A]
              }
            } yield result
          case None =>
            yieldUnsupportedType[A]
        }
      }

    private def deriveEnumCases[A: DerivationCtx](
        enumm: Enum[A]
    ): MIO[Expr[StringBuilder]] = {
      val name = Expr(Type[A].shortName)

      implicit val StringBuilder: Type[StringBuilder] = Types.StringBuilder

      enumm
        .parMatchOn[MIO, StringBuilder](ctx.value) { matched =>
          import matched.{value as enumCaseValue, Underlying as EnumCase}
          Log.namedScope(s"Deriving the value ${enumCaseValue.prettyPrint}: ${EnumCase.prettyPrint}") {
            // Use incrementLevel so nested case classes in enum cases are indented properly
            deriveResultRecursively[EnumCase](using ctx.incrementLevel.nest(enumCaseValue)).map { enumCaseResult =>
              Expr.quote {
                val _ = Expr.splice(ctx.sb).append("(")
                Expr.splice(enumCaseResult).append("): ").append(Expr.splice(name))
              }
            }
          }
        }
        .flatMap {
          case Some(result) =>
            MIO.pure(result)
          case None =>
            MIO.fail(new RuntimeException(s"The type ${Type[A].prettyPrint} does not have any children!"))
        }
    }

    private def yieldUnsupportedType[A: DerivationCtx]: MIO[Rule.Applicability[Expr[StringBuilder]]] =
      MIO.pure(Rule.yielded(s"The type ${Type[A].prettyPrint} is not considered to be an enum"))
  }
}

sealed private[compiletime] trait DerivationError extends util.control.NoStackTrace with Product with Serializable {
  def message: String
  override def getMessage(): String = message
}
private[compiletime] object DerivationError {
  final case class UnsupportedType(tpeName: String, reasons: List[String]) extends DerivationError {
    override def message: String =
      s"The type $tpeName was not handled by any derivation rule:\n${reasons.mkString("\n")}"
  }
}
