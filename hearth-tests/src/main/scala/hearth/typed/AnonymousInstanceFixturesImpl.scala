package hearth
package typed

import hearth.data.Data

trait AnonymousInstanceFixturesImpl { this: MacroCommons =>

  private val intTypeAI: Type[Int] = Type.of[Int]
  private val stringTypeAI: Type[String] = Type.of[String]
  private val booleanTypeAI: Type[Boolean] = Type.of[Boolean]
  private val traitWithConcreteMethodTypeAI: Type[examples.anonymous_instances.TraitWithConcreteMethod] =
    Type.of[examples.anonymous_instances.TraitWithConcreteMethod]
  private val genericParentOfStringTypeAI: Type[examples.anonymous_instances.GenericParent[String]] =
    Type.of[examples.anonymous_instances.GenericParent[String]]

  private val jvmInheritedMethodNames: Set[String] = Set(
    "clone",
    "equals",
    "finalize",
    "hashCode",
    "toString",
    "getClass",
    "notify",
    "notifyAll",
    "wait",
    "eq",
    "ne",
    "==",
    "!=",
    "##",
    "asInstanceOf",
    "isInstanceOf",
    "synchronized"
  )

  def testAnonymousInstanceParse[A: Type]: Expr[Data] = Expr {
    AnonymousInstance.parse[A] match {
      case ClassViewResult.Compatible(ai) =>
        Data.map(
          "result" -> Data("compatible"),
          "classParent" -> Data(ai.classParent.map(_._1.plainPrint).getOrElse("<none>")),
          "traitParents" -> Data(ai.traitParents.map(_.plainPrint).mkString(", ")),
          "mustOverride" -> Data(ai.mustOverride.map(_.method.name).mkString(", ")),
          "mayOverride" -> Data(
            ai.mayOverride.filterNot(cm => jvmInheritedMethodNames(cm.method.name)).map(_.method.name).mkString(", ")
          ),
          "cannotOverride" -> Data(
            ai.cannotOverride.filterNot(cm => jvmInheritedMethodNames(cm.method.name)).map(_.method.name).mkString(", ")
          ),
          "diamondConflicts" -> Data(ai.diamondConflicts.map(_.method.name).mkString(", "))
        )
      case ClassViewResult.Incompatible(reason) =>
        Data.map(
          "result" -> Data("incompatible"),
          "reason" -> Data(reason)
        )
    }
  }

  def testAnonymousInstanceParseWithMixins[A: Type](mixin1: ??): Expr[Data] = Expr {
    AnonymousInstance.parseWithMixins[A](List(mixin1)) match {
      case ClassViewResult.Compatible(ai) =>
        Data.map(
          "result" -> Data("compatible"),
          "classParent" -> Data(ai.classParent.map(_._1.plainPrint).getOrElse("<none>")),
          "traitParents" -> Data(ai.traitParents.map(_.plainPrint).mkString(", ")),
          "mustOverride" -> Data(ai.mustOverride.map(_.method.name).mkString(", ")),
          "mayOverride" -> Data(
            ai.mayOverride.filterNot(cm => jvmInheritedMethodNames(cm.method.name)).map(_.method.name).mkString(", ")
          ),
          "cannotOverride" -> Data(
            ai.cannotOverride.filterNot(cm => jvmInheritedMethodNames(cm.method.name)).map(_.method.name).mkString(", ")
          ),
          "diamondConflicts" -> Data(ai.diamondConflicts.map(_.method.name).mkString(", ")),
          "constructorCount" -> Data(ai.classParent.map(_._2.size).getOrElse(0))
        )
      case ClassViewResult.Incompatible(reason) =>
        Data.map(
          "result" -> Data("incompatible"),
          "reason" -> Data(reason)
        )
    }
  }

  def testAnonymousInstanceConstruct[A: Type]: Expr[String] = {
    implicit val IntType: Type[Int] = intTypeAI
    implicit val StringType: Type[String] = stringTypeAI
    implicit val BooleanType: Type[Boolean] = booleanTypeAI

    AnonymousInstance.parse[A] match {
      case ClassViewResult.Compatible(ai) =>
        constructWithDefaults(ai)
      case ClassViewResult.Incompatible(reason) =>
        Expr(s"incompatible: $reason")
    }
  }

  def testAnonymousInstanceConstructWithMixins[A: Type](mixin1: ??): Expr[String] = {
    implicit val IntType: Type[Int] = intTypeAI
    implicit val StringType: Type[String] = stringTypeAI
    implicit val BooleanType: Type[Boolean] = booleanTypeAI

    AnonymousInstance.parseWithMixins[A](List(mixin1)) match {
      case ClassViewResult.Compatible(ai) =>
        constructWithDefaults(ai)
      case ClassViewResult.Incompatible(reason) =>
        Expr(s"incompatible: $reason")
    }
  }

  def testAnonymousInstanceConstructWithCtorIndex[A: Type](ctorIndex: Expr[Int]): Expr[String] = {
    implicit val IntType: Type[Int] = intTypeAI
    implicit val StringType: Type[String] = stringTypeAI
    implicit val BooleanType: Type[Boolean] = booleanTypeAI

    val idx = ctorIndex match {
      case Expr(i) => i
      case _       => 0
    }

    AnonymousInstance.parse[A] match {
      case ClassViewResult.Compatible(ai) =>
        val ctor = ai.classParent.flatMap(_._2.lift(idx))
        val ctorArgs: Arguments = ctor match {
          case Some(c) => buildDefaultCtorArgs(c)
          case None    => Map.empty
        }
        val overrides = buildDefaultOverrides(ai)
        constructAndSplice(ai, ctor, ctorArgs, overrides)
      case ClassViewResult.Incompatible(reason) =>
        Expr(s"incompatible: $reason")
    }
  }

  def testAnonymousInstanceParseInaccessible: Expr[Data] = {
    val inaccessible =
      UntypedType.fromClassName("hearth.examples.anonymous_instances.PackagePrivateTrait").as_??
    import inaccessible.Underlying as Inaccessible
    AnonymousInstance.parse[Inaccessible] match {
      case ClassViewResult.Compatible(_) =>
        Expr(Data.map("result" -> Data("compatible")))
      case ClassViewResult.Incompatible(reason) =>
        Expr(Data.map("result" -> Data("incompatible"), "reason" -> Data(reason)))
    }
  }

  def testAnonymousInstanceConstructNoOverrides[A: Type]: Expr[String] = {
    implicit val IntType: Type[Int] = intTypeAI
    implicit val StringType: Type[String] = stringTypeAI
    implicit val BooleanType: Type[Boolean] = booleanTypeAI

    AnonymousInstance.parse[A] match {
      case ClassViewResult.Compatible(ai) =>
        val ctor = ai.classParent.flatMap(_._2.headOption)
        val ctorArgs: Arguments = ctor match {
          case Some(c) => buildDefaultCtorArgs(c)
          case None    => Map.empty
        }
        constructAndSplice(ai, ctor, ctorArgs, Map.empty)
      case ClassViewResult.Incompatible(reason) =>
        Expr(s"incompatible: $reason")
    }
  }

  def testAnonymousInstanceConstructNoOverridesWithMixins[A: Type](mixin1: ??): Expr[String] = {
    implicit val IntType: Type[Int] = intTypeAI
    implicit val StringType: Type[String] = stringTypeAI
    implicit val BooleanType: Type[Boolean] = booleanTypeAI

    AnonymousInstance.parseWithMixins[A](List(mixin1)) match {
      case ClassViewResult.Compatible(ai) =>
        val ctor = ai.classParent.flatMap(_._2.headOption)
        val ctorArgs: Arguments = ctor match {
          case Some(c) => buildDefaultCtorArgs(c)
          case None    => Map.empty
        }
        constructAndSplice(ai, ctor, ctorArgs, Map.empty)
      case ClassViewResult.Incompatible(reason) =>
        Expr(s"incompatible: $reason")
    }
  }

  def testAnonymousInstanceConstructOverridingFinal[A: Type](finalMethodName: Expr[String]): Expr[String] = {
    implicit val IntType: Type[Int] = intTypeAI
    implicit val StringType: Type[String] = stringTypeAI
    implicit val BooleanType: Type[Boolean] = booleanTypeAI

    val name = Expr
      .unapply(finalMethodName)
      .getOrElse(
        Environment.reportErrorAndAbort(s"Method name must be a string literal, got ${finalMethodName.prettyPrint}")
      )

    AnonymousInstance.parse[A] match {
      case ClassViewResult.Compatible(ai) =>
        val finalOverrides: Map[UntypedMethod, OverrideBody] = ai.cannotOverride.collect {
          case cm if cm.method.name == name =>
            cm.method.asUntyped -> new OverrideBody {
              def apply(ctx: OverrideContext): Expr_?? = {
                import ctx.returnType.Underlying as R
                if (Type[R] <:< Type[Int]) Expr(0).as_??
                else if (Type[R] <:< Type[String]) Expr("stub").as_??
                else if (Type[R] <:< Type[Boolean]) Expr(false).as_??
                else Expr.quote(null.asInstanceOf[R]).as_??
              }
            }
        }.toMap
        if (finalOverrides.isEmpty)
          Environment.reportErrorAndAbort(s"No final (CannotOverride) method named $name found")
        val overrides = buildDefaultOverrides(ai) ++ finalOverrides
        val ctor = ai.classParent.flatMap(_._2.headOption)
        val ctorArgs: Arguments = ctor match {
          case Some(c) => buildDefaultCtorArgs(c)
          case None    => Map.empty
        }
        constructAndSplice(ai, ctor, ctorArgs, overrides)
      case ClassViewResult.Incompatible(reason) =>
        Expr(s"incompatible: $reason")
    }
  }

  /** On Scala 2 a premature `c.typecheck` of the generated `new ... { ... }` tree breaks an override body that splices
    * an expression captured at the call site (a reference to a local val).
    */
  def testAnonymousInstanceCapturedOverride(captured: Expr[String]): Expr[String] = {
    implicit val StringType: Type[String] = stringTypeAI
    implicit val TraitType: Type[examples.anonymous_instances.TraitWithConcreteMethod] =
      traitWithConcreteMethodTypeAI

    AnonymousInstance.parse[examples.anonymous_instances.TraitWithConcreteMethod] match {
      case ClassViewResult.Compatible(ai) =>
        val overrides: Map[UntypedMethod, OverrideBody] = ai.mustOverride.map { cm =>
          cm.method.asUntyped -> new OverrideBody {
            def apply(ctx: OverrideContext): Expr_?? =
              Expr.quote(Expr.splice(captured) + "!").as_??
          }
        }.toMap
        ai.construct(None, Map.empty, overrides) match {
          case Right(constructed) =>
            Expr.quote {
              Expr.splice(constructed).abstractMethod
            }
          case Left(errors) =>
            Expr(s"errors: ${errors.toVector.mkString("; ")}")
        }
      case ClassViewResult.Incompatible(reason) =>
        Expr(s"incompatible: $reason")
    }
  }

  /** The cats-tagless pattern: the instance is constructed inside `Expr.splice`, and its override body references a
    * lambda parameter bound by the enclosing [[Expr.quote]]. A premature `c.typecheck` of the generated `new` tree
    * cannot resolve such a reference (the binding only exists in the quote under construction), so construct must keep
    * the tree untyped until final splicing.
    */
  def testAnonymousInstanceOverrideReferencingQuoteParam: Expr[String => String] =
    Expr.quote { (prefix: String) =>
      val _ = prefix
      Expr.splice {
        buildInstanceReturning(Expr.quote(prefix))
      }
    }

  private def buildInstanceReturning(prefixExpr: Expr[String]): Expr[String] = {
    implicit val StringType: Type[String] = stringTypeAI
    implicit val TraitType: Type[examples.anonymous_instances.TraitWithConcreteMethod] =
      traitWithConcreteMethodTypeAI

    AnonymousInstance.parse[examples.anonymous_instances.TraitWithConcreteMethod] match {
      case ClassViewResult.Compatible(ai) =>
        val overrides: Map[UntypedMethod, OverrideBody] = ai.mustOverride.map { cm =>
          cm.method.asUntyped -> new OverrideBody {
            def apply(ctx: OverrideContext): Expr_?? =
              Expr.quote(Expr.splice(prefixExpr) + "!").as_??
          }
        }.toMap
        ai.construct(None, Map.empty, overrides) match {
          case Right(constructed) =>
            Expr.quote {
              Expr.splice(constructed).abstractMethod
            }
          case Left(errors) =>
            Expr(s"errors: ${errors.toVector.mkString("; ")}")
        }
      case ClassViewResult.Incompatible(reason) =>
        Expr(s"incompatible: $reason")
    }
  }

  /** Derives an instance inside `Expr.splice` with an override body built in a `ValDefs.createVal` scope, as a real
    * derivation would. On Scala 3 the created val must be owned by the nested method, not the macro-entry splice, or
    * `-Xcheck-macros` aborts with "Block contains definition with different owners". Deriving TWO instances in one
    * expansion also checks that splice scoping does not leak from one to the other.
    */
  def testAnonymousInstanceValDefBodyInSplice: Expr[String] =
    Expr.quote {
      val outer = "base"
      Expr.splice {
        buildInstanceWithValDefBody(Expr.quote(outer), "-one")
      }
    }

  def testAnonymousInstanceTwoValDefInstancesInSplice: Expr[String] =
    Expr.quote {
      val outer = "base"
      Expr.splice {
        buildTwoInstancesWithValDefBody(Expr.quote(outer))
      }
    }

  private def buildTwoInstancesWithValDefBody(captured: Expr[String]): Expr[String] = {
    implicit val StringType: Type[String] = stringTypeAI
    val first = buildInstanceWithValDefBody(captured, "-one")
    val second = buildInstanceWithValDefBody(captured, "-two")
    Expr.quote(Expr.splice(first) + "|" + Expr.splice(second))
  }

  private def buildInstanceWithValDefBody(captured: Expr[String], suffix: String): Expr[String] = {
    implicit val StringType: Type[String] = stringTypeAI
    implicit val TraitType: Type[examples.anonymous_instances.TraitWithConcreteMethod] =
      traitWithConcreteMethodTypeAI
    val suffixExpr = Expr(suffix)
    AnonymousInstance.parse[examples.anonymous_instances.TraitWithConcreteMethod] match {
      case ClassViewResult.Compatible(ai) =>
        val overrides: Map[UntypedMethod, OverrideBody] = ai.mustOverride.map { cm =>
          cm.method.asUntyped -> new OverrideBody {
            def apply(ctx: OverrideContext): Expr_?? = {
              // DirectStyle runs the continuation on a DirectStyleExecutor thread, like a real derivation; together
              // with construction inside a splice and two instances, it is what trips the -Xcheck-macros scope checker.
              val vd: ValDefs[Expr[String]] = fp.DirectStyle[ValDefs].scoped { runSafe =>
                val c: Expr[String] = runSafe(ValDefs.createVal(captured, "captured"))
                Expr.quote(Expr.splice(c) + Expr.splice(suffixExpr))
              }
              vd.close.as_??
            }
          }
        }.toMap
        ai.construct(None, Map.empty, overrides) match {
          case Right(constructed) =>
            Expr.quote {
              Expr.splice(constructed).abstractMethod
            }
          case Left(errors) =>
            Expr(s"errors: ${errors.toVector.mkString("; ")}")
        }
      case ClassViewResult.Incompatible(reason) =>
        Expr(s"incompatible: $reason")
    }
  }

  def testAnonymousInstanceOverrideUsingParams(captured: Expr[String]): Expr[String] = {
    implicit val StringType: Type[String] = stringTypeAI
    implicit val ParentType: Type[examples.anonymous_instances.GenericParent[String]] =
      genericParentOfStringTypeAI

    AnonymousInstance.parse[examples.anonymous_instances.GenericParent[String]] match {
      case ClassViewResult.Compatible(ai) =>
        val overrides: Map[UntypedMethod, OverrideBody] = ai.mustOverride.map { cm =>
          cm.method.asUntyped -> new OverrideBody {
            def apply(ctx: OverrideContext): Expr_?? =
              if (cm.method.name == "transform") ctx.parameters.head
              else Expr.quote(Expr.splice(captured) + "!").as_??
          }
        }.toMap
        ai.construct(None, Map.empty, overrides) match {
          case Right(constructed) =>
            Expr.quote {
              val instance = Expr.splice(constructed)
              instance.value + " " + instance.transform("echo")
            }
          case Left(errors) =>
            Expr(s"errors: ${errors.toVector.mkString("; ")}")
        }
      case ClassViewResult.Incompatible(reason) =>
        Expr(s"incompatible: $reason")
    }
  }

  /** Each overload gets a distinct body, so overrides collapsed into a single method by name would show in the result.
    */
  def testAnonymousInstanceConstructOverloads: Expr[String] = {
    implicit val IntType: Type[Int] = intTypeAI
    implicit val StringType: Type[String] = stringTypeAI
    implicit val OverloadsType: Type[examples.anonymous_instances.TraitWithOverloads] =
      Type.of[examples.anonymous_instances.TraitWithOverloads]

    AnonymousInstance.parse[examples.anonymous_instances.TraitWithOverloads] match {
      case ClassViewResult.Compatible(ai) =>
        val overrides: Map[UntypedMethod, OverrideBody] = ai.mustOverride.map { cm =>
          cm.method.asUntyped -> new OverrideBody {
            def apply(ctx: OverrideContext): Expr_?? = {
              import ctx.returnType.Underlying as R
              val paramTypes = cm.method.totalParameters.flatten.toList.map { case (_, p) => p.tpe.plainPrint }
              val tag =
                if (paramTypes == List("scala.Int")) "int->string"
                else if (paramTypes == List("java.lang.String")) 1
                else if (paramTypes == List("scala.Int", "scala.Int")) 2
                else "?"
              if (Type[R] <:< Type[String]) Expr(tag.toString).as_??
              else Expr(tag.asInstanceOf[Int]).as_??
            }
          }
        }.toMap
        ai.construct(None, Map.empty, overrides) match {
          case Right(constructed) =>
            Expr.quote {
              val inst = Expr.splice(constructed)
              inst.overloaded(0) + "|" + inst.overloaded("x").toString + "|" + inst.overloaded(0, 0).toString
            }
          case Left(errors) =>
            Expr(s"errors: ${errors.toVector.mkString("; ")}")
        }
      case ClassViewResult.Incompatible(reason) =>
        Expr(s"incompatible: $reason")
    }
  }

  def testAnonymousInstanceConstructGeneric: Expr[String] = {
    implicit val StringType: Type[String] = stringTypeAI
    implicit val GenericType: Type[examples.anonymous_instances.TraitWithGenericMethod] =
      Type.of[examples.anonymous_instances.TraitWithGenericMethod]

    AnonymousInstance.parse[examples.anonymous_instances.TraitWithGenericMethod] match {
      case ClassViewResult.Compatible(ai) =>
        val overrides: Map[UntypedMethod, OverrideBody] = ai.mustOverride.map { cm =>
          cm.method.asUntyped -> new OverrideBody {
            def apply(ctx: OverrideContext): Expr_?? =
              ctx.parameters.head
          }
        }.toMap
        ai.construct(None, Map.empty, overrides) match {
          case Right(constructed) =>
            Expr.quote {
              val inst = Expr.splice(constructed)
              inst.identity("hello") + "|" + inst.identity(42).toString
            }
          case Left(errors) =>
            Expr(s"errors: ${errors.toVector.mkString("; ")}")
        }
      case ClassViewResult.Incompatible(reason) =>
        Expr(s"incompatible: $reason")
    }
  }

  def testAnonymousInstanceConstructGenericConcreteReturn: Expr[String] = {
    implicit val StringType: Type[String] = stringTypeAI
    implicit val GenericType: Type[examples.anonymous_instances.TraitWithGenericConcreteReturn] =
      Type.of[examples.anonymous_instances.TraitWithGenericConcreteReturn]

    AnonymousInstance.parse[examples.anonymous_instances.TraitWithGenericConcreteReturn] match {
      case ClassViewResult.Compatible(ai) =>
        val overrides: Map[UntypedMethod, OverrideBody] = ai.mustOverride.map { cm =>
          cm.method.asUntyped -> new OverrideBody {
            def apply(ctx: OverrideContext): Expr_?? = {
              import ctx.returnType.Underlying as R
              // returnType MUST be String here, not Any — otherwise this branch is not taken and the body is
              // typed `Any`, which fails to override a method declared to return String.
              if (Type[R] =:= Type[String]) Expr("described").as_??
              else Expr(s"wrong-return-type: ${Type[R].plainPrint}").as_??
            }
          }
        }.toMap
        ai.construct(None, Map.empty, overrides) match {
          case Right(constructed) =>
            Expr.quote {
              val inst = Expr.splice(constructed)
              inst.describe("hello") + "|" + inst.describe(42)
            }
          case Left(errors) =>
            Expr(s"errors: ${errors.toVector.mkString("; ")}")
        }
      case ClassViewResult.Incompatible(reason) =>
        Expr(s"incompatible: $reason")
    }
  }

  /** `emptyList[T]: List[T]` has no value parameters, so `ctx.typeParameters` is the only way to name `T`. */
  def testAnonymousInstanceConstructGenericFactory: Expr[String] = {
    implicit val StringType: Type[String] = stringTypeAI
    implicit val FactoryType: Type[examples.anonymous_instances.TraitWithGenericFactory] =
      Type.of[examples.anonymous_instances.TraitWithGenericFactory]

    AnonymousInstance.parse[examples.anonymous_instances.TraitWithGenericFactory] match {
      case ClassViewResult.Compatible(ai) =>
        val overrides: Map[UntypedMethod, OverrideBody] = ai.mustOverride.map { cm =>
          cm.method.asUntyped -> new OverrideBody {
            def apply(ctx: OverrideContext): Expr_?? = {
              val tParam = ctx.typeParameters.head
              import tParam.Underlying as T
              implicit val listOfT: Type[List[T]] = Type.of[List[T]]
              Expr.quote(List.empty[T]).as_??
            }
          }
        }.toMap
        ai.construct(None, Map.empty, overrides) match {
          case Right(constructed) =>
            Expr.quote {
              val inst = Expr.splice(constructed)
              inst.emptyList[String].mkString("[", ",", "]") + "|" + inst.emptyList[Int].sum.toString
            }
          case Left(errors) =>
            Expr(s"errors: ${errors.toVector.mkString("; ")}")
        }
      case ClassViewResult.Incompatible(reason) =>
        Expr(s"incompatible: $reason")
    }
  }

  /** A symbolic member has to be synthesized under its encoded name (`$plus`) to override `+`. */
  def testAnonymousInstanceConstructSymbolic: Expr[String] = {
    implicit val OpsType: Type[examples.anonymous_instances.TraitWithSymbolicMethod] =
      Type.of[examples.anonymous_instances.TraitWithSymbolicMethod]

    AnonymousInstance.parse[examples.anonymous_instances.TraitWithSymbolicMethod] match {
      case ClassViewResult.Compatible(ai) =>
        val overrides: Map[UntypedMethod, OverrideBody] = ai.mustOverride.map { cm =>
          cm.method.asUntyped -> new OverrideBody {
            def apply(ctx: OverrideContext): Expr_?? = ctx.parameters.head
          }
        }.toMap
        ai.construct(None, Map.empty, overrides) match {
          case Right(constructed) =>
            Expr.quote {
              val inst = Expr.splice(constructed)
              (inst + 41).toString
            }
          case Left(errors) => Expr(s"errors: ${errors.toVector.mkString("; ")}")
        }
      case ClassViewResult.Incompatible(reason) => Expr(s"incompatible: $reason")
    }
  }

  /** Unless the override keeps the trailing clause implicit, it does not match `run` and the member stays abstract. */
  def testAnonymousInstanceConstructImplicitParam: Expr[String] = {
    implicit val StringType: Type[String] = stringTypeAI
    implicit val RunType: Type[examples.anonymous_instances.TraitWithImplicitParam] =
      Type.of[examples.anonymous_instances.TraitWithImplicitParam]

    AnonymousInstance.parse[examples.anonymous_instances.TraitWithImplicitParam] match {
      case ClassViewResult.Compatible(ai) =>
        val overrides: Map[UntypedMethod, OverrideBody] = ai.mustOverride.map { cm =>
          cm.method.asUntyped -> new OverrideBody {
            def apply(ctx: OverrideContext): Expr_?? = {
              val cmdE = ctx.parameters.head
              val cfgE = ctx.parameters(1)
              import cmdE.Underlying as Cmd
              import cfgE.Underlying as Cfg
              Expr
                .quote(Expr.splice(cmdE.value: Expr[Cmd]).toString + Expr.splice(cfgE.value: Expr[Cfg]).toString)
                .as_??
            }
          }
        }.toMap
        ai.construct(None, Map.empty, overrides) match {
          case Right(constructed) =>
            Expr.quote {
              implicit val cfg: Int = 7
              val inst = Expr.splice(constructed)
              inst.run("go")
            }
          case Left(errors) => Expr(s"errors: ${errors.toVector.mkString("; ")}")
        }
      case ClassViewResult.Incompatible(reason) => Expr(s"incompatible: $reason")
    }
  }

  /** An abstract `val` cannot be overridden by a `def`, so a `val` member has to be synthesized. */
  def testAnonymousInstanceConstructAbstractVal: Expr[String] = {
    implicit val StringType: Type[String] = stringTypeAI
    implicit val HasValType: Type[examples.anonymous_instances.TraitWithAbstractVal] =
      Type.of[examples.anonymous_instances.TraitWithAbstractVal]

    AnonymousInstance.parse[examples.anonymous_instances.TraitWithAbstractVal] match {
      case ClassViewResult.Compatible(ai) =>
        val overrides: Map[UntypedMethod, OverrideBody] = ai.mustOverride.map { cm =>
          cm.method.asUntyped -> new OverrideBody {
            def apply(ctx: OverrideContext): Expr_?? = Expr("the-val").as_??
          }
        }.toMap
        ai.construct(None, Map.empty, overrides) match {
          case Right(constructed) =>
            Expr.quote {
              val inst = Expr.splice(constructed)
              inst.abstractVal
            }
          case Left(errors) => Expr(s"errors: ${errors.toVector.mkString("; ")}")
        }
      case ClassViewResult.Incompatible(reason) => Expr(s"incompatible: $reason")
    }
  }

  /** The override's `this.type` has to refer to the synthesized subtype, or returning `ctx.self` does not typecheck. */
  def testAnonymousInstanceConstructThisType: Expr[String] = {
    implicit val IntType: Type[Int] = intTypeAI
    implicit val ChainType: Type[examples.anonymous_instances.TraitWithThisTypeReturn] =
      Type.of[examples.anonymous_instances.TraitWithThisTypeReturn]

    AnonymousInstance.parse[examples.anonymous_instances.TraitWithThisTypeReturn] match {
      case ClassViewResult.Compatible(ai) =>
        val overrides: Map[UntypedMethod, OverrideBody] = ai.mustOverride.map { cm =>
          cm.method.asUntyped -> new OverrideBody {
            def apply(ctx: OverrideContext): Expr_?? =
              // The fluent-mock pattern: a `this.type` method must return `self`; an ordinary method must return a
              // real value. `returnsThisType` is the only signal that distinguishes them (the resolved `returnType`
              // cannot). A wrong flag is a compile error here (self isn't an Int; 7 isn't a this.type).
              if (ctx.returnsThisType) ctx.self
              else Expr(7).as_??
          }
        }.toMap
        ai.construct(None, Map.empty, overrides) match {
          case Right(constructed) =>
            Expr.quote {
              val inst = Expr.splice(constructed)
              (inst.chain eq inst).toString + "|" + inst.sibling.toString
            }
          case Left(errors) => Expr(s"errors: ${errors.toVector.mkString("; ")}")
        }
      case ClassViewResult.Incompatible(reason) => Expr(s"incompatible: $reason")
    }
  }

  private def constructWithDefaults[A: Type](ai: AnonymousInstance[A])(implicit
      IntType: Type[Int],
      StringType: Type[String],
      BooleanType: Type[Boolean]
  ): Expr[String] = {
    val ctor = ai.classParent.flatMap(_._2.headOption)
    val ctorArgs: Arguments = ctor match {
      case Some(c) => buildDefaultCtorArgs(c)
      case None    => Map.empty
    }
    val overrides = buildDefaultOverrides(ai)
    constructAndSplice(ai, ctor, ctorArgs, overrides)
  }

  private def constructAndSplice[A: Type](
      ai: AnonymousInstance[A],
      ctor: Option[Method],
      ctorArgs: Arguments,
      overrides: Map[UntypedMethod, OverrideBody]
  )(implicit StringType: Type[String]): Expr[String] =
    ai.construct(ctor, ctorArgs, overrides) match {
      case Right(constructed) =>
        Expr.quote {
          val _ = Expr.splice(constructed)
          "success"
        }
      case Left(errors) =>
        Expr(s"errors: ${errors.toVector.mkString("; ")}")
    }

  private def buildDefaultCtorArgs(c: Method)(implicit
      IntType: Type[Int],
      StringType: Type[String],
      BooleanType: Type[Boolean]
  ): Arguments =
    c.totalParameters.flatten.toList.map { case (name, param) =>
      import param.tpe.Underlying as P
      if (Type[P] <:< Type[Int]) name -> Expr(0).as_??
      else if (Type[P] <:< Type[String]) name -> Expr("").as_??
      else name -> Expr.quote(null.asInstanceOf[P]).as_??
    }.toMap

  private def buildDefaultOverrides[A: Type](ai: AnonymousInstance[A])(implicit
      IntType: Type[Int],
      StringType: Type[String],
      BooleanType: Type[Boolean]
  ): Map[UntypedMethod, OverrideBody] =
    (ai.mustOverride ++ ai.diamondConflicts).map { cm =>
      cm.method.asUntyped -> new OverrideBody {
        def apply(ctx: OverrideContext): Expr_?? = {
          import ctx.returnType.Underlying as R
          if (Type[R] <:< Type[Int]) Expr(0).as_??
          else if (Type[R] <:< Type[String]) Expr("stub").as_??
          else if (Type[R] <:< Type[Boolean]) Expr(false).as_??
          else Expr.quote(null.asInstanceOf[R]).as_??
        }
      }
    }.toMap

  def testAnonymousInstanceClassifiedMethodsWithMixins[A: Type](mixin1: ??): Expr[Data] = Expr {
    AnonymousInstance.parseWithMixins[A](List(mixin1)) match {
      case ClassViewResult.Compatible(ai) =>
        Data(
          ai.classifiedMethods
            .filterNot(cm => jvmInheritedMethodNames(cm.method.name))
            .map(cm => Data(s"${cm.method.name}: ${cm.classification} @ ${cm.declaredIn.plainPrint}"))
        )
      case ClassViewResult.Incompatible(reason) => Data(s"incompatible: $reason")
    }
  }

  def testAnonymousInstanceConstructWithoutConstructor[A: Type]: Expr[String] = {
    implicit val IntType: Type[Int] = intTypeAI
    implicit val StringType: Type[String] = stringTypeAI
    implicit val BooleanType: Type[Boolean] = booleanTypeAI

    AnonymousInstance.parse[A] match {
      case ClassViewResult.Compatible(ai) =>
        constructAndSplice(ai, None, Map.empty, buildDefaultOverrides(ai))
      case ClassViewResult.Incompatible(reason) =>
        Expr(s"incompatible: $reason")
    }
  }

  /** The override returns its by-name argument, so the runtime result shows the argument reached the body. */
  def testAnonymousInstanceConstructByName: Expr[String] = {
    implicit val StringType: Type[String] = stringTypeAI
    implicit val ByNameType: Type[examples.classesedge.AITraitWithByName] =
      Type.of[examples.classesedge.AITraitWithByName]

    AnonymousInstance.parse[examples.classesedge.AITraitWithByName] match {
      case ClassViewResult.Compatible(ai) =>
        val overrides: Map[UntypedMethod, OverrideBody] = ai.mustOverride.map { cm =>
          cm.method.asUntyped -> new OverrideBody {
            def apply(ctx: OverrideContext): Expr_?? = ctx.parameters.head
          }
        }.toMap
        ai.construct(None, Map.empty, overrides) match {
          case Right(constructed) =>
            Expr.quote {
              val inst = Expr.splice(constructed)
              inst.lazily(41).toString
            }
          case Left(errors) => Expr(s"errors: ${errors.toVector.mkString("; ")}")
        }
      case ClassViewResult.Incompatible(reason) => Expr(s"incompatible: $reason")
    }
  }

  private val unitTypeAI: Type[Unit] = Type.of[Unit]

  /** The getter returns a constant and the setter does nothing, so reading after writing shows both were overridden. */
  def testAnonymousInstanceConstructVar: Expr[String] = {
    implicit val StringType: Type[String] = stringTypeAI
    implicit val UnitType: Type[Unit] = unitTypeAI
    implicit val HasVarType: Type[examples.classesedge.AITraitWithVar] =
      Type.of[examples.classesedge.AITraitWithVar]

    AnonymousInstance.parse[examples.classesedge.AITraitWithVar] match {
      case ClassViewResult.Compatible(ai) =>
        val overrides: Map[UntypedMethod, OverrideBody] = ai.mustOverride.map { cm =>
          cm.method.asUntyped -> new OverrideBody {
            def apply(ctx: OverrideContext): Expr_?? =
              if (ctx.parameters.isEmpty) Expr("the-var").as_??
              else Expr.quote(()).as_??
          }
        }.toMap
        ai.construct(None, Map.empty, overrides) match {
          case Right(constructed) =>
            Expr.quote {
              val inst = Expr.splice(constructed)
              inst.abstractVar = "ignored"
              inst.abstractVar
            }
          case Left(errors) => Expr(s"errors: ${errors.toVector.mkString("; ")}")
        }
      case ClassViewResult.Incompatible(reason) => Expr(s"incompatible: $reason")
    }
  }
}
