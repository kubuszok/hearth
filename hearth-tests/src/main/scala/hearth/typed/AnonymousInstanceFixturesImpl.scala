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

  /** Regression for commit 7c82fc9 (Scala 2 anonymous instance scope): the override body splices an expression captured
    * at the call site (a reference to a local val), and the constructed instance's overridden method is called at
    * runtime. A premature `c.typecheck` of the generated `new ... { ... }` tree must not break this.
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

  /** Regression for commit 7c82fc9 (the cats-tagless pattern): the anonymous instance is constructed inside
    * `Expr.splice`, and its override body references a lambda parameter bound by the enclosing [[Expr.quote]]. A
    * premature `c.typecheck` of the generated `new` tree cannot resolve such a reference (the binding only exists in
    * the quote under construction), so construct must keep the tree untyped until final splicing.
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

  /** Companion regression for commit 7c82fc9: override bodies that use the override's own parameters (`Ident` refs to
    * the generated method's params) together with a captured call-site expression.
    */
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
}
