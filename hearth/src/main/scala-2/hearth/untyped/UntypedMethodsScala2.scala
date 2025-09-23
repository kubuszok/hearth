package hearth
package untyped

import scala.collection.immutable.ListMap

trait UntypedMethodsScala2 extends UntypedMethods { this: MacroCommonsScala2 =>

  import c.universe.*

  import UntypedType.platformSpecific.{positionOf, symbolName}

  final class UntypedParameter private (val method: UntypedMethod, val symbol: TermSymbol, val index: Int)
      extends UntypedParameterMethods {

    override def name: String = symbolName(symbol)
    override def position: Option[Position] = positionOf(symbol)

    override def annotations: List[UntypedExpr] =
      symbol.annotations.map { ann =>
        c.untypecheck(ann.tree)
      }

    override def isByName: Boolean = symbol.isByNameParam
    override def isImplicit: Boolean = symbol.isImplicit
    override def hasDefault: Boolean = symbol.asTerm.isParamWithDefault
  }

  object UntypedParameter extends UntypedParameterModule {

    def parse(method: UntypedMethod, symbol: Symbol, index: Int): Either[String, UntypedParameter] =
      if (symbol.isTerm) Right(new UntypedParameter(method, symbol.asTerm, index))
      else Left(s"Expected param Symbol, got $symbol")
  }

  object UntypedParameters extends UntypedParametersModule {

    override def toTyped[Instance: Type](untyped: UntypedParameters): Parameters = {
      val instanceTpe = UntypedType.fromTyped[Instance]
      lazy val method = untyped.head.head._2.method // If params are empty it would throw... unless we don't use it.

      lazy val typesByParamName = method.symbol.asMethod
        .typeSignatureIn(instanceTpe)
        .paramLists
        .flatten
        .map(param => symbolName(param) -> param.typeSignatureIn(instanceTpe))
        .toMap

      untyped.map { params =>
        params.map { case (paramName, untyped) =>
          val param =
            new Parameter(
              asUntyped = untyped,
              untypedInstanceType = instanceTpe,
              tpe = typesByParamName(paramName).as_??
            )
          paramName -> param
        }
      }
    }
  }

  final class UntypedMethod private (
      val symbol: MethodSymbol,
      val invocation: Invocation,
      val isDeclared: Boolean,
      val isConstructorArgument: Boolean,
      val isCaseField: Boolean
  ) extends UntypedMethodMethods {

    override def unsafeApply(
        instanceTpe: UntypedType
    )(instance: Option[UntypedExpr], arguments: UntypedArguments): UntypedExpr = {
      lazy val adaptedArguments = arguments.adaptToParams(instanceTpe, instance, this)
      invocation match {
        case Invocation.Constructor =>
          // new A... or new A() or new A(b1, b2), ...
          q"new $instanceTpe(...$adaptedArguments)"
        case Invocation.OnInstance =>
          instance match {
            case None =>
              hearthAssertionFailed(s"Expected an instance for method $name that is called on an instance")
            case Some(instance) =>
              // instance.method, or instance.method(), or instance.method(b1, b2, ...)
              q"$instance.$symbol(...$adaptedArguments)"
          }
        case Invocation.OnModule(module) =>
          // module.method, or module.method(), or module.method(b1, b2, ...)
          q"$module.$symbol(...$adaptedArguments)"
      }
    }

    override lazy val hasTypeParameters: Boolean = symbol.typeParams.nonEmpty

    override lazy val parameters: UntypedParameters = {
      val paramss = symbol.paramLists
      val indices = paramss.flatten.zipWithIndex.toMap
      paramss
        .map(inner =>
          ListMap.from(inner.map { param =>
            symbolName(param.asTerm) -> UntypedParameter.parse(this, param.asTerm, indices(param)).toOption.get
          })
        )
    }

    override lazy val name: String = symbolName(symbol)
    override def position: Option[Position] = positionOf(symbol)

    override def annotations: List[UntypedExpr] =
      symbol.annotations.map { ann =>
        c.untypecheck(ann.tree)
      }

    override def isConstructor: Boolean = symbol.isConstructor

    override def isVal: Boolean = symbol.isVal || accessedOf(symbol).exists(_.isVal)
    override def isVar: Boolean = symbol.isVar || accessedOf(symbol).exists(_.isVar)
    override def isLazy: Boolean = symbol.isLazy || accessedOf(symbol).exists(_.isLazy)
    override def isDef: Boolean =
      !isVal && !isLazy && (!isVar || name.endsWith("_=")) // var's setter should both var AND def
    override def isImplicit: Boolean = symbol.isImplicit
    override def isSynthetic: Boolean = symbol.isSynthetic || UntypedMethod.methodsConsideredSynthetic(symbol)

    override def isAvailable(scope: Accessible): Boolean = scope match {
      case Everywhere => symbol.isPublic
      case AtCallSite => false // TODO
    }

    // -------------------------------------------- Special cases handling --------------------------------------------
    // So, the symbol of val/var is no just one symbol - there is a symbol for var/var `fieldName` as a `TermSymbol`,
    // representing the field, but there is also a separate symbol for `def fieldName` as a getter `MethodSymbol`,
    // and - if we are talking about var - a separate symbol for `def fieldName_=` as a setter `MethodSymbol`.
    //
    // Oh. and if we are talking about class constructor argument then I guess it's another one?
    //
    // These symbols are not equal, so if we want to check for .isVal, .isVar, .isLazy, etc, we need to check both the
    // symbol itself and the accessed symbol. FML

    private def accessedOf(symbol: MethodSymbol): Option[TermSymbol] =
      scala.util.Try(symbol.accessed).toOption.filterNot(_ == NoSymbol).collectFirst {
        case s if s.isTerm => s.asTerm
      }
    private def getterOf(symbol: MethodSymbol): Option[MethodSymbol] =
      scala.util.Try(symbol.getter).toOption.filterNot(_ == NoSymbol).collectFirst {
        case s if s.isMethod => s.asMethod
      }
    private def setterOf(symbol: MethodSymbol): Option[MethodSymbol] =
      scala.util.Try(symbol.setter).toOption.filterNot(_ == NoSymbol).collectFirst {
        case s if s.isMethod => s.asMethod
      }
  }

  object UntypedMethod extends UntypedMethodModule {

    private def parse(
        isDeclared: Boolean,
        isConstructorArgument: Boolean,
        isCaseField: Boolean,
        module: Option[UntypedExpr]
    )(
        symbol: Symbol
    ): Either[String, UntypedMethod] =
      if (symbol.isMethod)
        Right(
          new UntypedMethod(
            symbol = symbol.asMethod,
            invocation =
              if (symbol.isConstructor) Invocation.Constructor
              else module.map(Invocation.OnModule).getOrElse(Invocation.OnInstance),
            isDeclared = isDeclared,
            isConstructorArgument = isConstructorArgument,
            isCaseField = isCaseField
          )
        )
      else Left(s"Expected method Symbol, got $symbol")
    private def parseOption(
        isDeclared: Boolean,
        isConstructorArgument: Boolean,
        isCaseField: Boolean,
        module: Option[UntypedExpr]
    )(symbol: Symbol): Option[UntypedMethod] =
      parse(isDeclared, isConstructorArgument, isCaseField, module)(symbol).toOption

    private val parseCtorOption =
      parseOption(isDeclared = true, isConstructorArgument = false, isCaseField = false, module = None)

    override def toTyped[Instance: Type](untyped: UntypedMethod): Method.Of[Instance] = {
      val Instance = UntypedType.fromTyped[Instance]
      untyped.invocation match {
        case Invocation.Constructor =>
          Existential[Method[Instance, *], Instance](
            // TODO: check if the method is not parametric if it's a module
            Method.NoInstance[Instance](untyped, Instance): Method[Instance, Instance]
          )
        case Invocation.OnModule(_) =>
          if (!untyped.hasTypeParameters) {
            val returnType = untyped.symbol.typeSignatureIn(Instance).resultType.as_??
            // TODO: check if the method is not having any other issues, e.g. path-dependent types (we cannot handle them like this)
            import returnType.Underlying as Returned
            Existential[Method[Instance, *], Returned](
              Method.NoInstance[Returned](untyped, Instance): Method[Instance, Returned]
            )
          } else {
            Existential[Method[Instance, *], Nothing](
              Method.Unsupported(untyped, Instance)("Method defines type parameters")
            )
          }
        case Invocation.OnInstance =>
          if (!untyped.hasTypeParameters) {
            val returnType = untyped.symbol.typeSignatureIn(Instance).resultType.as_??
            // TODO: check if the method is not having any other issues, e.g. path-dependent types (we cannot handle them like this)
            import returnType.Underlying as Returned
            Existential[Method[Instance, *], Returned](
              Method.OfInstance[Instance, Returned](untyped, Instance): Method[Instance, Returned]
            )
          } else {
            Existential[Method[Instance, *], Nothing](
              Method.Unsupported(untyped, Instance)("Method defines type parameters")
            )
          }
      }
    }

    override def primaryConstructor(instanceTpe: UntypedType): Option[UntypedMethod] =
      Option(instanceTpe.typeSymbol)
        .filter(_.isClass)
        .map(_.asClass.primaryConstructor)
        .filter(_.isConstructor)
        .flatMap(parseCtorOption)
    override def constructors(instanceTpe: UntypedType): List[UntypedMethod] =
      instanceTpe.decls.filter(_.isConstructor).flatMap(parseCtorOption).toList
    override def methods(instanceTpe: UntypedType): List[UntypedMethod] = {
      // Defined in the type or its parent, or synthetic
      val classMembers = instanceTpe.members
      // Defined exatcly in the type
      val classDeclared = instanceTpe.decls.toSet

      val (members, declared, moduleBySymbol) = instanceTpe.companionObject
        .map { case (companionTpe, companionRef) =>
          // Defined in the companion object or its parent, or synthetic
          val companionMembers = companionTpe.members
          // Defined exatcly in the companion object
          val companionDeclared = companionTpe.decls.toSet

          val allMembers = classMembers ++ companionMembers
          val allDeclared = classDeclared ++ companionDeclared
          val moduleBySymbol = companionMembers.toList.map(_ -> companionRef).toMap[Symbol, UntypedExpr]
          (allMembers, allDeclared, moduleBySymbol)
        }
        .getOrElse((classMembers, classDeclared, Map.empty[Symbol, UntypedExpr]))

      val constructorArguments = (for {
        symbol <- List(instanceTpe.typeSymbol)
        if symbol.isClass
        primaryConstructor = symbol.asClass.primaryConstructor
        if primaryConstructor.isConstructor
        argument <- primaryConstructor.asMethod.paramLists.flatten
      } yield symbolName(argument)).toSet

      sortMethods(
        members
          .filter(_.isMethod)
          .filterNot(_.isConstructor) // Constructors are handled by `primaryConstructor` and `constructors`
          .filterNot { s =>
            val name = symbolName(s)
            name.contains("$default$") || // Default parameters are methods, but we don't want them
            name == "<clinit>" // Class static initializer is a method, but we don't want it
          }
          .flatMap { s =>
            val fieldNames = Set(symbolName(s), symbolName(s) + "_=")
            val module = moduleBySymbol.get(s)
            parseOption(
              isDeclared = declared(s) && !methodsConsideredSynthetic(s),
              isConstructorArgument = (constructorArguments & fieldNames).nonEmpty,
              isCaseField = s.asMethod.isCaseAccessor,
              module = module
            )(s)
          }
          .toList
      )
    }

    override def defaultValue(instanceTpe: UntypedType)(param: UntypedParameter): Option[UntypedMethod] = if (
      param.hasDefault
    )
      Some {
        val (names, invocation, decls) = param.method.invocation match {
          case Invocation.Constructor =>
            val (companionTpe, companionRef) = instanceTpe.companionObject.getOrElse {
              // $COVERAGE-OFF$should never happen unless someone mess around with type-level representation
              hearthAssertionFailed(s"Expected that ${instanceTpe.prettyPrint} would have a companion object")
              // $COVERAGE-ON$
            }
            val names = possibleConstructorNames
            val invocation = Invocation.OnModule(companionRef)
            val decls = companionTpe.decls
            (names, invocation, decls)
          case Invocation.OnInstance =>
            val names = List(param.method.name)
            val invocation = Invocation.OnInstance
            val decls = instanceTpe.decls
            (names, invocation, decls)
          case Invocation.OnModule(module) =>
            val names = List(param.method.name)
            val invocation = Invocation.OnModule(module)
            val decls = module.as_??.Underlying.tpe.decls
            (names, invocation, decls)
        }

        val possibleDefaultNames = names.map(defaultValueMethodName(_, param.index + 1))
        val defaultMethod = decls
          .to(List)
          .collectFirst {
            case method if possibleDefaultNames.contains(method.name.decodedName.toString) => method.asMethod
          }
          .getOrElse {
            // $COVERAGE-OFF$should never happen unless someone mess around with type-level representation
            hearthAssertionFailed(
              s"Expected that ${instanceTpe.prettyPrint}'s constructor parameter `${param.name}` would have default value: attempted `${possibleDefaultNames.mkString(", ")}`, found: ${decls.to(List).mkString(", ")}"
            )
            // $COVERAGE-ON$
          }
        new UntypedMethod(
          symbol = defaultMethod,
          invocation = invocation,
          isDeclared = param.method.isDeclared,
          isConstructorArgument = false,
          isCaseField = false
        )
      }
    else None

    override def enclosing: Option[UntypedMethod] = {
      @scala.annotation.tailrec
      def enclosingOf(symbol: Symbol): Option[UntypedMethod] =
        if (symbol == NoSymbol) None
        else if (symbol.isMethod)
          parseCtorOption(symbol) // not ctor, but we don't want to call it, so it doesn't matter
        else if (symbol.isClass) parseCtorOption(symbol.asClass.primaryConstructor)
        else enclosingOf(symbol.owner)
      enclosingOf(c.internal.enclosingOwner)
    }

    // ------------------------------------------------- Special cases handling -------------------------------------------------
    // When behavior between Scala 2 and 3 is different, and it makes sense to align them, we have to decide which behavior is
    // "saner" and which one needs adjustment. Below are methods used to adjust behavior on Scala 2 side.

    // For these symbol.isSynthetic flag is false, but we want to consider them synthetic.
    private val methodsConsideredSynthetic = {
      val names = Set("asInstanceOf", "isInstanceOf", "getClass", "synchronized", "==", "!=", "eq", "ne", "##")
      c.weakTypeOf[Object].members.filter(symbol => names(symbolName(symbol))).toSet
    }
  }
}
