package hearth
package untyped

import scala.collection.immutable.ListMap
import scala.util.chaining.*

trait UntypedMethodsScala3 extends UntypedMethods { this: MacroCommonsScala3 =>

  import quotes.*, quotes.reflect.*

  import UntypedType.platformSpecific.{positionOf, symbolAvailable}

  final class UntypedParameter private (val method: UntypedMethod, val symbol: Symbol, val index: Int)
      extends UntypedParameterMethods {

    override def name: String = symbol.name
    override def position: Option[Position] = positionOf(symbol)

    override def annotations: List[UntypedExpr] = symbol.annotations

    override def isByName: Boolean = symbol.typeRef.simplified match {
      case ByNameType(_) => true
      case _             => false
    }
    override def isImplicit: Boolean = symbol.flags.is(Flags.Private)
    override def hasDefault: Boolean = symbol.flags.is(Flags.HasDefault)
  }

  object UntypedParameter extends UntypedParameterModule {

    def parse(method: UntypedMethod, symbol: Symbol, index: Int): Either[String, UntypedParameter] =
      if symbol.flags.is(Flags.Param) then Right(new UntypedParameter(method, symbol, index))
      else Left(s"Expected param Symbol, got $symbol")
  }

  object UntypedParameters extends UntypedParametersModule {

    override def toTyped[Instance: Type](untyped: UntypedParameters): Parameters = {
      lazy val instanceTpe = UntypedType.fromTyped[Instance]
      lazy val method = untyped.head.head._2.method // If params are empty it would throw... unless we don't use it.

      // Constructor methods still have to have their type parameters manually applied, even if we know the exact type of their class.
      lazy val appliedIfNecessary =
        if instanceTpe.typeArgs.isEmpty && method.symbol.isClassConstructor then instanceTpe.memberType(method.symbol)
        else instanceTpe.memberType(method.symbol).appliedTo(instanceTpe.typeArgs)
      lazy val typesByParamName = appliedIfNecessary match {
        // Monomorphic type - no type parameters to re-apply
        case MethodType(names, types, _) => names.zip(types).toMap
        // Polymorphic type - type parameters are applied to the method type
        case PolyType(_, _, MethodType(names, types, AppliedType(_, typeRefs))) =>
          val typeArgumentByAlias = typeRefs.zip(instanceTpe.typeArgs).toMap
          val typeArgumentByName: Map[String, TypeRepr] =
            names
              .zip(types)
              .toMap
              .view
              .mapValues { tpe =>
                typeArgumentByAlias.getOrElse(tpe, tpe)
              }
              .toMap
          typeArgumentByName
        // Type is monomorphic, but because we it got its types applied.
        case AppliedType(MethodType(names, types, _), typeRefs) =>
          val typeArgumentByAlias = typeRefs.zip(instanceTpe.typeArgs).toMap
          val typeArgumentByName: Map[String, TypeRepr] =
            names
              .zip(types)
              .toMap
              .view
              .mapValues { tpe =>
                typeArgumentByAlias.getOrElse(tpe, tpe)
              }
              .toMap
          typeArgumentByName
        // Unknown type - should never happen unless we messed up.
        // $COVERAGE-OFF$ Should never happen unless we messed up
        case out =>
          val methodName = if method.isConstructor then "Constructor" else s"Method ${method.name}"
          val typeName = instanceTpe.prettyPrint
          val outTypeName = out.prettyPrint
          hearthAssertionFailed(s"$methodName of $typeName has unrecognized/unsupported format of type: $outTypeName")
        // $COVERAGE-ON$
      }

      untyped.map { params =>
        params.map { case (paramName, untyped) =>
          val param =
            Parameter(asUntyped = untyped, untypedInstanceType = instanceTpe, tpe = typesByParamName(paramName).as_??)
          paramName -> param
        }
      }
    }
  }

  final class UntypedMethod private (
      val symbol: Symbol,
      val invocation: Invocation,
      val isDeclared: Boolean,
      val isConstructorArgument: Boolean,
      val isCaseField: Boolean
  ) extends UntypedMethodMethods {

    override def unsafeApply(
        instanceTpe: UntypedType
    )(instance: Option[UntypedExpr], arguments: UntypedArguments): UntypedExpr = {
      type Instance
      given Instance: Type[Instance] = instanceTpe.asTyped[Instance]
      lazy val adaptedArguments = arguments.adaptToParams(instanceTpe, instance, this)
      invocation match {
        case Invocation.Constructor =>
          // new A
          val select = New(TypeTree.of[Instance]).select(symbol)
          // new A[B1, B2, ...] vs new A
          val tree = if instanceTpe.typeArgs.nonEmpty then select.appliedToTypes(instanceTpe.typeArgs) else select
          // new A... or new A() or new A(b1, b2), ...
          tree.appliedToArgss(adaptedArguments)
        case Invocation.OnInstance =>
          instance match {
            // $COVERAGE-OFF$
            case None =>
              hearthAssertionFailed(s"Expected an instance for method $name that is called on an instance")
            // $COVERAGE-ON$
            case Some(instance) =>
              // instance.method, or instance.method(), or instance.method(b1, b2, ...)
              instance.select(symbol).appliedToArgss(adaptedArguments)
          }
        case Invocation.OnModule(module) =>
          // module.method, or module.method(), or module.method(b1, b2, ...)
          module.select(symbol).appliedToArgss(adaptedArguments)
      }
    }

    override lazy val hasTypeParameters: Boolean = symbol.paramSymss.exists(_.exists(_.isType))

    override lazy val parameters: UntypedParameters = {
      val paramss = symbol.paramSymss.filterNot(_.exists(_.isType))
      val indices = paramss.flatten.zipWithIndex.toMap
      paramss
        .map(inner =>
          ListMap.from(inner.map { param =>
            param.name -> UntypedParameter.parse(this, param, indices(param)).toOption.get
          })
        )
    }

    override lazy val name: String = symbol.name
    override def position: Option[Position] = positionOf(symbol)

    override def annotations: List[UntypedExpr] = symbol.annotations

    override def isConstructor: Boolean = symbol.isClassConstructor

    override def isVal: Boolean = symbol.isValDef && !symbol.flags.is(Flags.Mutable)
    override def isVar: Boolean = symbol.flags.is(Flags.Mutable)
    override def isLazy: Boolean = symbol.flags.is(Flags.Lazy)
    override def isDef: Boolean = symbol.isDefDef
    override def isImplicit: Boolean = symbol.flags.is(Flags.Implicit)
    override def isSynthetic: Boolean =
      symbol.flags.is(Flags.Synthetic) || UntypedMethod.methodsConsideredSynthetic(symbol)

    override def isAvailable(scope: Accessible): Boolean = symbolAvailable(symbol, scope)
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
      if symbol.isValDef || symbol.isDefDef then Right(
        new UntypedMethod(
          symbol = symbol,
          invocation =
            if symbol.isClassConstructor then Invocation.Constructor
            else module.map(Invocation.OnModule.apply).getOrElse(Invocation.OnInstance),
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
      lazy val (typeParams, valueParams) = untyped.symbol.paramSymss.partition(_.exists(_.isType))
      untyped.invocation match {
        case Invocation.Constructor =>
          Existential[Method[Instance, *], Instance](
            Method.NoInstance[Instance](untyped, Instance): Method[Instance, Instance]
          )
        case Invocation.OnModule(_) =>
          if !untyped.hasTypeParameters then {
            val returnType = Instance.memberType(untyped.symbol).widenByName match {
              case lambda: LambdaType => lambda.resType.as_??
              case out                => out.as_??
            }
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
          if !untyped.hasTypeParameters then {
            val returnType = Instance.memberType(untyped.symbol).widenByName match {
              case lambda: LambdaType => lambda.resType.as_??
              case out                => out.as_??
            }
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
      Option(instanceTpe.typeSymbol.primaryConstructor)
        .filterNot(_.isNoSymbol)
        .flatMap(
          UntypedMethod
            .parseOption(isDeclared = true, isConstructorArgument = false, isCaseField = false, module = None)
        )
    override def constructors(instanceTpe: UntypedType): List[UntypedMethod] =
      instanceTpe.typeSymbol.declarations
        .filterNot(_.isNoSymbol)
        .filter(_.isClassConstructor)
        .flatMap(
          UntypedMethod
            .parseOption(isDeclared = true, isConstructorArgument = false, isCaseField = false, module = None)
        )
    override def methods(instanceTpe: UntypedType): List[UntypedMethod] = {
      val symbol = instanceTpe.typeSymbol
      // Defined in the type or its parent, or synthetic
      val classMembers = symbol.methodMembers ++ symbol.fieldMembers
      // Defined exatcly in the type
      val classDeclared = symbol.declaredMethods.toSet ++ symbol.declaredFields.toSet ++ declaredByJvmOrScala(symbol)

      val (members, declared, moduleBySymbol) = instanceTpe.companionObject
        .map { case (companionTpe, companionRef) =>
          val companionSymbol = companionTpe.typeSymbol
          // Defined in the companion object or its parent, or synthetic
          val companionMembers =
            (companionSymbol.methodMembers ++ companionSymbol.fieldMembers).filterNot(methodsSkippedInCompanion)
          // Defined exatcly in the companion object
          val companionDeclared =
            (companionSymbol.declaredMethods.toSet ++ companionSymbol.declaredFields.toSet ++ declaredByJvmOrScala(
              companionSymbol
            )).filterNot(methodsSkippedInCompanion)

          val allMembers = classMembers ++ companionMembers
          val allDeclared = classDeclared ++ companionDeclared
          val moduleBySymbol = companionMembers.toList.map(_ -> companionRef).toMap[Symbol, UntypedExpr]
          (allMembers, allDeclared, moduleBySymbol)
        }
        .getOrElse((classMembers, classDeclared, Map.empty[Symbol, UntypedExpr]))

      val constructorArguments = (for {
        primaryConstructor <- Option(symbol.primaryConstructor).toList
        if !primaryConstructor.isNoSymbol
        argument <- primaryConstructor.paramSymss.flatten
      } yield argument.name).toSet
      val caseFields = (for {
        field <- symbol.caseFields
        if !field.isNoSymbol
      } yield field.name).toSet

      sortMethods(
        members
          .filterNot(_.isNoSymbol)
          .filterNot(_.isClassConstructor) // Constructors are handled by `primaryConstructor` and `constructors`
          .filterNot { s =>
            val name = s.name
            (name == "apply" && s.flags
              .is(Flags.Synthetic)) || // Generated apply methods that just forward the arguments to the constructor
            name.contains("$default$") || // Default parameters are methods, but we don't want them
            name == "<clinit>" // Class static initializer is a method, but we don't want it
          }
          .filterNot(excludedMethods)
          .flatMap { s =>
            val fieldName = s.name.pipe { name =>
              if name.endsWith("_=") then name.dropRight(2) else name
            }
            val module = moduleBySymbol.get(s)
            UntypedMethod.parseOption(
              isDeclared = declared(s) && !methodsConsideredSynthetic(s),
              isConstructorArgument = constructorArguments(fieldName),
              isCaseField = caseFields(fieldName),
              module = module
            )(s)
          }
      )
    }

    override def defaultValue(instanceTpe: UntypedType)(param: UntypedParameter): Option[UntypedMethod] =
      if param.hasDefault
      then Some {
        val (names, invocation, decls) = param.method.invocation match {
          case Invocation.Constructor =>
            val (companionTpe, companionRef) = instanceTpe.companionObject.getOrElse {
              // $COVERAGE-OFF$ Should never happen unless someone mess around with type-level representation
              hearthAssertionFailed(s"Expected that ${instanceTpe.prettyPrint} would have a companion object")
              // $COVERAGE-ON$
            }
            val names = possibleConstructorNames
            val invocation = Invocation.OnModule(companionRef)
            val decls = companionTpe.typeSymbol
            (names, invocation, decls)
          case Invocation.OnInstance =>
            val names = List(param.method.name)
            val invocation = Invocation.OnInstance
            val decls = instanceTpe.typeSymbol
            (names, invocation, decls)
          case Invocation.OnModule(module) =>
            val names = List(param.method.name)
            val invocation = Invocation.OnModule(module)
            val decls = module.as_??.Underlying.asUntyped.typeSymbol
            (names, invocation, decls)
        }

        val possibleDefaultNames = names.map(defaultValueMethodName(_, param.index + 1))
        val defaultMethod = possibleDefaultNames.flatMap(decls.declaredMethod).headOption.getOrElse {
          // $COVERAGE-OFF$ Should never happen unless someone mess around with type-level representation
          hearthAssertionFailed(
            s"Expected that ${instanceTpe.prettyPrint}'s constructor parameter `${param.name}` would have default value: attempted `${possibleDefaultNames.mkString(", ")}`, found: ${decls.declarations.mkString(", ")}"
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
      // TODO: figure out somehow if it's inherited and if it's a module
      @scala.annotation.tailrec
      def enclosingOf(symbol: Symbol): Option[UntypedMethod] =
        // not ctor, but we don't want to call it, so it doesn't matter
        if symbol.isNoSymbol then None
        else if symbol.isDefDef then parseCtorOption(symbol)
        else if symbol.isClassDef then parseCtorOption(symbol.primaryConstructor)
        else enclosingOf(symbol.owner)
      enclosingOf(Symbol.spliceOwner)
    }

    // ------------------------------------------------- Special cases handling -------------------------------------------------
    // When behavior between Scala 2 and 3 is different, and it makes sense to align them, we have to decide which behavior is
    // "saner" and which one needs adjustment. Below are methods used to adjust behavior on Scala 3 side.

    // These methods are only available on Scala 3, and we want to align behavior with Scala 2.
    // For now we just exclude them, but in the future we might want to implement them in Scala 2.
    private lazy val excludedMethods = TypeRepr
      .of[java.lang.Object]
      .typeSymbol
      .methodMembers
      .filter { symbol =>
        // Both "asInstanceOf" and "isInstanceOf" exist on both Scala 2 and 3, so I am not sure why we ALSO have these on Scala 3.
        symbol.name == "$asInstanceOf$" || symbol.name == "$isInstanceOf$"
      }
      .toSet

    // We check if something is inherited by comparing declared methods with all methods of the type. But some methods are
    // ensured to exist, even when they do not appear in the source code, and they should not be considered "inherited".
    private lazy val declaredByJvmOrScala = Map {
      val objectSymbol = TypeRepr.of[java.lang.Object].typeSymbol
      val objectExceptionNames = Set("toString", "equals", "hashCode")
      objectSymbol -> objectSymbol.methodMembers.filter(symbol => objectExceptionNames(symbol.name)).toSet
    }.withDefaultValue(Set.empty)

    // For these symbol.isSynthetic flag is false, but we want to consider them synthetic.
    private val methodsConsideredSynthetic = {
      val names = Set("asInstanceOf", "isInstanceOf", "getClass", "synchronized", "==", "!=", "eq", "ne", "##")
      TypeRepr.of[Object].typeSymbol.methodMembers.filter(symbol => names(symbol.name)).toSet
    }

    // We do not want to include methods and fields from java.lang.Object in the companion object.
    // Because the companion class has them and we don't want to mix them when listing methods for companion class.
    private val methodsSkippedInCompanion = {
      val sym = TypeRepr.of[java.lang.Object].typeSymbol
      (sym.methodMembers ++ sym.fieldMembers).toSet
    }
  }
}
