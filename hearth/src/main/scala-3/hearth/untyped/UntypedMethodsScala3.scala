package hearth
package untyped

import scala.collection.immutable.ListMap
import scala.util.chaining.*

trait UntypedMethodsScala3 extends UntypedMethods { this: MacroCommonsScala3 =>

  import quotes.*, quotes.reflect.*

  import UntypedType.platformSpecific.{positionOf, repositionAnnotation, symbolAvailable}

  // Repeated (vararg) parameters appear in two shapes on Scala 3:
  //   - `scala.<repeated>[A]` (an `AppliedType` of `defn.RepeatedParamClass`) in method signatures,
  //   - `Seq[A] @scala.annotation.internal.Repeated` (an `AnnotatedType`) when widening the parameter's term ref.
  private lazy val repeatedAnnotationSymbol: Symbol = Symbol.requiredClass("scala.annotation.internal.Repeated")
  private lazy val immutableSeqTypeConstructor: TypeRepr = TypeRepr.of[scala.collection.immutable.Seq[Any]] match {
    case AppliedType(tycon, _) => tycon
    // $COVERAGE-OFF$ Should never happen - Seq[Any] is always an AppliedType
    case other => other
    // $COVERAGE-ON$
  }

  private def isRepeatedParamType(tpe: TypeRepr): Boolean = tpe match {
    case AppliedType(tycon, _) => tycon.typeSymbol == defn.RepeatedParamClass
    case AnnotatedType(_, ann) => ann.tpe.typeSymbol == repeatedAnnotationSymbol
    case _                     => false
  }

  /** Normalizes the raw repeated-parameter marker type (`scala.<repeated>[A]` or `Seq[A] @Repeated`) to
    * `scala.collection.immutable.Seq[A]`, so that [[Parameter.tpe]] is a real, usable type, consistent between Scala 2
    * and Scala 3. Non-repeated types are returned unchanged.
    */
  private def normalizeRepeatedParamType(tpe: TypeRepr): TypeRepr = tpe match {
    case AppliedType(tycon, List(elem)) if tycon.typeSymbol == defn.RepeatedParamClass =>
      immutableSeqTypeConstructor.appliedTo(elem)
    case AnnotatedType(underlying, ann) if ann.tpe.typeSymbol == repeatedAnnotationSymbol => underlying
    case other                                                                            => other
  }

  override protected def adaptVarargArgument(expr: UntypedExpr): UntypedExpr = {
    val argTpe = expr.tpe.widenTermRefByName.dealias
    val elementType = argTpe.baseType(immutableSeqTypeConstructor.typeSymbol) match {
      case AppliedType(_, List(elem)) => elem
      case _                          => argTpe.typeArgs.headOption.getOrElse(TypeRepr.of[Any])
    }
    Typed(expr, Inferred(defn.RepeatedParamClass.typeRef.appliedTo(elementType)))
  }

  class UntypedParameter private[UntypedMethodsScala3] (val method: UntypedMethod, val symbol: Symbol, val index: Int)
      extends UntypedParameterMethods {

    override def name: String = symbol.name
    override def position: Option[Position] = positionOf(symbol)

    override def annotations: List[UntypedExpr] = symbol.annotations.map(repositionAnnotation)
    override def annotationTypes: List[UntypedType] = symbol.annotations.map(_.tpe)

    // By-name-ness is not visible on the parameter symbol itself (its termRef widens the ByNameType away),
    // so we look the parameter up in the owning method's signature, where it appears as `ByNameType`.
    override def isByName: Boolean = {
      def loop(tpe: TypeRepr): Boolean = tpe match {
        case MethodType(names, types, returnType) =>
          names.zip(types).collectFirst { case (n, t) if n == symbol.name => t } match {
            case Some(ByNameType(_)) => true
            case Some(_)             => false
            case None                => loop(returnType)
          }
        case PolyType(_, _, returnType) => loop(returnType)
        case _                          => false
      }
      loop(safeMemberType(method.symbol.owner.typeRef, method.symbol))
    }
    override def isVararg: Boolean = isRepeatedParamType(symbol.termRef.widenTermRefByName)
    override def isImplicit: Boolean = symbol.flags.is(Flags.Implicit) || symbol.flags.is(Flags.Given)
    override def hasDefault: Boolean = symbol.flags.is(Flags.HasDefault)

    // The underlying `A` of a by-name parameter `a: => A`. Like `isByName`, by-name-ness is only visible in the owning
    // method's signature (the parameter's own termRef widens the `ByNameType` away), so we read `ByNameType(u)` there.
    override def byNameUnderlying: Option[TypeRepr] = {
      def loop(tpe: TypeRepr): Option[TypeRepr] = tpe match {
        case MethodType(names, types, returnType) =>
          names.zip(types).collectFirst { case (n, t) if n == symbol.name => t } match {
            case Some(ByNameType(u)) => Some(u)
            case Some(_)             => None
            case None                => loop(returnType)
          }
        case PolyType(_, _, returnType) => loop(returnType)
        case _                          => None
      }
      loop(safeMemberType(method.symbol.owner.typeRef, method.symbol))
    }
  }

  object UntypedParameter extends UntypedParameterModule {

    def parse(method: UntypedMethod, symbol: Symbol, index: Int): Either[String, UntypedParameter] =
      if symbol.flags.is(Flags.Param) then Right(new UntypedParameter(method, symbol, index))
      else Left(s"Expected param Symbol, got $symbol")
  }

  private[hearth] def safeMemberType(tpe: TypeRepr, symbol: Symbol): TypeRepr =
    try tpe.memberType(symbol)
    catch { case _: AssertionError => symbol.owner.typeRef.memberType(symbol) }

  object UntypedParameters extends UntypedParametersModule {

    override def toTyped[Instance: Type](untyped: UntypedParameters): Parameters =
      toTypedWith[Instance](
        untyped,
        // If params are empty, `.head` would throw... unless we don't use it (the thunk is forced per-param only).
        () => resolveTypesByParamName(UntypedType.fromTyped[Instance], untyped.head.head._2.method)
      )

    /** Resolves the parameter-name -> parameter-type map for ALL value clauses of `method` as seen from `instanceTpe`.
      * Since the map covers every clause, `UntypedMethod.toTyped` computes it ONCE and shares it between the per-clause
      * expectations and `totalParameters` (previously it was recomputed for each).
      */
    private[hearth] def resolveTypesByParamName(
        instanceTpe: UntypedType,
        method: UntypedMethod
    ): Map[String, TypeRepr] = {
      // Constructor methods still have to have their type parameters manually applied, even if we know the exact type of their class.
      val appliedIfNecessary = {
        val raw =
          if instanceTpe.typeArgs.isEmpty && method.symbol.isClassConstructor then safeMemberType(
            instanceTpe,
            method.symbol
          )
          else safeMemberType(instanceTpe, method.symbol).appliedTo(instanceTpe.typeArgs)
        // Extension methods have a receiver parameter as the first value parameter list in their type,
        // which was already dropped from `parameters` — skip it here too to keep names aligned.
        if method.symbol.flags.is(Flags.ExtensionMethod) then raw match {
          case MethodType(_, _, inner) => inner
          case other                   => other
        }
        else raw
      }
      def collectAllParamTypes(tpe: TypeRepr): Map[String, TypeRepr] = tpe match {
        case MethodType(names, types, res) => names.zip(types).toMap ++ collectAllParamTypes(res)
        case PolyType(_, _, res)           => collectAllParamTypes(res)
        case _                             => Map.empty
      }
      def applyTypeAliases(params: Map[String, TypeRepr], aliases: Map[TypeRepr, TypeRepr]): Map[String, TypeRepr] =
        if aliases.isEmpty then params
        else params.view.mapValues(tpe => aliases.getOrElse(tpe, tpe)).toMap

      appliedIfNecessary match {
        case mt: MethodType =>
          collectAllParamTypes(mt)
        case PolyType(_, _, inner) =>
          val rawParams = collectAllParamTypes(inner)
          inner match {
            case MethodType(_, _, AppliedType(_, typeRefs)) =>
              applyTypeAliases(rawParams, typeRefs.zip(instanceTpe.typeArgs).toMap)
            case _ => rawParams
          }
        case AppliedType(inner, typeRefs) =>
          val rawParams = collectAllParamTypes(inner)
          applyTypeAliases(rawParams, typeRefs.zip(instanceTpe.typeArgs).toMap)
        // $COVERAGE-OFF$
        case out =>
          val methodName = if method.isConstructor then "Constructor" else s"Method ${method.name}"
          val typeName = instanceTpe.prettyPrint
          val outTypeName = out.prettyPrint
          hearthAssertionFailed(s"$methodName of $typeName has unrecognized/unsupported format of type: $outTypeName")
        // $COVERAGE-ON$
      }
    }

    private[hearth] def toTypedWith[Instance: Type](
        untyped: UntypedParameters,
        typesByParamName0: () => Map[String, TypeRepr]
    ): Parameters = {
      lazy val instanceTpe = UntypedType.fromTyped[Instance]

      // Synthetic NamedTuple parameters carry their types directly — no symbol-based resolution needed.
      val isSyntheticNamedTuple = untyped.exists(_.exists(_._2.isInstanceOf[SyntheticNamedTupleParameter]))
      if isSyntheticNamedTuple then return untyped.map { params =>
        params.map { case (paramName, param) =>
          val synParam = param.asInstanceOf[SyntheticNamedTupleParameter]
          paramName -> Parameter(asUntyped = param, untypedInstanceType = instanceTpe, tpe = synParam.fieldType.as_??)
        }
      }

      lazy val typesByParamName = typesByParamName0()

      untyped.map { params =>
        params.flatMap { case (paramName, untyped) =>
          typesByParamName.get(paramName).map { tpe =>
            val param = Parameter(
              asUntyped = untyped,
              untypedInstanceType = instanceTpe,
              tpe = normalizeRepeatedParamType(tpe).as_??
            )
            paramName -> param
          }
        }
      }
    }
  }

  type UntypedTypeParameter = Symbol

  object UntypedTypeParameter extends UntypedTypeParameterModule {
    override def name(param: UntypedTypeParameter): String = param.name
    override def upperBound(param: UntypedTypeParameter): UntypedType = param.typeRef.translucentSuperType match {
      case TypeBounds(_, hi) => hi
      case other             => other
    }
    override def lowerBound(param: UntypedTypeParameter): UntypedType = param.typeRef.translucentSuperType match {
      case TypeBounds(lo, _) => lo
      case _                 => TypeRepr.of[Nothing]
    }
  }

  class UntypedMethod private[UntypedMethodsScala3] (
      val symbol: Symbol,
      val invocation: Invocation,
      val isDeclared: Boolean,
      val isConstructorArgument: Boolean,
      val constructorArgumentIndex: Option[Int],
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

    override lazy val hasTypeParameters: Boolean = typeParameters.flatten.nonEmpty

    override lazy val typeParameters: UntypedTypeParameters =
      symbol.paramSymss.filter(_.exists(_.isType))

    override def unsafeApplyWithTypes(instanceTpe: UntypedType)(
        typeArgs: UntypedTypeArguments,
        instance: Option[UntypedExpr],
        arguments: UntypedArguments
    ): UntypedExpr = {
      type Instance
      given Instance: Type[Instance] = instanceTpe.asTyped[Instance]
      lazy val adaptedArguments = arguments.adaptToParams(instanceTpe, instance, this)
      lazy val typeArgTypes: List[TypeRepr] =
        if typeArgs.isEmpty then Nil
        else typeParameters.flatten.map(tp => typeArgs.get(tp).fold(tp.typeRef: TypeRepr)(_.Underlying.asUntyped))
      invocation match {
        case Invocation.Constructor =>
          val select = New(TypeTree.of[Instance]).select(symbol)
          val tree =
            if typeArgTypes.nonEmpty then select.appliedToTypes(typeArgTypes)
            else if instanceTpe.typeArgs.nonEmpty then select.appliedToTypes(instanceTpe.typeArgs)
            else select
          tree.appliedToArgss(adaptedArguments)
        case Invocation.OnInstance =>
          instance match {
            case None =>
              hearthAssertionFailed(s"Expected an instance for method $name that is called on an instance")
            case Some(instance) =>
              val select = instance.select(symbol)
              val applied = if typeArgTypes.nonEmpty then select.appliedToTypes(typeArgTypes) else select
              applied.appliedToArgss(adaptedArguments)
          }
        case Invocation.OnModule(module) =>
          val select = module.select(symbol)
          val applied = if typeArgTypes.nonEmpty then select.appliedToTypes(typeArgTypes) else select
          applied.appliedToArgss(adaptedArguments)
      }
    }

    override def methodExpectations(instanceTpe: UntypedType): List[UntypedMethodExpectation] = {
      val steps = List.newBuilder[UntypedMethodExpectation]
      if invocation == Invocation.OnInstance then steps += UntypedMethodExpectation.NeedsInstance

      if isConstructor then {
        val valueParamLists = parameters
        if valueParamLists.nonEmpty then {
          val groups = groupParamListsByPathDependency(instanceTpe, valueParamLists)
          groups.foreach(group => steps += UntypedMethodExpectation.NeedsValues(group))
        }
      } else {
        // Walk paramSymss in order to preserve clause interleaving
        val paramss = symbol.paramSymss
        val isExtension = symbol.flags.is(Flags.ExtensionMethod)
        var skippedExtensionReceiver = !isExtension
        var valueParamAccum = List.newBuilder[ListMap[String, UntypedParameter]]
        val indices = paramss.filterNot(_.exists(_.isType)).flatten.zipWithIndex.toMap

        def flushValues(): Unit = {
          val accumulated = valueParamAccum.result()
          if accumulated.nonEmpty then {
            val groups = groupParamListsByPathDependency(instanceTpe, accumulated)
            groups.foreach(group => steps += UntypedMethodExpectation.NeedsValues(group))
          }
          valueParamAccum = List.newBuilder[ListMap[String, UntypedParameter]]
        }

        paramss.foreach { paramList =>
          if paramList.exists(_.isType) then {
            // Type parameter list — flush accumulated values, then add NeedsTypes
            flushValues()
            steps += UntypedMethodExpectation.NeedsTypes(List(paramList))
          } else if !skippedExtensionReceiver then {
            skippedExtensionReceiver = true
          } else {
            val parsed = ListMap.from(paramList.flatMap { param =>
              UntypedParameter.parse(this, param, indices.getOrElse(param, 0)).toOption.map(p => param.name -> p)
            })
            valueParamAccum += parsed
          }
        }
        flushValues()
      }
      steps.result()
    }

    private def groupParamListsByPathDependency(
        instanceTpe: UntypedType,
        paramLists: UntypedParameters
    ): List[UntypedParameters] = {
      if paramLists.sizeIs <= 1 then return List(paramLists)
      if symbol.isNoSymbol then return List(paramLists)

      val methodType = safeMemberType(instanceTpe, symbol).widenByName match {
        case poly: PolyType => poly.resType
        case other          => other
      }

      val paramListTypes = collectMethodTypeParamLists(methodType)
      if paramListTypes.isEmpty then return List(paramLists)

      val allPrecedingParamNames = scala.collection.mutable.Set.empty[String]
      val result = List.newBuilder[UntypedParameters]
      var currentGroup = List.newBuilder[ListMap[String, UntypedParameter]]

      paramLists.zip(paramListTypes).zipWithIndex.foreach { case ((paramList, paramTypeList), idx) =>
        val hasDependency = idx > 0 && paramTypeList.exists { case (_, tpe) =>
          containsTermParamRef(tpe, allPrecedingParamNames.toSet)
        }
        if hasDependency then {
          val built = currentGroup.result()
          if built.nonEmpty then result += built
          currentGroup = List.newBuilder[ListMap[String, UntypedParameter]]
        }
        currentGroup += paramList
        allPrecedingParamNames ++= paramList.keys
      }
      val lastGroup = currentGroup.result()
      if lastGroup.nonEmpty then result += lastGroup
      result.result()
    }

    private def collectMethodTypeParamLists(tpe: TypeRepr): List[List[(String, TypeRepr)]] = tpe match {
      case mt: MethodType => mt.paramNames.zip(mt.paramTypes) :: collectMethodTypeParamLists(mt.resType)
      case _              => Nil
    }

    private def containsTermParamRef(tpe: TypeRepr, precedingParamNames: Set[String]): Boolean = {
      def check(t: TypeRepr): Boolean = t match {
        case TermRef(_, name) if precedingParamNames.contains(name) => true
        case AppliedType(tycon, args)                               => check(tycon) || args.exists(check)
        case AndType(left, right)                                   => check(left) || check(right)
        case OrType(left, right)                                    => check(left) || check(right)
        case TypeBounds(lo, hi)                                     => check(lo) || check(hi)
        case AnnotatedType(tp, _)                                   => check(tp)
        case ByNameType(tp)                                         => check(tp)
        case _                                                      => false
      }
      check(tpe)
    }

    override lazy val parameters: UntypedParameters = {
      val paramss0 = symbol.paramSymss.filterNot(_.exists(_.isType))
      // Extension methods have a receiver parameter as the first value parameter list,
      // which doesn't have Flags.Param and would fail UntypedParameter.parse
      val paramss = if symbol.flags.is(Flags.ExtensionMethod) then paramss0.drop(1) else paramss0
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

    override def annotations: List[UntypedExpr] = symbol.annotations.map(repositionAnnotation)
    override def annotationTypes: List[UntypedType] = symbol.annotations.map(_.tpe)

    override def isConstructor: Boolean = symbol.isClassConstructor

    override def isVal: Boolean = symbol.isValDef && !symbol.flags.is(Flags.Mutable)
    override def isVar: Boolean = symbol.flags.is(Flags.Mutable)
    override def isLazy: Boolean = symbol.flags.is(Flags.Lazy)
    override def isDef: Boolean = symbol.isDefDef
    override def isImplicit: Boolean = symbol.flags.is(Flags.Implicit) || symbol.flags.is(Flags.Given)
    override def isSynthetic: Boolean =
      symbol.flags.is(Flags.Synthetic) || UntypedMethod.methodsConsideredSynthetic(symbol)

    override def isFinal: Boolean = symbol.flags.is(Flags.Final)
    override def isAbstract: Boolean = symbol.flags.is(Flags.Deferred)
    override def isOverride: Boolean = symbol.flags.is(Flags.Override)

    override def isPrivate: Boolean =
      (symbol.flags.is(Flags.Private) || symbol.flags.is(Flags.PrivateLocal)) &&
        symbol.privateWithin.isEmpty
    override def isProtected: Boolean =
      symbol.flags.is(Flags.Protected) &&
        symbol.protectedWithin.isEmpty
    override def privateWithin: Option[String] = symbol.privateWithin.map(_.typeSymbol.name)
    override def protectedWithin: Option[String] = symbol.protectedWithin.map(_.typeSymbol.name)
    override def isAvailable(scope: Accessible): Boolean = symbolAvailable(symbol, scope)

    private def paramTypePrintsImpl(
        instanceTpe: UntypedType,
        hl: hearth.treeprinter.SyntaxHighlight
    ): (List[List[(String, String)]], String) = {
      if symbol.isNoSymbol then {
        val params = parameters.map(_.toList.map { case (n, _) => (n, "?") })
        return (
          params,
          if hl.TypeDefColor.isEmpty then instanceTpe.plainPrint else hl.highlightTypeDef(instanceTpe.plainPrint)
        )
      }
      val rawMemberType = safeMemberType(instanceTpe, symbol).widenByName
      val memberType =
        if isConstructor && instanceTpe.typeArgs.nonEmpty then rawMemberType.appliedTo(instanceTpe.typeArgs)
        else rawMemberType
      def typePrint(t: TypeRepr): String = {
        val raw = t match {
          case AppliedType(tycon, List(elem)) if tycon.typeSymbol == defn.RepeatedParamClass =>
            s"${elem.plainPrint}*"
          case AnnotatedType(underlying, ann) if ann.tpe.typeSymbol == repeatedAnnotationSymbol =>
            underlying.typeArgs.headOption.map(elem => s"${elem.plainPrint}*").getOrElse(underlying.plainPrint)
          case _ => t.plainPrint
        }
        if hl.TypeDefColor.isEmpty then raw else hl.highlightTypeDef(raw)
      }
      def collectParamLists(tpe: TypeRepr): (List[List[(String, String)]], String) = tpe match {
        case mt: MethodType =>
          val paramPairs = mt.paramNames.zip(mt.paramTypes).map { case (n, t) => (n, typePrint(t)) }
          val (rest, ret) = collectParamLists(mt.resType)
          (paramPairs :: rest, ret)
        case pt: PolyType => collectParamLists(pt.resType)
        case other        => (Nil, typePrint(other))
      }
      collectParamLists(memberType)
    }

    override def paramTypePrints(instanceTpe: UntypedType): (List[List[(String, String)]], String) =
      paramTypePrintsImpl(instanceTpe, hearth.treeprinter.SyntaxHighlight.plain)

    override def paramTypePrints(
        instanceTpe: UntypedType,
        hl: hearth.treeprinter.SyntaxHighlight
    ): (List[List[(String, String)]], String) =
      paramTypePrintsImpl(instanceTpe, hl)

    private def signatureSegmentsImpl(
        instanceTpe: UntypedType,
        hl: hearth.treeprinter.SyntaxHighlight
    ): List[String] = {
      if symbol.isNoSymbol then {
        return parameters.map { pl =>
          "(" + pl.map { case (n, _) => s"${hl.highlightValDef(n)}: ?" }.mkString(", ") + ")"
        }
      }
      val (paramListsWithTypes, _) = paramTypePrintsImpl(instanceTpe, hl)

      def renderTypeParam(tp: Symbol): String = {
        val tpName = UntypedTypeParameter.name(tp)
        val coloredName = if hl.TypeDefColor.isEmpty then tpName else hl.highlightTypeDef(tpName)
        val upper = UntypedTypeParameter.upperBoundPrint(tp)
        val lower = UntypedTypeParameter.lowerBoundPrint(tp)
        val isTopBound =
          upper == "scala.Any" || upper.startsWith("scala.Any[") || upper.contains("@") || upper.startsWith("[")
        val isBottomBound = lower == "scala.Nothing" || lower.contains("@") || lower.startsWith("[")
        val coloredUpper = if hl.TypeDefColor.isEmpty then upper else hl.highlightTypeDef(upper)
        val coloredLower = if hl.TypeDefColor.isEmpty then lower else hl.highlightTypeDef(lower)
        if !isTopBound && !isBottomBound then s"$coloredName <: $coloredUpper >: $coloredLower"
        else if !isTopBound then s"$coloredName <: $coloredUpper"
        else if !isBottomBound then s"$coloredName >: $coloredLower"
        else coloredName
      }

      if isConstructor then {
        paramListsWithTypes.zip(parameters).map { case (typedList, untypedList) =>
          "(" + typedList
            .map { case (pName, pType) =>
              val default = untypedList.get(pName).filter(_.hasDefault).map(_ => " = <default>").getOrElse("")
              s"${hl.highlightValDef(pName)}: $pType$default"
            }
            .mkString(", ") + ")"
        }
      } else {
        val paramss = symbol.paramSymss
        val isExtension = symbol.flags.is(Flags.ExtensionMethod)
        var skippedExtensionReceiver = !isExtension
        var valueListIdx = 0
        val segments = List.newBuilder[String]

        paramss.foreach { paramList =>
          if paramList.exists(_.isType) then {
            segments += "[" + paramList.map(renderTypeParam).mkString(", ") + "]"
          } else if !skippedExtensionReceiver then {
            skippedExtensionReceiver = true
          } else {
            if valueListIdx < paramListsWithTypes.length then {
              val typedList = paramListsWithTypes(valueListIdx)
              val untypedList =
                if valueListIdx < parameters.length then parameters(valueListIdx)
                else ListMap.empty[String, UntypedParameter]
              segments += "(" + typedList
                .map { case (pName, pType) =>
                  val default = untypedList.get(pName).filter(_.hasDefault).map(_ => " = <default>").getOrElse("")
                  s"${hl.highlightValDef(pName)}: $pType$default"
                }
                .mkString(", ") + ")"
            } else {
              segments += "(" + paramList.map(p => hl.highlightValDef(p.name)).mkString(", ") + ")"
            }
            valueListIdx += 1
          }
        }
        segments.result()
      }
    }

    override def signatureSegments(instanceTpe: UntypedType): List[String] =
      signatureSegmentsImpl(instanceTpe, hearth.treeprinter.SyntaxHighlight.plain)

    override def signatureSegments(instanceTpe: UntypedType, hl: hearth.treeprinter.SyntaxHighlight): List[String] =
      signatureSegmentsImpl(instanceTpe, hl)
  }

  // NamedTuple synthetic constructor support (Scala 3.7+ only).
  // NamedTuples are opaque type aliases and have no real constructor symbol.
  // We synthesize one that builds the underlying tuple and type-ascribes it.

  /** Synthetic parameter for NamedTuple constructor — carries its type directly instead of using a Symbol. */
  final private class SyntheticNamedTupleParameter(
      override val method: UntypedMethod,
      override val index: Int,
      val fieldName: String,
      val fieldType: TypeRepr
  ) extends UntypedParameter(method, Symbol.noSymbol, index) {
    override def name: String = fieldName
    override def position: Option[Position] = None
    override def annotations: List[UntypedExpr] = Nil
    override def annotationTypes: List[UntypedType] = Nil
    override def isByName: Boolean = false
    override def isVararg: Boolean = false
    override def isImplicit: Boolean = false
    override def hasDefault: Boolean = false
    override def byNameUnderlying: Option[TypeRepr] = None
  }

  /** Synthetic constructor for NamedTuple types.
    *
    * Constructs the underlying regular tuple and type-ascribes it to the NamedTuple type. E.g. for
    * `(name: String, age: Int)` which is `NamedTuple[("name", "age"), (String, Int)]`, the constructor builds a
    * `Tuple2[String, Int]` and ascribes it to the NamedTuple type.
    */
  final private class SyntheticNamedTupleConstructor(
      val fieldNames: List[String],
      val fieldTypes: List[TypeRepr]
  ) extends UntypedMethod(
        symbol = Symbol.noSymbol,
        invocation = Invocation.Constructor,
        isDeclared = true,
        isConstructorArgument = false,
        constructorArgumentIndex = None,
        isCaseField = false
      ) {

    override lazy val hasTypeParameters: Boolean = false
    override lazy val typeParameters: UntypedTypeParameters = Nil

    override lazy val parameters: UntypedParameters = {
      val params = ListMap.from(fieldNames.zip(fieldTypes).zipWithIndex.map { case ((fname, ftype), idx) =>
        fname -> new SyntheticNamedTupleParameter(
          method = this,
          index = idx,
          fieldName = fname,
          fieldType = ftype
        )
      })
      List(params)
    }

    override def methodExpectations(instanceTpe: UntypedType): List[UntypedMethodExpectation] =
      if parameters.nonEmpty then List(UntypedMethodExpectation.NeedsValues(parameters))
      else Nil

    override def unsafeApplyWithTypes(instanceTpe: UntypedType)(
        typeArgs: UntypedTypeArguments,
        instance: Option[UntypedExpr],
        arguments: UntypedArguments
    ): UntypedExpr = unsafeApply(instanceTpe)(instance, arguments)

    override def unsafeApply(
        instanceTpe: UntypedType
    )(instance: Option[UntypedExpr], arguments: UntypedArguments): UntypedExpr = {
      // Build a regular Tuple with arguments in the right order
      val orderedArgs = fieldNames.map { fname =>
        arguments.getOrElse(fname, hearthRequirementFailed(s"Missing argument for field '$fname'"))
      }
      type NT
      given scala.quoted.Type[NT] = instanceTpe.asType.asInstanceOf[scala.quoted.Type[NT]]
      // A NamedTuple is represented at runtime by its underlying value tuple, so casting the tuple to the named-tuple
      // type is sound. We use a cast (rather than a type ascription) for the EmptyTuple/TupleXXL shapes because they
      // are not statically seen as subtypes of the (opaque) NamedTuple type.
      def castToNamedTuple(expr: scala.quoted.Expr[Any]): UntypedExpr = '{ $expr.asInstanceOf[NT] }.asTerm

      orderedArgs.length match {
        case 0 =>
          // Empty named tuple (e.g. NamedTuple.Empty) -> EmptyTuple. See issue #313.
          castToNamedTuple('{ scala.EmptyTuple })
        case n if n < 23 =>
          // Construct the underlying tuple via its constructor: new TupleN(arg1, arg2, ...)
          val underlyingTupleTpe = instanceTpe.dealias match {
            case AppliedType(_, List(_, valuesTpe)) => valuesTpe
            case _ => hearthAssertionFailed(s"Expected NamedTuple AppliedType, got ${instanceTpe.show}")
          }
          type Underlying
          given scala.quoted.Type[Underlying] = underlyingTupleTpe.asType.asInstanceOf[scala.quoted.Type[Underlying]]
          val tupleCtor = underlyingTupleTpe.typeSymbol.primaryConstructor
          val select = New(TypeTree.of[Underlying]).select(tupleCtor)
          val applied =
            if underlyingTupleTpe.typeArgs.nonEmpty then select.appliedToTypes(underlyingTupleTpe.typeArgs)
            else select
          // Type-ascribe the tuple to the NamedTuple type: (tupleExpr : NamedTupleType)
          Typed(applied.appliedToArgss(List(orderedArgs)), TypeTree.of[NT])
        case _ =>
          // TupleXXL (arity >= 23) has no public N-ary constructor - build the runtime tuple via Tuple.fromArray,
          // boxing each element to Object. See issue #314.
          val boxed = orderedArgs.map(arg => '{ ${ arg.asExpr }.asInstanceOf[Object] })
          castToNamedTuple('{ scala.Tuple.fromArray(scala.Array[Object](${ scala.quoted.Varargs(boxed) }*)) })
      }
    }

    override lazy val name: String = "<init>"
    override def position: Option[Position] = None
    override def annotations: List[UntypedExpr] = Nil
    override def annotationTypes: List[UntypedType] = Nil

    override def isConstructor: Boolean = true

    override def isVal: Boolean = false
    override def isVar: Boolean = false
    override def isLazy: Boolean = false
    override def isDef: Boolean = true
    override def isSynthetic: Boolean = true
    override def isImplicit: Boolean = false

    override def isFinal: Boolean = false
    override def isAbstract: Boolean = false
    override def isOverride: Boolean = false

    override def isPrivate: Boolean = false
    override def isProtected: Boolean = false
    override def privateWithin: Option[String] = None
    override def protectedWithin: Option[String] = None
    override def isAvailable(scope: Accessible): Boolean = true

    override def signatureSegments(instanceTpe: UntypedType): List[String] =
      parameters.map(pl => "(" + pl.map { case (n, _) => s"$n: ?" }.mkString(", ") + ")")
    override def signatureSegments(instanceTpe: UntypedType, hl: hearth.treeprinter.SyntaxHighlight): List[String] =
      parameters.map(pl => "(" + pl.map { case (n, _) => s"${hl.highlightValDef(n)}: ?" }.mkString(", ") + ")")
  }

  /** Extracts field names and types from a NamedTuple type.
    *
    * NamedTuple[("name", "age"), (String, Int)] has:
    *   - type args(0) = ("name", "age") — tuple of string literal types
    *   - type args(1) = (String, Int) — tuple of value types
    *
    * Returns Some((names, types)) if the type is a recognized NamedTuple, None otherwise.
    */
  private def namedTupleComponents(instanceTpe: UntypedType): Option[(List[String], List[TypeRepr])] = {
    def extractTupleElements(tpe: TypeRepr): List[TypeRepr] = tpe.dealias.simplified match {
      case AppliedType(_, args) if tpe.dealias <:< TypeRepr.of[Tuple] =>
        // For Tuple2[A, B], args = List(A, B)
        // For *:[H, T], args = List(H, T) where T is another tuple
        val base = tpe.dealias.simplified
        base match {
          case AppliedType(tycon, List(head, tail)) if tycon.typeSymbol.name == "*:" =>
            head :: extractTupleElements(tail)
          case AppliedType(_, args) =>
            args // For TupleN types, all args are the elements
        }
      case _ => Nil // EmptyTuple or unrecognized
    }

    def extractStringLiterals(tpe: TypeRepr): List[String] =
      extractTupleElements(tpe).collect { case ConstantType(StringConstant(name)) =>
        name
      }

    // Dealias so that alias forms such as `scala.NamedTuple.Empty` (= `NamedTuple[EmptyTuple, EmptyTuple]`) are
    // recognized. The empty named tuple has zero fields, so we must allow `names.length == 0`. See issue #313.
    instanceTpe.dealias match {
      case applied @ AppliedType(_, List(namesTpe, valuesTpe)) if UntypedType.isNamedTuple(applied) =>
        val names = extractStringLiterals(namesTpe)
        val types = extractTupleElements(valuesTpe)
        if names.length == types.length then Some((names, types))
        else None
      case _ => None
    }
  }

  object UntypedMethod extends UntypedMethodModule {

    private def parse(
        isDeclared: Boolean,
        isConstructorArgument: Boolean,
        constructorArgumentIndex: Option[Int],
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
          constructorArgumentIndex = constructorArgumentIndex,
          isCaseField = isCaseField
        )
      )
      else Left(s"Expected method Symbol, got $symbol")
    private def parseOption(
        isDeclared: Boolean,
        isConstructorArgument: Boolean,
        constructorArgumentIndex: Option[Int],
        isCaseField: Boolean,
        module: Option[UntypedExpr]
    )(symbol: Symbol): Option[UntypedMethod] =
      parse(isDeclared, isConstructorArgument, constructorArgumentIndex, isCaseField, module)(symbol).toOption

    private val parseCtorOption =
      parseOption(
        isDeclared = true,
        isConstructorArgument = false,
        constructorArgumentIndex = None,
        isCaseField = false,
        module = None
      )

    override def toTyped[Instance: Type](untyped: UntypedMethod): Method = {
      val instanceTpe = UntypedType.fromTyped[Instance]

      val untypedExpectations = untyped.methodExpectations(instanceTpe)

      // The map covers ALL value clauses, so resolve it once and share it between the per-clause expectations and
      // `totalParameters` below (it used to be recomputed for each of them).
      lazy val sharedTypesByParamName = UntypedParameters.resolveTypesByParamName(instanceTpe, untyped)

      // [hearth#331] A value/implicit clause that FOLLOWS a type-parameter clause references the method's own type
      // parameters (`(implicit ev: Sync[F])`). `up.asTyped[Instance]` cannot resolve those against `Instance` (it
      // conflates method and class type parameters and can even crash), so historically the clause was dropped to
      // `List.empty`. Instead we read the parameter types straight from the member signature (class type parameters
      // already resolved as-seen-from `Instance`, method type parameters left abstract) and substitute the applied
      // type arguments for the method's type parameters — abstract when none are applied yet, concrete after `onTypes`.
      val methodTypeParamSyms: List[Symbol] = untyped.typeParameters.flatten
      // Resolve parameter types with the method's own type parameters instantiated to `typeArgs` — applied arguments
      // become concrete, un-applied ones become the type parameter's own (bare) ref. We `appliedTo` the member
      // `PolyType` rather than `substituteTypes` on its inner `MethodType`, because the parameter types reference the
      // type parameters as PolyType-BOUND refs, which `substituteTypes(symbols, …)` would not touch.
      def resolvedParamTypesByName(typeArgs: UntypedTypeArguments): Map[String, TypeRepr] = {
        def collect(tpe: TypeRepr): Map[String, TypeRepr] = tpe match {
          case mt: MethodType => mt.paramNames.zip(mt.paramTypes).toMap ++ collect(mt.resType)
          case pt: PolyType   => collect(pt.resType)
          case _              => Map.empty
        }
        // When nothing is applied yet, keep the `PolyType` untouched: its BOUND parameter refs print bare (`A`, `F`),
        // matching Scala 2. Only instantiate once there are real arguments (then the applied slots become concrete).
        val applied = safeMemberType(instanceTpe, untyped.symbol) match {
          case pt: PolyType if methodTypeParamSyms.nonEmpty && typeArgs.nonEmpty =>
            pt.appliedTo(
              methodTypeParamSyms.map(tp => typeArgs.get(tp).fold(tp.typeRef: TypeRepr)(_.Underlying.asUntyped))
            )
          case other => other
        }
        collect(applied)
      }
      def resolveTypeParamClauseValues(up: UntypedParameters, typeArgs: UntypedTypeArguments): Parameters = {
        val byName = resolvedParamTypesByName(typeArgs)
        up.map(_.flatMap { case (name, param) =>
          byName.get(name).map { tpe =>
            name -> Parameter(
              asUntyped = param,
              untypedInstanceType = instanceTpe,
              tpe = normalizeRepeatedParamType(tpe).as_??
            )
          }
        })
      }

      def resolveExpectations(typeArgs: UntypedTypeArguments): List[MethodExpectation] = {
        var seenTypeParams = false
        untypedExpectations.map {
          case UntypedMethodExpectation.NeedsInstance   => MethodExpectation.NeedsInstance
          case UntypedMethodExpectation.NeedsTypes(utp) =>
            seenTypeParams = true
            MethodExpectation.NeedsTypes(utp.map(_.map { sym =>
              new TypeParameter(
                asUntyped = sym,
                upperBound = UntypedTypeParameter.upperBound(sym).as_??,
                lowerBound = UntypedTypeParameter.lowerBound(sym).as_??
              )
            }))
          case UntypedMethodExpectation.NeedsValues(up) =>
            if seenTypeParams then MethodExpectation.NeedsValues(resolveTypeParamClauseValues(up, typeArgs))
            else
              MethodExpectation.NeedsValues(UntypedParameters.toTypedWith[Instance](up, () => sharedTypesByParamName))
        }
      }

      val typedExpectations: List[MethodExpectation] = resolveExpectations(Map.empty)

      val hasUnresolvedTypeParams = untyped.hasTypeParameters && !untyped.isConstructor
      val totalParams: Parameters =
        if hasUnresolvedTypeParams then List.empty
        else UntypedParameters.toTypedWith[Instance](untyped.parameters, () => sharedTypesByParamName)

      // Option-ness is decided by cheap flags; the expensive `safeMemberType` resolution inside `Some` is deferred
      // (memoized via the local lazy val) until someone actually asks the chain for `knownReturning`.
      lazy val resolvedReturnType: ?? = {
        def finalResultType(tpe: TypeRepr): TypeRepr = tpe match {
          case mt: MethodType => finalResultType(mt.resType)
          case pt: PolyType   => finalResultType(pt.resType)
          case other          => other
        }
        val memberType = safeMemberType(instanceTpe, untyped.symbol).widenByName
        // E.g. the case-field accessor of a vararg parameter would otherwise return `scala.<repeated>[A]`.
        normalizeRepeatedParamType(finalResultType(memberType)).as_??
      }
      val returnType: Option[() => ??] =
        if hasUnresolvedTypeParams then None
        else if untyped.symbol.isNoSymbol then Some(() => instanceTpe.as_??)
        else if untyped.isConstructor then Some(() => instanceTpe.as_??)
        else Some(() => resolvedReturnType)

      val buildExpr: (Option[UntypedExpr], UntypedTypeArguments, UntypedArguments) => Either[String, UntypedExpr] =
        (instance, typeArgs, args) => Right(untyped.unsafeApplyWithTypes(instanceTpe)(typeArgs, instance, args))

      Method.buildChain(
        asUntyped = untyped,
        untypedInstanceType = instanceTpe,
        instanceEvidence = Type[Instance].as_??,
        expectations = typedExpectations,
        totalParameters = totalParams,
        returnType = returnType,
        buildExpr = buildExpr,
        pathDepResolvers = Map.empty,
        resolveExpectationsForTypeArgs = resolveExpectations
      )
    }

    override def primaryConstructor(instanceTpe: UntypedType): Option[UntypedMethod] =
      namedTupleComponents(instanceTpe) match {
        case Some((names, types)) => Some(new SyntheticNamedTupleConstructor(names, types))
        case None                 =>
          // Dealias so that an `export`-created alias (`export Inner.Foo`) resolves to the underlying class symbol -
          // otherwise `typeSymbol` is the alias and reports no constructors. Scala 2's `typeSymbol` auto-dealiases.
          // See issue #315.
          Option(instanceTpe.dealias.typeSymbol.primaryConstructor)
            .filterNot(_.isNoSymbol)
            .flatMap(
              UntypedMethod
                .parseOption(
                  isDeclared = true,
                  isConstructorArgument = false,
                  constructorArgumentIndex = None,
                  isCaseField = false,
                  module = None
                )
            )
      }
    override def constructors(instanceTpe: UntypedType): List[UntypedMethod] =
      namedTupleComponents(instanceTpe) match {
        case Some((names, types)) => List(new SyntheticNamedTupleConstructor(names, types))
        case None                 =>
          instanceTpe.dealias.typeSymbol.declarations // dealias for `export`-created aliases, see issue #315
            .filterNot(_.isNoSymbol)
            .filter(_.isClassConstructor)
            .flatMap(
              UntypedMethod
                .parseOption(
                  isDeclared = true,
                  isConstructorArgument = false,
                  constructorArgumentIndex = None,
                  isCaseField = false,
                  module = None
                )
            )
      }
    override def methods(instanceTpe: UntypedType): List[UntypedMethod] =
      sortMethods(unsortedMethods(instanceTpe))

    override def unsortedMethods(instanceTpe: UntypedType): List[UntypedMethod] = {
      val symbol = instanceTpe.typeSymbol
      // `fieldMembers` only returns the type's OWN fields, so `val` fields inherited from parent CLASSES (e.g. the
      // constructor vals of an abstract parent) are neither methods nor own fields and would vanish entirely. Walk the
      // base classes and pick up their public, non-synthetic fields, deduplicating by name (closest class wins, since
      // `baseClasses` is ordered subclass-first) and never shadowing an own member. Scala 2's `.members` already sees
      // them. See issue #327. `UntypedType.baseClasses` is used explicitly to avoid the extension-shadowing footgun.
      // [hearth#327] The genuine PUBLIC accessors inherited from parent CLASSES (candidates, closest class first).
      val inheritedPublicFieldCandidates = UntypedType
        .baseClasses(instanceTpe)
        .iterator
        .flatMap(_.typeSymbol.fieldMembers)
        .filter(f => !f.isNoSymbol && !f.flags.is(Flags.Private) && !f.flags.is(Flags.Synthetic))
        .toList
      val inheritedPublicFieldNames = inheritedPublicFieldCandidates.iterator.map(_.name).toSet
      // [hearth#327] When a subclass re-takes a constructor param and only passes it to `super` (e.g. `class
      // IdStatusEntity(id: Long, ...) extends AbstractIdStatusEntity(id, ...)`), Scala 3 keeps it as a `private[this]`
      // `ParamAccessor` field. Such a field would `seenNames`-shadow the genuine PUBLIC inherited accessor of the same
      // name, leaving the listing with an unavailable, "declared" member (isAvailable=false, isInherited=false). Drop it
      // ONLY in that shadowing case — a private param-accessor that is NOT hiding an inherited accessor (an ordinary
      // constructor argument) must still be listed. Scala 2 never retains these, so this also aligns the platforms.
      def shadowsInheritedAccessor(f: Symbol): Boolean =
        f.flags.is(Flags.ParamAccessor) && f.flags.is(Flags.Private) && inheritedPublicFieldNames(f.name)
      val ownMembers = symbol.methodMembers ++ symbol.fieldMembers.filterNot(shadowsInheritedAccessor)
      val seenNames = scala.collection.mutable.Set.from(ownMembers.iterator.map(_.name))
      val inheritedFields = inheritedPublicFieldCandidates.filter(f => seenNames.add(f.name))
      // Defined in the type or its parent, or synthetic
      val classMembers = ownMembers ++ inheritedFields
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
          val moduleBySymbol = companionMembers.iterator.map(_ -> companionRef).toMap[Symbol, UntypedExpr]
          (allMembers, allDeclared, moduleBySymbol)
        }
        .getOrElse((classMembers, classDeclared, Map.empty[Symbol, UntypedExpr]))

      val constructorArguments: Map[String, Int] = (for {
        primaryConstructor <- Option(symbol.primaryConstructor).toList
        if !primaryConstructor.isNoSymbol
        (argument, idx) <- primaryConstructor.paramSymss.filterNot(_.exists(_.isType)).flatten.zipWithIndex
      } yield argument.name -> idx).toMap
      val caseFields = (for {
        field <- symbol.caseFields
        if !field.isNoSymbol
      } yield field.name).toSet

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
          val isSetter = s.name.endsWith("_=")
          val fieldName = if isSetter then s.name.dropRight(2) else s.name
          val module = moduleBySymbol.get(s)
          UntypedMethod.parseOption(
            isDeclared = declared(s) && !methodsConsideredSynthetic(s),
            isConstructorArgument = constructorArguments.contains(fieldName) && !isSetter,
            constructorArgumentIndex = if isSetter then None else constructorArguments.get(fieldName),
            isCaseField = caseFields(fieldName) && !isSetter,
            module = module
          )(s)
        }
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
            // For `apply` on case class companions, also try constructor names since they may share defaults
            val names =
              if param.method.name == "apply" then possibleConstructorNames
              else List(param.method.name)
            val invocation = Invocation.OnModule(module)
            // Use companionObject for proper type resolution - module.as_?? may fail for synthetic companions
            val decls = instanceTpe.companionObject
              .map(_._1.typeSymbol)
              .getOrElse(module.as_??.Underlying.asUntyped.typeSymbol)
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
          constructorArgumentIndex = None,
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
