package hearth
package untyped

import scala.collection.immutable.ListMap

trait UntypedMethodsScala2 extends UntypedMethods { this: MacroCommonsScala2 =>

  import c.universe.*

  import UntypedType.platformSpecific.{positionOf, symbolAvailable, symbolName}

  final class UntypedParameter private (val method: UntypedMethod, val symbol: TermSymbol, val index: Int)
      extends UntypedParameterMethods {

    override def name: String = symbolName(symbol)
    override def position: Option[Position] = positionOf(symbol)

    override def annotations: List[UntypedExpr] =
      symbol.annotations.map(ann => c.untypecheck(ann.tree))
    override def annotationTypes: List[UntypedType] =
      symbol.annotations.map(_.tree.tpe)

    override def isByName: Boolean = symbol.isByNameParam
    override def isVararg: Boolean = {
      val tpeSymbol = symbol.typeSignature.typeSymbol
      tpeSymbol == definitions.RepeatedParamClass || tpeSymbol == definitions.JavaRepeatedParamClass
    }
    override def isImplicit: Boolean = symbol.isImplicit
    override def hasDefault: Boolean = symbol.asTerm.isParamWithDefault

    // A by-name parameter's type is the applied wrapper `<byname>[A]`, so the underlying `A` is its sole type argument.
    override def byNameUnderlying: Option[c.universe.Type] =
      if (symbol.isByNameParam) symbol.typeSignature.typeArgs.headOption
      else None
  }

  object UntypedParameter extends UntypedParameterModule {

    def parse(method: UntypedMethod, symbol: Symbol, index: Int): Either[String, UntypedParameter] =
      if (symbol.isTerm) Right(new UntypedParameter(method, symbol.asTerm, index))
      else Left(s"Expected param Symbol, got $symbol")
  }

  /** Normalizes the raw repeated-parameter marker type `A*` (`scala.<repeated>[A]` / `scala.<repeated...>[A]`) to
    * `scala.collection.immutable.Seq[A]`, so that [[Parameter.tpe]] is a real, usable type, consistent between Scala 2
    * and Scala 3. Non-repeated types are returned unchanged.
    */
  private def normalizeRepeatedParamType(tpe: c.universe.Type): c.universe.Type = {
    val tpeSymbol = tpe.typeSymbol
    if (
      (tpeSymbol == definitions.RepeatedParamClass || tpeSymbol == definitions.JavaRepeatedParamClass) &&
      tpe.typeArgs.sizeIs == 1
    )
      appliedType(c.typeOf[scala.collection.immutable.Seq[Any]].typeConstructor, tpe.typeArgs)
    else tpe
  }

  override protected def adaptVarargArgument(expr: UntypedExpr): UntypedExpr = q"$expr: _*"

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
    ): Map[String, c.universe.Type] = {
      val rawSig = method.symbol.asMethod.typeSignatureIn(instanceTpe)
      val sig = rawSig match {
        case ExistentialType(_, underlying) => underlying
        case other                          => other
      }
      val firstListSize = sig.paramLists.headOption.map(_.size).getOrElse(0)
      sig.paramLists.flatten.zipWithIndex.map { case (param, idx) =>
        val rawType = param.typeSignatureIn(instanceTpe)
        val resolvedType =
          if (idx < firstListSize) rawType
          else {
            val asMethodType = rawType.asInstanceOf[scala.reflect.internal.Types#Type]
            asMethodType match {
              case imt: scala.reflect.internal.Types#MethodType =>
                val paramIdx = imt.params.indexWhere(_.decodedName.toString.trim == symbolName(param))
                if (paramIdx >= 0) {
                  val internalParams = imt.asInstanceOf[scala.reflect.internal.SymbolTable#MethodType].paramTypes
                  internalParams(paramIdx).asInstanceOf[c.universe.Type]
                } else rawType
              case _ => rawType
            }
          }
        symbolName(param) -> resolvedType
      }.toMap
    }

    private[hearth] def toTypedWith[Instance: Type](
        untyped: UntypedParameters,
        typesByParamName0: () => Map[String, c.universe.Type]
    ): Parameters = {
      val instanceTpe = UntypedType.fromTyped[Instance]
      lazy val typesByParamName = typesByParamName0()

      untyped.map { params =>
        params.flatMap { case (paramName, untyped) =>
          typesByParamName.get(paramName).map { tpe =>
            val param = new Parameter(
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
    override def name(param: UntypedTypeParameter): String = symbolName(param)
    override def upperBound(param: UntypedTypeParameter): UntypedType = param.asType.typeSignature match {
      case TypeBounds(_, hi) => hi
      case other             => other
    }
    override def lowerBound(param: UntypedTypeParameter): UntypedType = param.asType.typeSignature match {
      case TypeBounds(lo, _) => lo
      case _                 => c.universe.definitions.NothingTpe
    }

    private def typeParamAwarePrint(tpe: c.universe.Type): String = {
      def containsTP(t: c.universe.Type): Boolean = t.dealias match {
        case TypeRef(_, sym, _) if sym.isType && (sym.asType.isParameter || sym.owner.isMethod) =>
          true
        case TypeRef(_, _, args) => args.exists(containsTP)
        case _                   => false
      }
      def printType(t: c.universe.Type): String = t.dealias match {
        case TypeRef(_, sym, Nil) if sym.isType && (sym.asType.isParameter || sym.owner.isMethod) =>
          symbolName(sym)
        case TypeRef(_, sym, args) if args.exists(containsTP) =>
          val base =
            if (sym.isType && (sym.asType.isParameter || sym.owner.isMethod))
              symbolName(sym)
            else sym.fullName
          s"$base[${args.map(printType).mkString(", ")}]"
        case _ => scala.util.Try(UntypedType.plainPrint(t)).getOrElse(t.toString)
      }
      printType(tpe)
    }

    override def upperBoundPrint(param: UntypedTypeParameter): String = typeParamAwarePrint(upperBound(param))
    override def lowerBoundPrint(param: UntypedTypeParameter): String = typeParamAwarePrint(lowerBound(param))
  }

  final class UntypedMethod private (
      val symbol: TermSymbol,
      val invocation: Invocation,
      val isDeclared: Boolean,
      val isConstructorArgument: Boolean,
      val constructorArgumentIndex: Option[Int],
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

    override lazy val hasTypeParameters: Boolean = typeParameters.flatten.nonEmpty

    override lazy val typeParameters: UntypedTypeParameters =
      if (symbol.isMethod) List(symbol.asMethod.typeParams) else Nil

    override def unsafeApplyWithTypes(instanceTpe: UntypedType)(
        typeArgs: UntypedTypeArguments,
        instance: Option[UntypedExpr],
        arguments: UntypedArguments
    ): UntypedExpr = {
      lazy val adaptedArguments = arguments.adaptToParams(instanceTpe, instance, this)
      lazy val typeArgTypes: List[c.universe.Type] =
        if (typeArgs.isEmpty) Nil
        else typeParameters.flatten.map(tp => typeArgs.get(tp).fold(tp.asType.toType)(_.Underlying.asUntyped))
      invocation match {
        case Invocation.Constructor =>
          if (typeArgTypes.nonEmpty) q"new $instanceTpe[..$typeArgTypes](...$adaptedArguments)"
          else q"new $instanceTpe(...$adaptedArguments)"
        case Invocation.OnInstance =>
          instance match {
            case None =>
              hearthAssertionFailed(s"Expected an instance for method $name that is called on an instance")
            case Some(instance) =>
              if (typeArgTypes.nonEmpty) q"$instance.$symbol[..$typeArgTypes](...$adaptedArguments)"
              else q"$instance.$symbol(...$adaptedArguments)"
          }
        case Invocation.OnModule(module) =>
          if (typeArgTypes.nonEmpty) q"$module.$symbol[..$typeArgTypes](...$adaptedArguments)"
          else q"$module.$symbol(...$adaptedArguments)"
      }
    }

    override def methodExpectations(instanceTpe: UntypedType): List[UntypedMethodExpectation] = {
      val steps = List.newBuilder[UntypedMethodExpectation]
      if (invocation == Invocation.OnInstance) steps += UntypedMethodExpectation.NeedsInstance
      // Constructor type params are class-level — they're already resolved by the instance type
      if (!isConstructor && typeParameters.flatten.nonEmpty)
        steps += UntypedMethodExpectation.NeedsTypes(typeParameters)
      val valueParamLists = parameters
      if (valueParamLists.nonEmpty) {
        val groups = groupParamListsByPathDependency(instanceTpe, valueParamLists)
        groups.foreach(group => steps += UntypedMethodExpectation.NeedsValues(group))
      }
      steps.result()
    }

    private def groupParamListsByPathDependency(
        instanceTpe: UntypedType,
        paramLists: UntypedParameters
    ): List[UntypedParameters] = {
      if (paramLists.sizeIs <= 1) return List(paramLists)

      val rawParamLists =
        if (symbol.isMethod) symbol.asMethod.typeSignatureIn(instanceTpe).paramLists
        else Nil

      if (rawParamLists.isEmpty) return List(paramLists)

      val allPrecedingParamSymbols = scala.collection.mutable.Set.empty[Symbol]
      val result = List.newBuilder[UntypedParameters]
      var currentGroup = List.newBuilder[ListMap[String, UntypedParameter]]

      paramLists.zip(rawParamLists).zipWithIndex.foreach { case ((paramList, rawList), idx) =>
        val hasDependency = idx > 0 && rawList.exists { param =>
          containsTermRef(param.typeSignature, allPrecedingParamSymbols.toSet)
        }
        if (hasDependency) {
          val built = currentGroup.result()
          if (built.nonEmpty) result += built
          currentGroup = List.newBuilder[ListMap[String, UntypedParameter]]
        }
        currentGroup += paramList
        allPrecedingParamSymbols ++= rawList
      }
      val lastGroup = currentGroup.result()
      if (lastGroup.nonEmpty) result += lastGroup
      result.result()
    }

    private def containsTermRef(tpe: c.universe.Type, precedingParams: Set[Symbol]): Boolean = tpe match {
      case SingleType(_, sym) if precedingParams.contains(sym) => true
      case TypeRef(pre, _, _)                                  => containsTermRef(pre, precedingParams)
      case _                                                   =>
        tpe.typeArgs.exists(containsTermRef(_, precedingParams))
    }

    override lazy val parameters: UntypedParameters = {
      val paramss =
        try if (symbol.isMethod) symbol.asMethod.paramLists else Nil
        catch {
          // [Chimney scalalandio/chimney#899] Forcing a member's parameter lists completes its full signature. For a
          // value/getter whose type is still being inferred (`val foo = someMacro[A]` with no explicit type), completing
          // that signature re-enters the member's own still-running completer and scala-reflect throws a CyclicReference.
          // A getter provably takes no value parameters, so we can safely report `Nil` (exactly what a completed
          // getter would return) WITHOUT forcing the cyclic signature - reading `isGetter` does not complete it. The
          // member stays listable; its still-unknown return type surfaces as `knownReturning == None` (see
          // `Method.knownReturning`) instead of throwing. Non-getter members genuinely cannot be shaped without the
          // signature, so their cycle propagates (the compiler then reports "recursive value ... needs type").
          case e: Throwable
              if e.getClass.getName.endsWith("CyclicReference") && symbol.isMethod && symbol.asMethod.isGetter =>
            Nil
        }
      val indices = paramss.flatten.zipWithIndex.toMap
      paramss
        .map(inner =>
          ListMap.from(inner.map { param =>
            symbolName(param.asTerm) -> UntypedParameter
              .parse(this, param.asTerm, indices(param))
              .fold(hearthAssertionFailed(_), identity)
          })
        )
    }

    override lazy val name: String = symbolName(symbol)
    override def position: Option[Position] = positionOf(symbol)

    override def annotations: List[UntypedExpr] =
      symbol.annotations.map(ann => c.untypecheck(ann.tree))
    override def annotationTypes: List[UntypedType] =
      symbol.annotations.map(_.tree.tpe)

    override def isConstructor: Boolean = symbol.isConstructor

    // A stable deferred accessor (e.g. the `value` member of a structural refinement `{ val value: String }`) is a
    // method symbol with no accessed field and `symbol.isVal == false`, yet scalac's `MethodSymbol.isStable` is the
    // authoritative "this reads like a val" signal - without it such accessors are misclassified as plain methods.
    // See issue #326.
    override def isVal: Boolean =
      symbol.isVal || accessedOf(symbol).exists(_.isVal) || isLazy ||
        (symbol.isMethod && symbol.asMethod.isStable && !symbol.isConstructor)
    override def isVar: Boolean = symbol.isVar || accessedOf(symbol).exists(_.isVar)
    override def isLazy: Boolean = symbol.isLazy || accessedOf(symbol).exists(_.isLazy)
    override def isDef: Boolean = !isVal && (!isVar || name.endsWith("_=")) // var's setter should both var AND def
    override def isImplicit: Boolean = symbol.isImplicit
    override def isSynthetic: Boolean = symbol.isSynthetic || UntypedMethod.methodsConsideredSynthetic(symbol)

    override def isFinal: Boolean = symbol.isFinal
    override def isAbstract: Boolean = (symbol: Symbol).isAbstract
    override def isOverride: Boolean = if (symbol.isMethod) symbol.asMethod.overrides.nonEmpty else false

    override def isPrivate: Boolean =
      (symbol.isPrivate || symbol.isPrivateThis) &&
        Option(symbol.privateWithin).forall(_ == NoSymbol)
    override def isProtected: Boolean =
      (symbol.isProtected || symbol.isProtectedThis) &&
        Option(symbol.privateWithin).forall(_ == NoSymbol)
    override def privateWithin: Option[String] = {
      val pw = Option(symbol.privateWithin).filterNot(_ == NoSymbol)
      pw.filter(_ => symbol.isPrivate && !symbol.isProtected).map(_.name.toString)
    }
    override def protectedWithin: Option[String] = {
      val pw = Option(symbol.privateWithin).filterNot(_ == NoSymbol)
      pw.filter(_ => symbol.isProtected).map(_.name.toString)
    }
    override def isAvailable(scope: Accessible): Boolean = symbolAvailable(symbol, scope)

    private def paramTypePrintsImpl(
        instanceTpe: UntypedType,
        hl: hearth.treeprinter.SyntaxHighlight
    ): (List[List[(String, String)]], String) = {
      val typeParamSymbols = typeParameters.flatten.toSet
      def safePrint(tpe: c.universe.Type): String = {
        def isTP(sym: Symbol): Boolean =
          typeParamSymbols.contains(sym) || (sym.isType && (sym.asType.isParameter || sym.owner.isMethod))
        def containsTypeParam(t: c.universe.Type): Boolean = t.dealias match {
          case TypeRef(_, sym, _) if isTP(sym) => true
          case TypeRef(_, _, args)             => args.exists(containsTypeParam)
          case _                               => false
        }
        def printType(t: c.universe.Type): String = t.dealias match {
          case TypeRef(_, sym, List(arg))
              if sym == definitions.RepeatedParamClass || sym == definitions.JavaRepeatedParamClass =>
            s"${printType(arg)}*"
          case TypeRef(_, sym, Nil) if isTP(sym)                                                    => symbolName(sym)
          case TypeRef(_, sym, Nil) if sym.isType && sym.owner.isClass && !sym.owner.isPackageClass =>
            s"${sym.owner.fullName}#${symbolName(sym)}"
          case TypeRef(_, sym, args) if isTP(sym) || args.exists(containsTypeParam) =>
            val base =
              if (isTP(sym)) symbolName(sym)
              else sym.fullName
            s"$base[${args.map(printType).mkString(", ")}]"
          case ExistentialType(_, underlying) => printType(underlying)
          case _                              => scala.util.Try(UntypedType.plainPrint(t)).getOrElse(t.toString)
        }
        val raw = printType(tpe)
        if (hl.TypeDefColor.isEmpty) raw else hl.highlightTypeDef(raw)
      }

      val rawSig = symbol.asMethod.typeSignatureIn(instanceTpe)
      val sig = rawSig match {
        case ExistentialType(_, underlying) => underlying
        case other                          => other
      }
      @scala.annotation.tailrec
      def collect(
          tpe: c.universe.Type,
          acc: List[List[(String, String)]]
      ): (List[List[(String, String)]], String) = tpe match {
        case NullaryMethodType(res) => collect(res, acc)
        case PolyType(_, res)       => collect(res, acc)
        case mt: MethodType         =>
          val internalMt = mt.asInstanceOf[scala.reflect.internal.SymbolTable#MethodType]
          val paramPairs = mt.params
            .map(symbolName(_))
            .zip(
              internalMt.paramTypes.asInstanceOf[List[c.universe.Type]].map(safePrint)
            )
          collect(mt.resultType, acc :+ paramPairs)
        case other => (acc, safePrint(other))
      }
      collect(sig, Nil)
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
      val (paramListsWithTypes, _) = paramTypePrintsImpl(instanceTpe, hl)

      def renderTypeParam(tp: UntypedTypeParameter): String = {
        val tpName = UntypedTypeParameter.name(tp)
        val coloredName = if (hl.TypeDefColor.isEmpty) tpName else hl.highlightTypeDef(tpName)
        val upper = UntypedTypeParameter.upperBoundPrint(tp)
        val lower = UntypedTypeParameter.lowerBoundPrint(tp)
        val isTopBound =
          upper == "scala.Any" || upper.startsWith("scala.Any[") || upper.contains("@") || upper.startsWith("[")
        val isBottomBound = lower == "scala.Nothing" || lower.contains("@") || lower.startsWith("[")
        val coloredUpper = if (hl.TypeDefColor.isEmpty) upper else hl.highlightTypeDef(upper)
        val coloredLower = if (hl.TypeDefColor.isEmpty) lower else hl.highlightTypeDef(lower)
        if (!isTopBound && !isBottomBound) s"$coloredName <: $coloredUpper >: $coloredLower"
        else if (!isTopBound) s"$coloredName <: $coloredUpper"
        else if (!isBottomBound) s"$coloredName >: $coloredLower"
        else coloredName
      }

      val segments = List.newBuilder[String]
      // Scala 2 doesn't support clause interleaving — type params first, then value params
      if (!isConstructor && hasTypeParameters) {
        segments += typeParameters.map(_.map(renderTypeParam).mkString(", ")).mkString("[", "][", "]")
      }
      paramListsWithTypes.zip(parameters).foreach { case (typedList, untypedList) =>
        segments += "(" + typedList
          .map { case (pName, pType) =>
            val default = untypedList.get(pName).filter(_.hasDefault).map(_ => " = <default>").getOrElse("")
            s"${hl.highlightValDef(pName)}: $pType$default"
          }
          .mkString(", ") + ")"
      }
      segments.result()
    }

    override def signatureSegments(instanceTpe: UntypedType): List[String] =
      signatureSegmentsImpl(instanceTpe, hearth.treeprinter.SyntaxHighlight.plain)

    override def signatureSegments(instanceTpe: UntypedType, hl: hearth.treeprinter.SyntaxHighlight): List[String] =
      signatureSegmentsImpl(instanceTpe, hl)

    // -------------------------------------------- Special cases handling --------------------------------------------
    // So, the symbol of val/var is no just one symbol - there is a symbol for var/var `fieldName` as a `TermSymbol`,
    // representing the field, but there is also a separate symbol for `def fieldName` as a getter `MethodSymbol`,
    // and - if we are talking about var - a separate symbol for `def fieldName_=` as a setter `MethodSymbol`.
    //
    // Oh. and if we are talking about class constructor argument then I guess it's another one?
    //
    // These symbols are not equal, so if we want to check for .isVal, .isVar, .isLazy, etc, we need to check both the
    // symbol itself and the accessed symbol. FML

    private def accessedOf(symbol: TermSymbol): Option[TermSymbol] =
      scala.util.Try(symbol.accessed).toOption.filterNot(_ == NoSymbol).collectFirst {
        case s if s.isTerm => s.asTerm
      }
    private def getterOf(symbol: TermSymbol): Option[MethodSymbol] =
      scala.util.Try(symbol.getter).toOption.filterNot(_ == NoSymbol).collectFirst {
        case s if s.isMethod => s.asMethod
      }
    private def setterOf(symbol: TermSymbol): Option[MethodSymbol] =
      scala.util.Try(symbol.setter).toOption.filterNot(_ == NoSymbol).collectFirst {
        case s if s.isMethod => s.asMethod
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
      if (symbol.isTerm)
        Right(
          new UntypedMethod(
            symbol = symbol.asTerm,
            invocation =
              if (symbol.isConstructor) Invocation.Constructor
              else module.map(Invocation.OnModule).getOrElse(Invocation.OnInstance),
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
      // parameters (`(implicit ev: Sync[F])`); resolving it against `Instance` conflates method and class type
      // parameters, so it used to be dropped to `List.empty`. Instead read the parameter types straight from the
      // member signature (class type parameters resolved as-seen-from `Instance`, method type parameters left
      // abstract) and substitute the applied type arguments — abstract before `onTypes`, concrete after.
      val methodTypeParamSyms: List[Symbol] = untyped.typeParameters.flatten
      lazy val rawParamTypesByName: Map[String, c.universe.Type] = {
        def collect(tpe: c.universe.Type): Map[String, c.universe.Type] = tpe match {
          case MethodType(params, res) => params.map(p => symbolName(p) -> p.typeSignature).toMap ++ collect(res)
          case NullaryMethodType(res)  => collect(res)
          case PolyType(_, res)        => collect(res)
          case _                       => Map.empty
        }
        collect(untyped.symbol.typeSignatureIn(instanceTpe))
      }
      def resolveTypeParamClauseValues(up: UntypedParameters, typeArgs: UntypedTypeArguments): Parameters = {
        // Unapplied method type parameters are kept abstract as a BARE `NoPrefix` ref: `internal.typeRef(NoPrefix, ...)`
        // both avoids eta-applying a higher-kinded param (`tp.asType.toType` would turn `F` into `F[_$1]`) and prints
        // without an owner prefix (`A`, not `Owner.A`), matching Scala 3's rendering.
        val toReprs = methodTypeParamSyms.map(tp =>
          typeArgs.get(tp).fold(internal.typeRef(NoPrefix, tp, Nil))(_.Underlying.asUntyped)
        )
        up.map(_.flatMap { case (name, param) =>
          rawParamTypesByName.get(name).map { raw =>
            val substituted =
              if (methodTypeParamSyms.isEmpty) raw else raw.substituteTypes(methodTypeParamSyms, toReprs)
            name -> new Parameter(
              asUntyped = param,
              untypedInstanceType = instanceTpe,
              tpe = normalizeRepeatedParamType(substituted).as_??
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
            if (seenTypeParams) MethodExpectation.NeedsValues(resolveTypeParamClauseValues(up, typeArgs))
            else
              MethodExpectation.NeedsValues(UntypedParameters.toTypedWith[Instance](up, () => sharedTypesByParamName))
        }
      }
      val typedExpectations: List[MethodExpectation] = resolveExpectations(Map.empty)

      val hasUnresolvedTypeParams = untyped.hasTypeParameters && !untyped.isConstructor
      val totalParams: Parameters =
        if (hasUnresolvedTypeParams) List.empty
        else UntypedParameters.toTypedWith[Instance](untyped.parameters, () => sharedTypesByParamName)

      // Option-ness is decided by cheap flags; the expensive `typeSignatureIn` resolution inside `Some` is deferred
      // (memoized via the local lazy val) until someone actually asks the chain for `knownReturning`.
      lazy val resolvedReturnType: ?? = {
        val rawSig = untyped.symbol.typeSignatureIn(instanceTpe)
        val sig = rawSig match {
          case ExistentialType(_, underlying) => underlying
          case other                          => other
        }
        // `typeSignatureIn` (as-seen-from applied to the whole NullaryMethodType) DROPS type-position annotations
        // (`AnnotatedType`, e.g. `String @Ann` on a case-field getter) from the result on Scala 2.13 - see
        // [hearth#348], where they survive the constructor-parameter path but not this getter/`knownReturning` one.
        // The raw `typeSignature` keeps them, so recover the annotations from ITS result type and re-apply
        // `asSeenFrom` to the underlying (preserving prefix/type-param substitution); non-annotated results keep the
        // instantiated signature's result unchanged.
        val resultType = untyped.symbol.typeSignature.finalResultType match {
          case AnnotatedType(annots, underlying) =>
            internal.annotatedType(annots, underlying.asSeenFrom(instanceTpe, untyped.symbol.owner))
          case _ => sig.finalResultType
        }
        // E.g. the case-field accessor of a vararg parameter would otherwise return `scala.<repeated>[A]`.
        normalizeRepeatedParamType(resultType.dealias).as_??
      }
      val returnType: Option[() => ??] =
        if (hasUnresolvedTypeParams) None
        else if (untyped.isConstructor) Some(() => instanceTpe.as_??)
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
      Option(instanceTpe.typeSymbol)
        .filter(_.isClass)
        .map(_.asClass.primaryConstructor)
        .filter(_.isConstructor)
        .flatMap(parseCtorOption)
    override def constructors(instanceTpe: UntypedType): List[UntypedMethod] =
      instanceTpe.decls.filter(_.isConstructor).flatMap(parseCtorOption).toList
    override def methods(instanceTpe: UntypedType): List[UntypedMethod] =
      sortMethods(unsortedMethods(instanceTpe))

    override def unsortedMethods(instanceTpe: UntypedType): List[UntypedMethod] = {
      // Defined in the type or its parent, or synthetic
      val classMembers = instanceTpe.members
      // Defined exatcly in the type
      val classDeclared = instanceTpe.decls.toSet

      val (members, declared, moduleBySymbol) = instanceTpe.companionObject
        .map { case (companionTpe, companionRef) =>
          // Defined in the companion object or its parent, or synthetic
          val companionMembers = companionTpe.members.filterNot(methodsSkippedInCompanion)
          // Defined exatcly in the companion object
          val companionDeclared = companionTpe.decls.toSet.filterNot(methodsSkippedInCompanion)

          val allMembers = classMembers ++ companionMembers
          val allDeclared = classDeclared ++ companionDeclared
          val moduleBySymbol = companionMembers.iterator.map(_ -> companionRef).toMap[Symbol, UntypedExpr]
          (allMembers, allDeclared, moduleBySymbol)
        }
        .getOrElse((classMembers, classDeclared, Map.empty[Symbol, UntypedExpr]))

      val constructorArguments: Map[String, Int] = (for {
        symbol <- List(instanceTpe.typeSymbol)
        if symbol.isClass
        primaryConstructor = symbol.asClass.primaryConstructor
        if primaryConstructor.isConstructor
        (argument, idx) <- primaryConstructor.asMethod.paramLists.flatten.zipWithIndex
      } yield symbolName(argument) -> idx).toMap

      members
        .filterNot(_.isConstructor) // Constructors are handled by `primaryConstructor` and `constructors`
        .filterNot { s =>
          val name = symbolName(s)
          // val/vars create both term and method symbols - one of them is redundant, but we have to allow terms to not lose ctor arguments.
          // We can recognize extra term symbols by checking if the name ends with " " (for term) or not (for method).
          name.endsWith(" ") ||
          // Default parameters are methods, but we don't want them
          name.contains("$default$") ||
          // Class static initializer is a method, but we don't want it
          name == "<clinit>"
        }
        .flatMap { s =>
          val fieldNames = Set(symbolName(s), symbolName(s) + "_=")
          val module = moduleBySymbol.get(s)
          parseOption(
            isDeclared = declared(s) && !methodsConsideredSynthetic(s),
            isConstructorArgument = (constructorArguments.keySet & fieldNames).nonEmpty,
            constructorArgumentIndex = fieldNames.flatMap(constructorArguments.get).headOption,
            isCaseField = s.isMethod && s.asMethod.isCaseAccessor,
            module = module
          )(s)
        }
        .toList
    }

    override def defaultValue(instanceTpe: UntypedType)(param: UntypedParameter): Option[UntypedMethod] = if (
      param.hasDefault
    )
      Some {
        val (names, invocation, decls) = param.method.invocation match {
          case Invocation.Constructor =>
            val (companionTpe, companionRef) = instanceTpe.companionObject.getOrElse {
              // $COVERAGE-OFF$ Should never happen unless someone mess around with type-level representation
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
            // For `apply` on case class companions, also try constructor names since they may share defaults
            val names =
              if (param.method.name == "apply") possibleConstructorNames
              else List(param.method.name)
            val invocation = Invocation.OnModule(module)
            // Use companionObject for proper type resolution - module.as_?? may fail for synthetic companions
            val decls = instanceTpe.companionObject
              .map(_._1.decls)
              .getOrElse(module.as_??.Underlying.tpe.decls)
            (names, invocation, decls)
        }

        val possibleDefaultNames = names.map(defaultValueMethodName(_, param.index + 1))
        val defaultMethod = decls
          .to(List)
          .collectFirst {
            case method if possibleDefaultNames.contains(method.name.decodedName.toString) => method.asMethod
          }
          .getOrElse {
            // $COVERAGE-OFF$ Should never happen unless someone mess around with type-level representation
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
          constructorArgumentIndex = None,
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

    // Both filter sets below derive from java.lang.Object's members; the walk is done once (lazily - module init
    // runs on every macro expansion) and shared, instead of once per set.
    private lazy val objectMembers = c.weakTypeOf[java.lang.Object].members.toList

    // For these symbol.isSynthetic flag is false, but we want to consider them synthetic.
    private lazy val methodsConsideredSynthetic = {
      val names = Set("asInstanceOf", "isInstanceOf", "getClass", "synchronized", "==", "!=", "eq", "ne", "##")
      objectMembers.filter(symbol => names(symbolName(symbol))).toSet
    }

    // We do not want to include methods and fields from java.lang.Object in the companion object.
    // Because the companion class has them and we don't want to mix them when listing methods for companion class.
    private lazy val methodsSkippedInCompanion: Set[Symbol] =
      objectMembers.toSet
  }
}
