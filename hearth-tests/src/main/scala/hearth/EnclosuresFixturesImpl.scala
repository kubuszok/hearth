package hearth

import hearth.data.Data

trait EnclosuresFixturesImpl { this: MacroCommons =>

  /** Package enclosures are filtered out of the rendered chain because their names/segmentation differ between Scala 2
    * (one `Enclosure.Package` per package segment) and Scala 3 (a single package def), and the test focuses on the
    * method/class/object structure which is identical across platforms.
    */
  def testEnclosingScope: Expr[Data] = {
    val rendered = enclosingScope.toVector
      .filterNot(_.kind == "package")
      .map(enc => Data.map("kind" -> Data(enc.kind), "name" -> Data(enc.name)))
    Expr(Data.list(rendered*))
  }

  /** Render just the kinds present in the chain (including packages), as a sanity check that we always reach a package
    * at the end.
    */
  def testEnclosingScopeReachesPackage: Expr[Data] =
    Expr(Data(enclosingScope.toVector.lastOption.exists(_.kind == "package")))

  def testCallEnclosingHelper: Expr[Int] = {
    val receiverAndMembers: Option[(Expr_??, List[Method])] = enclosingScope.iterator.collectFirst {
      case enc: Enclosure.Class if enc.thisRef.isDefined => enc.thisRef.get -> enc.members
      case enc: Enclosure.Object                         => enc.thisRef -> enc.members
    }

    val call: Option[Expr[Int]] = receiverAndMembers.flatMap { case (receiver, members) =>
      members.collectFirst {
        case m: Method.OnInstance
            if m.name == "helper" && m.isNullary && m.knownReturning.exists(_.Underlying =:= Type.of[Int]) =>
          import receiver.value as self
          m.apply(self.asInstanceOf[Expr[m.Instance]]) match {
            case r: Method.Result[?] =>
              r.build() match {
                case Right(value) => value.asInstanceOf[Expr[Int]]
                case Left(err)    => Environment.reportErrorAndAbort(s"Failed to build helper call: $err")
              }
            case other =>
              Environment.reportErrorAndAbort(s"Unexpected step after OnInstance: ${other.getClass.getSimpleName}")
          }
      }
    }

    call.getOrElse(Environment.reportErrorAndAbort("No nullary Int-returning `helper` found in the enclosing scope"))
  }

  /** Members declared on a self-type requirement (`this: Provider =>`), not on the trait itself, have to be among the
    * enclosing class's `members` - wiring/DI relies on it.
    */
  def testEnclosingClassHasSelfTypeMember: Expr[Data] = {
    val result = enclosingScope.iterator
      .collectFirst { case enc: Enclosure.Class => enc.members.map(_.name).toSet }
      .map(names => Data(names("fromSelfType")))
      .getOrElse(Data("<no class>"))
    Expr(result)
  }

  /** Local `val`s declared before the macro call have to be discoverable - macwire-style wiring builds on them. */
  def testEnclosingLocalValues: Expr[Data] = {
    val locals: List[Enclosure.LocalValue] = enclosingScope.iterator
      .collectFirst { case m: Enclosure.Method => m.localValues }
      .getOrElse(Nil)
    val rendered = locals.map(lv => Data.map("name" -> Data(lv.name), "type" -> Data(lv.tpe.plainPrint)))
    Expr(Data.list(rendered*))
  }

  /** Checks that the `ref`s work in generated code, not only that the local vals are listed. */
  def testSumEnclosingLocalInts: Expr[Int] = {
    val refs: List[Expr[Int]] = enclosingScope.iterator
      .collectFirst { case m: Enclosure.Method => m.localValues }
      .getOrElse(Nil)
      .collect {
        case lv if lv.tpe.Underlying =:= Type.of[Int] =>
          lv.ref.value.asInstanceOf[Expr[Int]]
      }
    refs match {
      case Nil          => Expr(0)
      case head :: tail =>
        tail.foldLeft(head) { (acc, next) =>
          Expr.quote(Expr.splice(acc) + Expr.splice(next))
        }
    }
  }

  def testEnclosingMethodParameters: Expr[Data] =
    Expr(Data.list(Environment.enclosingMethodParameters.map { clause =>
      Data.list(clause.map { p =>
        Data.map(
          "name" -> Data(p.name),
          "type" -> Data(p.tpe.plainPrint),
          "isImplicit" -> Data(p.isImplicit),
          "isByName" -> Data(p.isByName)
        )
      }*)
    }*))

  def testEnclosingMethodParameterNames: Expr[Data] =
    Expr(Data.list(Environment.enclosingMethodParameters.map { clause =>
      Data.list(clause.map(p => Data(if (p.isImplicit) s"implicit ${p.name}" else p.name))*)
    }*))

  /** Checks that the `ref`s work in generated code, not only that the parameters are listed. */
  def testSumEnclosingMethodParameters: Expr[Int] = {
    implicit val IntType: Type[Int] = Type.of[Int]
    implicit val SeqIntType: Type[Seq[Int]] = Type.of[Seq[Int]]
    val refs: List[Expr[Int]] = Environment.enclosingMethodParameters.flatten.collect {
      case p if p.tpe.Underlying =:= Type.of[Int]      => p.ref.value.asInstanceOf[Expr[Int]]
      case p if p.tpe.Underlying =:= Type.of[Seq[Int]] =>
        val seq = p.ref.value.asInstanceOf[Expr[Seq[Int]]]
        Expr.quote(Expr.splice(seq).sum)
    }
    refs match {
      case Nil          => Expr(0)
      case head :: tail =>
        tail.foldLeft(head) { (acc, next) =>
          Expr.quote(Expr.splice(acc) + Expr.splice(next))
        }
    }
  }
}
