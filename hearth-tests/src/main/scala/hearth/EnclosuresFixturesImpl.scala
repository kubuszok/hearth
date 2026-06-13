package hearth

import hearth.data.Data

/** Fixtures for testing [[EnclosuresSpec]]. */
trait EnclosuresFixturesImpl { this: MacroCommons =>

  /** Render the enclosing scope chain (innermost -> outermost) as a list of `{kind, name}` maps.
    *
    * Package enclosures are filtered out of the rendered chain because their names/segmentation differ between Scala 2
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

  /** Find a nullary `Int`-returning member named `helper` in the immediately-enclosing class/object and call it on the
    * enclosing instance. Proves "allow calling methods/values from the scope".
    */
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

  /** Render the `localValues` of the immediately-enclosing method as a list of `{name, type}` maps. Proves that local
    * `val`s declared before the macro call are discoverable (the macwire case).
    */
  def testEnclosingLocalValues: Expr[Data] = {
    val locals: List[Enclosure.LocalValue] = enclosingScope.iterator
      .collectFirst { case m: Enclosure.Method => m.localValues }
      .getOrElse(Nil)
    val rendered = locals.map(lv => Data.map("name" -> Data(lv.name), "type" -> Data(lv.tpe.plainPrint)))
    Expr(Data.list(rendered*))
  }

  /** Sum the discovered local `Int` vals by USING their `ref` expressions in the generated code. Proves the ref is a
    * usable expression (not just metadata).
    */
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
}
