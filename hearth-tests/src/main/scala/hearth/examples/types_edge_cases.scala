package hearth
package examples
package typesedge

trait Root
trait Middle extends Root
trait Generic[A]
class Leaf extends Middle with Generic[Int]
class GenericLeaf[A] extends Generic[List[A]]

abstract class AbstractBase

sealed abstract class SealedBase
object SealedBase {
  final case class First(i: Int) extends SealedBase
  case object Second extends SealedBase
}

sealed trait Shape
sealed trait Round extends Shape
final case class Circle(r: Double) extends Round
case object Dot extends Round
final case class Square(a: Double) extends Shape

sealed trait Clash
object Clash {
  case object Green extends Clash
}
object OtherScope {
  case object Green extends Clash
}

sealed trait NoChildren

object PlainObject
case object TopLevelCaseObject

final case class FinalCaseClass(a: Int)

object aliases {
  type CaseClassAlias = FinalCaseClass
  type TraitAlias = Root
  type SealedAlias = Shape
  type IntAlias = Int
  type ListAlias[A] = List[A]
  type Flipped[B, A] = Either[A, B]
  type Const[A] = String
  type Identity[A] = A
}

class WithCompanion
object WithCompanion
class WithoutCompanion
trait TraitWithCompanion
object TraitWithCompanion

class PublicBean
class PackagePrivateBean private[hearth] ()
class PrivateBean private ()
object PrivateBean {
  def create(): PrivateBean = new PrivateBean()
}
class NoDefaultConstructorBean(val a: Int)
