package hearth
package examples
package methods

class NullaryMethods {
  def nullaryNoParamList: Int = 42
  def nullaryEmptyParamList(): Int = 42
}

class MultiParamListMethods {
  def singleParamList(arg1: Int, arg2: String): Boolean = true
  def multiParamList(arg1: Int)(arg2: String): Boolean = true
}

class Parametric {
  def parametric1[A](a: A): List[A] = List(a)
  def parametric2[A](a: A): A = a
  def parametricBounded[A <: Comparable[A]](a: A): A = a
}

class GenericClass[A, B](val a: A, val b: B) {
  @scala.annotation.nowarn
  def method(x: A, y: B): B = y
  def swap: GenericClass[B, A] = new GenericClass(b, a)
}

class SimpleConstructor(val a: Int, val b: String)
class DefaultConstructor(val a: Int = 0, val b: String = "")

class PathDepReturn {
  class Inner { type Result = String }
  @scala.annotation.nowarn
  def pathDepResult(arg: Inner): arg.Result = ""
}

class PathDepArgs {
  class Wrapper { type Inner = Int; type Result = String }
  @scala.annotation.nowarn
  def pathDep1(arg: Wrapper)(arg2: arg.Inner): String = ""
  @scala.annotation.nowarn
  def pathDep2(arg: Wrapper)(arg2: arg.Inner)(arg3: arg.Result): Boolean = true
}

class PathDepArgs2 {
  class W1 { type T1 = Int }
  class W2 { type T2 = String }
  @scala.annotation.nowarn
  def pathDep3(arg: W1)(arg2: arg.T1)(arg3: W2)(arg4: arg3.T2): Boolean = true
}

class GenericCtor[A](val value: A)

class HigherKinded {
  def higherKinded[F[_]](a: F[String]): F[String] = a
}

class WithImplicitParam {
  @scala.annotation.nowarn
  def withImplicit(a: Int)(implicit b: String): String = s"$a $b"
}

trait MethodModifiers {
  def abstractMethod(a: Int): String
  final def finalMethod(a: Int): String = a.toString
  def concreteMethod(a: Int): String = a.toString
}

abstract class AbstractWithModifiers {
  def abstractDef: Int
  final def finalDef: Int = 42
  def concreteDef: Int = 0
}

class OverridingChild extends AbstractWithModifiers {
  override def abstractDef: Int = 1
  override def concreteDef: Int = 2
}

case class GenericWithDefaults[A](value: A, label: String = "unlabeled")

trait TraitWithAbstractMethod {
  def compute(x: Int): String
}
class TraitWithAbstractMethodImpl extends TraitWithAbstractMethod {
  def compute(x: Int): String = s"result:$x"
}

trait SimpleAlg {
  def getUser(id: Int): String
}
class SimpleAlgImpl extends SimpleAlg {
  def getUser(id: Int): String = s"user:$id"
}
