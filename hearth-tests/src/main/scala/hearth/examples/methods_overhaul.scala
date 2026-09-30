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

// [hearth#331] a value/implicit clause that FOLLOWS a type-parameter clause: `Method.fold` used to hand `onValues`
// an EMPTY clause here (the params were dropped), so there was no way to supply the `Sync[F]`.
trait Sync[F[_]]
class HigherKindedImplicit {
  @scala.annotation.nowarn
  def resource[F[_]](implicit ev: Sync[F]): F[Int] = null.asInstanceOf[F[Int]]
  @scala.annotation.nowarn
  def make[F[_]](config: String)(implicit ev: Sync[F]): F[Int] = null.asInstanceOf[F[Int]]
}

class ProperKindedImplicit {
  // [hearth#331] value + implicit clauses that follow a `[T]` clause; used to check that applying `T := Int` in
  // `onTypes` substitutes into the subsequent clauses (`(t: T)(implicit ord: Ordering[T])`).
  @scala.annotation.nowarn
  def pick[T](t: T)(implicit ord: Ordering[T]): T = t
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

class WithVarargs {
  def varargMethod(xs: Int*): Int = xs.sum
  def normalMethod(x: Int): Int = x
  def byNameMethod(x: => Int): Int = x
}

class WithVarargsCtor(val xs: String*) {
  override def toString(): String = s"WithVarargsCtor(${xs.mkString(",")})"
}

// Chimney #960: a type alias whose type arguments do not line up with the aliased class' type parameters
// (different arity, fixed arguments, reordered arguments).
final class PhantomParams[R, E, A](val a: Int, val b: String) {
  override def toString(): String = s"PhantomParams($a, $b)"
}
object PhantomParams {
  type Partial[A] = PhantomParams[Any, Nothing, A]
  type Fixed = PhantomParams[Any, Nothing, String]
  // chained aliases: renamed, reordered, partially fixed type parameters
  type PartialOfPartial[B] = Partial[B]
  type Reordered[X, Y, Z] = PhantomParams[Z, X, Y]
  type ReorderedAgain[P, Q] = Reordered[Q, P, Any]
  type FixedThroughChain = ReorderedAgain[String, Nothing]
}
object GenericClassAliases {
  type Swapped[A, B] = GenericClass[B, A]
  // swapping twice restores the original order
  type SwappedTwice[X, Y] = Swapped[Y, X]
  // swapped, then partially fixed
  type SwappedPartial[C] = Swapped[C, String]
  // swapped, then renamed and swapped again through another alias
  type SwappedTwiceSwapped[L, R] = SwappedTwice[R, L]
}
