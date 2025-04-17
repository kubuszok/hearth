package hearth
package fp

import scala.collection.mutable.ListBuffer
import hearth.fp.syntax.*

object instances {

  implicit val IdentityApplicative: Applicative[Id] = new Applicative[Id] {

    def map2[A, B, C](fa: A, fb: B)(f: (A, B) => C): C = f(fa, fb)

    def pure[A](a: A): A = a
  }

  implicit val ListApplicativeTraverse: ApplicativeTraverse[List] = new ApplicativeTraverse[List] {

    def traverse[G[_]: Applicative, A, B](fa: List[A])(f: A => G[B]): G[List[B]] =
      fa.foldLeft(new ListBuffer[B].pure[G]) { (bufferG, a) =>
        bufferG.map2(f(a)) { (buf: ListBuffer[B], b: B) => buf.append(b); buf } // can't use append 'cause 2.12
      }.map(_.toList)

    def parTraverse[G[_]: Parallel, A, B](fa: List[A])(f: A => G[B]): G[List[B]] =
      fa.foldLeft(new ListBuffer[B].pure[G]) { (bufferG, a) =>
        bufferG.parMap2(f(a)) { (buf: ListBuffer[B], b: B) => buf.append(b); buf } // can't use append 'cause 2.12
      }.map(_.toList)

    def map2[A, B, C](fa: List[A], fb: List[B])(f: (A, B) => C): List[C] = fa.zip(fb).map(f.tupled)

    def pure[A](a: A): List[A] = List(a)
  }

  implicit val VectorApplicativeTraverse: ApplicativeTraverse[Vector] = new ApplicativeTraverse[Vector] {

    def traverse[G[_]: Applicative, A, B](fa: Vector[A])(f: A => G[B]): G[Vector[B]] =
      fa.foldLeft(new ListBuffer[B].pure[G]) { (bufferG, a) =>
        bufferG.map2(f(a)) { (buf: ListBuffer[B], b: B) => buf.append(b); buf } // can't use append 'cause 2.12
      }.map(_.toVector)

    def parTraverse[G[_]: Parallel, A, B](fa: Vector[A])(f: A => G[B]): G[Vector[B]] =
      fa.foldLeft(new ListBuffer[B].pure[G]) { (bufferG, a) =>
        bufferG.parMap2(f(a)) { (buf: ListBuffer[B], b: B) => buf.append(b); buf } // can't use append 'cause 2.12
      }.map(_.toVector)

    def map2[A, B, C](fa: Vector[A], fb: Vector[B])(f: (A, B) => C): Vector[C] = fa.zip(fb).map(f.tupled)

    def pure[A](a: A): Vector[A] = Vector(a)
  }
}
