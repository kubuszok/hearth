package hearth
package typed

import hearth.fp.Id

private[typed] trait TypesCompat { this: MacroCommons =>

  // Implicit Type composition for Scala 3 tuples (*: and EmptyTuple)

  @scala.annotation.nowarn
  implicit lazy val emptyTupleType: Type[EmptyTuple] = {
    given quotes: scala.quoted.Quotes = CrossQuotes.ctx
    scala.quoted.Type.of[EmptyTuple].asInstanceOf[Type[EmptyTuple]]
  }

  @scala.annotation.nowarn
  implicit def tupleConsType[H: Type, T <: Tuple: Type]: Type[H *: T] = {
    given quotes: scala.quoted.Quotes = CrossQuotes.ctx
    given scala.quoted.Type[H] = Type[H].asInstanceOf[scala.quoted.Type[H]]
    given scala.quoted.Type[T] = Type[T].asInstanceOf[scala.quoted.Type[T]]
    scala.quoted.Type.of[H *: T].asInstanceOf[Type[H *: T]]
  }

  trait TypeCompat { this: Type.type =>

    final lazy val EmptyTupleCodec: TypeCodec[EmptyTuple] = new TypeCodec[EmptyTuple] {
      override def toType[B <: EmptyTuple](value: B): Type[B] = emptyTupleType.asInstanceOf[Type[B]]
      override def fromType[B](B: Type[B]): Option[Existential.UpperBounded[EmptyTuple, Id]] = None // TODO
    }

    @scala.annotation.nowarn
    final def TupleConsCodec[H: Type, T <: Tuple: Type]: TypeCodec[H *: T] = new TypeCodec[H *: T] {
      override def toType[B <: H *: T](value: B): Type[B] = tupleConsType[H, T].asInstanceOf[Type[B]]
      override def fromType[B](B: Type[B]): Option[Existential.UpperBounded[H *: T, Id]] = None // TODO
    }
  }

  // Low-priority implicits for TypeCodec - inherited by object TypeCodec,
  // so these have lower priority than the Tuple1-22 codecs defined directly in the object.
  // Note: EmptyTupleCodec is intentionally NOT an implicit here because it would conflict
  // with ModuleCodec (EmptyTuple is a singleton). EmptyTupleCodec is available explicitly
  // via Type.EmptyTupleCodec if needed. TupleConsCodec only needs Type[EmptyTuple]
  // (via emptyTupleType), not TypeCodec[EmptyTuple].
  trait TypeCodecCompat {
    implicit def TupleConsCodec[H: Type, T <: Tuple: Type]: TypeCodec[H *: T] = Type.TupleConsCodec[H, T]
  }
}
