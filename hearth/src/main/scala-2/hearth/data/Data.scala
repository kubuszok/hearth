package hearth
package data

object Data extends DataCommons { self =>

  type Impl // null | Int | Long | Float | Double | Boolean | String | List[Data] | Map[String, Data]

  /** Empty/absent [[Data]] (renders as `null`).
    * @since 0.1.0
    */
  override def apply(): Data = null.asInstanceOf[Data]

  /** Wraps a primitive, `List[Data]` or `Map[String, Data]` as [[Data]].
    * @since 0.1.0
    */
  override def apply(value: Int): Data = value.asInstanceOf[Data]
  override def apply(value: Long): Data = value.asInstanceOf[Data]
  override def apply(value: Float): Data = value.asInstanceOf[Data]
  override def apply(value: Double): Data = value.asInstanceOf[Data]
  override def apply(value: Boolean): Data = value.asInstanceOf[Data]
  override def apply(value: String): Data = value.asInstanceOf[Data]
  override def apply(value: List[Data]): Data = value.asInstanceOf[Data]
  override def apply(value: Map[String, Data]): Data = value.asInstanceOf[Data]

  implicit final class DataOps(private val data: Data) extends AnyVal {

    /** Pattern-matches the 9 possible [[Data]] shapes, applying the matching handler.
      * @since 0.1.0
      */
    def fold[A](
        onNull: => A,
        onInt: Int => A,
        onLong: Long => A,
        onFloat: Float => A,
        onDouble: Double => A,
        onBoolean: Boolean => A,
        onString: String => A,
        onList: List[Data] => A,
        onMap: Map[String, Data] => A
    ): A = self.fold(data)(onNull, onInt, onLong, onFloat, onDouble, onBoolean, onString, onList, onMap)

    /** Optional accessors: `Some` iff this [[Data]] holds that shape, else `None`.
      *
      * Prefer the `<==>` assertion over `.asMap.get(...)` when writing tests.
      *
      * @since 0.1.0
      */
    // format: off
    def asNull: Option[Unit] = fold(onNull = Some(()), onInt = _ => None, onLong = _ => None, onFloat = _ => None, onDouble = _ => None, onBoolean = _ => None, onString = _ => None, onList = _ => None, onMap = _ => None)
    def asInt: Option[Int] = fold(None, onInt = Some(_), onLong = _ => None, onFloat = _ => None, onDouble = _ => None, onBoolean = _ => None, onString = _ => None, onList = _ => None, onMap = _ => None)
    def asLong: Option[Long] = fold(None, onInt = _ => None, onLong = Some(_), onFloat = _ => None, onDouble = _ => None, onBoolean = _ => None, onString = _ => None, onList = _ => None, onMap = _ => None)
    def asFloat: Option[Float] = fold(None, onInt = _ => None, onLong = _ => None, onFloat = Some(_), onDouble = _ => None, onBoolean = _ => None, onString = _ => None, onList = _ => None, onMap = _ => None)
    def asDouble: Option[Double] = fold(None, onInt = _ => None, onLong = _ => None, onFloat = _ => None, onDouble = Some(_), onBoolean = _ => None, onString = _ => None, onList = _ => None, onMap = _ => None)
    def asBoolean: Option[Boolean] = fold(None, onInt = _ => None, onLong = _ => None, onFloat = _ => None, onDouble = _ => None, onBoolean = Some(_), onString = _ => None, onList = _ => None, onMap = _ => None)
    def asString: Option[String] = fold(None, onInt = _ => None, onLong = _ => None, onFloat = _ => None, onDouble = _ => None, onBoolean = _ => None, onString = Some(_), onList = _ => None, onMap = _ => None)
    def asList: Option[List[Data]] = fold(None, onInt = _ => None, onLong = _ => None, onFloat = _ => None, onDouble = _ => None, onBoolean = _ => None, onString = _ => None, onList = Some(_), onMap = _ => None)
    def asMap: Option[Map[String, Data]] = fold(None, onInt = _ => None, onLong = _ => None, onFloat = _ => None, onDouble = _ => None, onBoolean = _ => None, onString = _ => None, onList = _ => None, onMap = Some(_))
    // format: on

    // format: off
    def updateAsInt(f: Int => Int): Data = asInt.fold(data)(a => Data(f(a)))
    def updateAsLong(f: Long => Long): Data = asLong.fold(data)(a => Data(f(a)))
    def updateAsFloat(f: Float => Float): Data = asFloat.fold(data)(a => Data(f(a)))
    def updateAsDouble(f: Double => Double): Data = asDouble.fold(data)(a => Data(f(a)))
    def updateAsBoolean(f: Boolean => Boolean): Data = asBoolean.fold(data)(a => Data(f(a)))
    def updateAsString(f: String => String): Data = asString.fold(data)(a => Data(f(a)))
    def updateAsList(f: List[Data] => List[Data]): Data = asList.fold(data)(a => Data(f(a)))
    def updateAsMap(f: Map[String, Data] => Map[String, Data]): Data = asMap.fold(data)(a => Data(f(a)))
    // format: on

    def get(key: String): Option[Data] = asMap.flatMap(_.get(key))
    def getOrElse(key: String, default: Data): Data = get(key).getOrElse(default)

    def get(index: Int): Option[Data] = asList.flatMap(_.lift(index))
    def getOrElse(index: Int, default: Data): Data = get(index).getOrElse(default)

    /** Structural diff of this [[Data]] against an `expected` one; an empty [[Diff]] means they are equal. Backs the
      * `<==>` assertion.
      *
      * @since 0.1.0
      *
      * @param expected
      *   the value to compare against
      */
    def diff(expected: Data): Diff = self.diff(expected = expected, actual = data)

    /** Pretty, multi-line rendering of this [[Data]].
      * @since 0.1.0
      */
    def render: String = self.render(data)
  }
}
