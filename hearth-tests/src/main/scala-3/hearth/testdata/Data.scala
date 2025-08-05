package hearth
package testdata

/** JSON-like data for usage in tests.
  *
  * Since macro can have only 1 result we can generate multiple things at once and return it as something easily
  * diffable.
  */
type Data = Data.Impl
object Data extends DataCommons { self =>

  opaque type Impl = Null | Int | Long | Float | Double | Boolean | String | List[?] | Map[?, ?]

  override def apply(): Data = null
  override def apply(value: Int): Data = value
  override def apply(value: Long): Data = value
  override def apply(value: Float): Data = value
  override def apply(value: Double): Data = value
  override def apply(value: Boolean): Data = value
  override def apply(value: String): Data = value
  override def apply(value: List[Data]): Data = value
  override def apply(value: Map[String, Data]): Data = value

  implicit final class DataOps(private val data: Data) extends AnyVal {

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

    def diff(expected: Data): Diff = self.diff(expected = expected, actual = data)

    def render: String = self.render(data)
  }
}
