package hearth
package testdata

object Data {

  type Impl // null | Int | Long | Float | Double | Boolean | String | List[Data] | Map[String, Data]

  def apply(): Data = null.asInstanceOf[Data]
  def apply(value: Int): Data = value.asInstanceOf[Data]
  def apply(value: Long): Data = value.asInstanceOf[Data]
  def apply(value: Float): Data = value.asInstanceOf[Data]
  def apply(value: Double): Data = value.asInstanceOf[Data]
  def apply(value: Boolean): Data = value.asInstanceOf[Data]
  def apply(value: String): Data = value.asInstanceOf[Data]
  def apply(value: List[Data]): Data = value.asInstanceOf[Data]
  def apply(value: Map[String, Data]): Data = value.asInstanceOf[Data]

  private object DataNull {
    def unapply(data: Data): Boolean = data == null.asInstanceOf[Data]
  }
  private object DataList {
    def unapply(data: Data): Option[List[Data]] = data match {
      case value: List[?] => Some(value.asInstanceOf[List[Data]])
      case _              => None
    }
  }
  private object DataMap {
    def unapply(data: Data): Option[Map[String, Data]] = data match {
      case value: Map[?, ?] => Some(value.asInstanceOf[Map[String, Data]])
      case _                => None
    }
  }

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
    ): A = data match {
      case x if x == null  => onNull
      case value: Int      => onInt(value)
      case value: Long     => onLong(value)
      case value: Float    => onFloat(value)
      case value: Double   => onDouble(value)
      case value: Boolean  => onBoolean(value)
      case value: String   => onString(value)
      case DataList(value) => onList(value)
      case DataMap(value)  => onMap(value)
    }

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

    def diff(actual: Data): Diff = (data, actual) match {
      case (DataList(expected), DataList(actual)) =>
        expected.zipAll(actual, Data(), Data()).zipWithIndex.flatMap { case ((expected, actual), index) =>
          expected.diff(actual).map(_.prependIndex(index))
        }
      case (DataMap(expected), DataMap(actual)) =>
        (expected.keys.toVector ++ actual.keys.toVector).distinct.toList.flatMap { key =>
          ((expected.get(key), actual.get(key)) match {
            case (Some(expected), Some(actual)) => expected.diff(actual)
            case (Some(expected), None)         => DiffEntry(expected, Data()) :: Nil
            case (None, Some(actual))           => DiffEntry(Data(), actual) :: Nil
            case (None, None)                   => Nil
          }).map(_.prependKey(key))
        }
      case (expected, actual) if expected != actual => DiffEntry(expected, actual) :: Nil
      case _                                        => Nil
    }

    def render: String = fold(
      onNull = "null",
      onInt = _.toString,
      onLong = _.toString + "L",
      onFloat = _.toString + "f",
      onDouble = _.toString,
      onBoolean = _.toString,
      onString = s => '"'.toString + s + '"'.toString,
      onList = _.map(d => "  " + d.render).mkString("[\n", ",\n", "\n]"),
      onMap = _.map { case (key, value) => s"  $key: ${value.render}" }.mkString("{\n", ",\n", "\n}")
    )
  }
}
