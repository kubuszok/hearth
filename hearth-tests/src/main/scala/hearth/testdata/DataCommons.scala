package hearth
package testdata

import scala.collection.immutable.ListMap

/** Implementation for [[Data]] companion object. */
private[testdata] trait DataCommons {

  def apply(): Data
  def apply(value: Int): Data
  def apply(value: Long): Data
  def apply(value: Float): Data
  def apply(value: Double): Data
  def apply(value: Boolean): Data
  def apply(value: String): Data
  def apply(value: List[Data]): Data
  def apply(value: Map[String, Data]): Data

  final def list(values: Data*): Data = apply(values.toList)
  final def map(values: (String, Data)*): Data = apply(ListMap.from(values))

  protected object DataList {
    def unapply(data: Data): Option[List[Data]] = data match {
      case value: List[?] => Some(value.asInstanceOf[List[Data]])
      case _              => None
    }
  }
  protected object DataMap {
    def unapply(data: Data): Option[Map[String, Data]] = data match {
      case value: Map[?, ?] => Some(value.asInstanceOf[Map[String, Data]])
      case _                => None
    }
  }

  final protected def fold[A](data: Data)(
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
    case x if x == null.asInstanceOf[Data] => onNull
    case value: Int                        => onInt(value)
    case value: Long                       => onLong(value)
    case value: Float                      => onFloat(value)
    case value: Double                     => onDouble(value)
    case value: Boolean                    => onBoolean(value)
    case value: String                     => onString(value)
    case DataList(value)                   => onList(value)
    case DataMap(value)                    => onMap(value)
  }

  final protected def diff(expected: Data, actual: Data): Diff = (expected, actual) match {
    case (DataList(expected), DataList(actual)) =>
      expected.zipAll(actual, Data(), Data()).zipWithIndex.flatMap { case ((expected, actual), index) =>
        diff(expected = expected, actual = actual).map(_.prependIndex(index))
      }
    case (DataMap(expected), DataMap(actual)) =>
      (expected.keys.toVector ++ actual.keys.toVector).distinct.toList.flatMap { key =>
        ((expected.get(key), actual.get(key)) match {
          case (Some(expected), Some(actual)) => diff(expected = expected, actual = actual)
          case (Some(expected), None)         => DiffEntry(expected = expected, actual = Data()) :: Nil
          case (None, Some(actual))           => DiffEntry(expected = Data(), actual = actual) :: Nil
          case (None, None)                   => Nil
        }).map(_.prependKey(key))
      }
    case (expected, actual) if expected != actual => DiffEntry(expected = expected, actual = actual) :: Nil
    case _                                        => Nil
  }

  final protected def render(data: Data): String = fold(data)(
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
