package hearth
package data

import fp.data.NonEmptyList
import scala.collection.immutable.ListMap

/** Implementation for [[Data]] companion object. */
private[data] trait DataCommons {

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

  final def parseString(string: String): Either[String, Data] = string.trim match {
    case s @ digitRegex(_, _) =>
      try
        Right(if (s.endsWith("f")) Data(s.toFloat) else Data(s.toDouble))
      catch {
        case e: NumberFormatException => Left(e.getMessage)
      }
    case s @ intRegex(n) =>
      try
        Right(if (s.endsWith("L")) Data(n.toLong) else Data(n.toInt))
      catch {
        case e: NumberFormatException => Left(e.getMessage)
      }
    case s if s.startsWith("\"") && s.endsWith("\"") =>
      Right(Data(s.drop(1).dropRight(1)))
    case s if s.equalsIgnoreCase("true") || s.equalsIgnoreCase("false") =>
      Right(Data(s.toLowerCase.toBoolean))
    case _ => Right(Data(string))
  }
  private val digitRegex = """^-?\d+(\.\d+)?(f|d)?$""".r
  private val intRegex = """^(-?\d+)L?$""".r

  final def parseList(strings: List[String]): Either[String, Data] =
    parsePairs(strings.collect { case s"$name=$value" =>
      (NonEmptyList.fromList(name.split('.').toList).get, value)
    }).left.map(_.mkString("\n"))

  private def parsePairs(pairs: List[(NonEmptyList[String], String)]): Either[List[String], Data] = {
    val x = pairs.groupMap(k => k._1.head) { case (k, v) =>
      k.tail -> v
    }
    val (errors, data) = x.partitionMap[List[String], (String, Data)] {
      // No values for key -> we return an error
      case (key, List()) =>
        Left(List(s"$key: no value"))
      // Single value for key -> we parse it
      case (key, List((Nil, str))) =>
        parseString(str).map(data => key -> data).left.map(e => List(s"$key: $e"))
      // Multiple values for key -> we group them by nested keys
      case (key, values) =>
        val (nested, leafs) = values.partitionMap { case (keys, str) =>
          NonEmptyList.fromList(keys) match {
            case Some(keys) => Left(keys -> str)
            case None       => Right(str)
          }
        }
        if (nested.isEmpty) {
          // No nested values -> there should be exactly one leaf
          if (leafs.isEmpty) Left(List(s"$key: no value"))
          else if (leafs.size == 1) parseString(leafs.head).map(data => key -> data).left.map(e => List(s"$key: $e"))
          else Left(List(s"$key: multiple values: ${leafs.mkString(", ")}"))
        } else {
          // There are nested values -> there should be no leafs
          if (leafs.isEmpty) parsePairs(nested).map(data => key -> data)
          else
            Left(
              List(
                s"$key: values: ${leafs.mkString(", ")} AND nested values: ${nested.map(_._1.toList.mkString(".")).mkString(", ")}"
              )
            )
        }
    }
    if (errors.nonEmpty) Left(errors.flatten.toList)
    else Right(Data.map(data.toSeq*))
  }

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
    onString = s => '"'.toString + s.replace("\"", "\\\"") + '"'.toString,
    onList = l =>
      if (l.isEmpty) "[]"
      else l.map(_.render.split("\n").map("  " + _).mkString("\n")).mkString("[\n", ",\n", "\n]"),
    onMap = m =>
      if (m.isEmpty) "{}"
      else
        m
          .map { case (k, v) => (s"$k: ${v.render}").split("\n").map("  " + _).mkString("\n") }
          .mkString("{\n", ",\n", "\n}")
  )
}
