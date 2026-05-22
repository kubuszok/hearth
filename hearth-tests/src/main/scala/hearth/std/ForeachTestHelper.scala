package hearth
package std

object ForeachTestHelper {
  private val buf = new ThreadLocal[scala.collection.mutable.ListBuffer[String]] {
    override def initialValue(): scala.collection.mutable.ListBuffer[String] =
      scala.collection.mutable.ListBuffer.empty[String]
  }

  def reset(): Unit = buf.get().clear()
  def add(s: String): Unit = buf.get() += s
  def result(): List[String] = buf.get().toList
}
