package hearth

import hearth.fp.data.NonEmptyVector
import java.util.ServiceLoader
import scala.jdk.CollectionConverters.*

/** Platform-specific service loader.
  *
  * Workaround for ScalaNative requiring ServiceLoader.load to have literal constant of class type as the first
  * argument.
  *
  * @since 0.1.0
  */
private[hearth] object platformSpecificServiceLoader {

  def load[T](clazz: Class[T], classLoader: ClassLoader): Either[NonEmptyVector[Throwable], Vector[T]] = try
    Right(
      // Workaround for guard:
      //   Limitation of ScalaNative runtime: first argument of method load needs to be literal constant of class type, use `classOf[T]` instead.
      // which doesn't affect us as ServiceLoader is used only inside macros, which are not compiled to native code.
      classLoader
        .loadClass("java.util.ServiceLoader")
        .getDeclaredMethod("load", classOf[Class[T]], classOf[ClassLoader])
        .invoke(null, clazz, classLoader)
        .asInstanceOf[ServiceLoader[T]]
        .asScala
        .toVector
    )
  catch {
    case e: Throwable => Left(NonEmptyVector.one(e))
  }
}
