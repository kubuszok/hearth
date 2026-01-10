package hearth

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

  def load[T](clazz: Class[T], classLoader: ClassLoader): Either[Throwable, Vector[T]] = try
    Right(ServiceLoader.load(clazz, classLoader).asScala.toVector)
  catch {
    case e: Throwable => Left(e)
  }
}
