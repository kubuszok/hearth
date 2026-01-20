package hearth

import java.util.ServiceLoader
import scala.jdk.StreamConverters.*
import scala.util.Try

/** Platform-specific service loader.
  *
  * Workaround for ScalaNative requiring ServiceLoader.load to have literal constant of class type as the first
  * argument.
  *
  * @since 0.1.0
  */
private[hearth] object platformSpecificServiceLoader extends platformSpecificServiceLoaderCompat {

  /** Loads all services from the given class loader.
    *
    * @since 0.1.0
    *
    * @param clazz
    *   the class to load services from
    * @param classLoader
    *   the class loader to load services from
    * @return
    *   the loaded services or the first error encountered
    */
  def load[T](clazz: Class[T], classLoader: ClassLoader): Either[Throwable, Vector[T]] =
    loadWhen(clazz, classLoader)(_ => true)

  /** Loads services from the given class loader, excluding services with names matching the given excluded names.
    *
    * @since 0.3.0
    *
    * @param clazz
    *   the class to load services from
    * @param classLoader
    *   the class loader to load services from
    * @param excluded
    *   the names of the services to exclude (case-insensitive full qualified class names)
    * @return
    *   the loaded services or the first error encountered
    */
  def loadExcluding[T](clazz: Class[T], classLoader: ClassLoader)(excluded: String*): Either[Throwable, Vector[T]] = {
    val excludedSet = excluded.toSet
    loadWhen(clazz, classLoader)(clazz => !excludedSet.contains(clazz.getName))
  }

  /** Loads services from the given class loader, excluding services with names matching the given excluded names.
    *
    * @since 0.3.0
    *
    * @param clazz
    *   the class to load services from
    * @param classLoader
    *   the class loader to load services from
    * @param condition
    *   the condition to filter services by
    * @return
    *   the loaded services or the first error encountered
    */
  def loadWhen[T](clazz: Class[T], classLoader: ClassLoader)(
      condition: Class[?] => Boolean
  ): Either[Throwable, Vector[T]] =
    getServiceLoader(clazz, classLoader).flatMap { loader =>
      val stream = loader.stream().toScala(Iterable).iterator

      def loop(acc: Vector[T]): Tried[Vector[T]] =
        if (stream.hasNext) {
          val provider = stream.next()
          if (condition(provider.`type`)) {
            getService(provider) match {
              case Right(service) => loop(acc :+ service)
              case Left(error)    => Left(error)
            }
          } else loop(acc)
        } else Right(acc)

      loop(Vector.empty[T])
    }

  protected type Tried[A] = Either[Throwable, A]
  protected object Tried {
    def apply[A](value: => A): Tried[A] = Try(value).toEither
  }

  // Caches previous results within the same class loader (compilation unit).

  private val serviceLoaders = scala.collection.mutable.WeakHashMap.empty[Class[?], Tried[ServiceLoader[?]]]
  private def getServiceLoader[T](clazz: Class[T], classLoader: ClassLoader): Tried[ServiceLoader[T]] =
    serviceLoaders.getOrElseUpdate(clazz, createServiceLoader(clazz, classLoader)).asInstanceOf[Tried[ServiceLoader[T]]]

  private val services = scala.collection.mutable.WeakHashMap.empty[Class[?], Tried[Any]]
  private def getService[T](provider: ServiceLoader.Provider[T]): Tried[T] =
    services.getOrElseUpdate(provider.`type`, Tried(provider.get())).asInstanceOf[Tried[T]]
}
