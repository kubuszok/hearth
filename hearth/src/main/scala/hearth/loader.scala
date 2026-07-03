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

  /** Result of a ServiceLoader run that survived individual provider failures.
    *
    * @param services
    *   the providers that loaded successfully
    * @param failures
    *   the providers that failed to load, paired with the cause (fully-qualified class name where known); a non-empty
    *   `failures` means one or more service jars were skipped rather than aborting the whole load. See issue #325.
    */
  final case class Loaded[+T](services: Vector[T], failures: Vector[(String, Throwable)])

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
  def load[T](clazz: Class[T], classLoader: ClassLoader): Either[Throwable, Loaded[T]] =
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
    *   the names of the services to exclude (case-sensitive full qualified class names)
    * @return
    *   the loaded services or the first error encountered
    */
  def loadExcluding[T](clazz: Class[T], classLoader: ClassLoader)(excluded: String*): Either[Throwable, Loaded[T]] = {
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
  ): Either[Throwable, Loaded[T]] =
    getServiceLoader(clazz, classLoader).map { loader =>
      val stream = loader.stream().toScala(Iterable).iterator

      // A single unloadable provider (e.g. a service jar with a forward-incompatible TASTy version) must NOT abort the
      // whole ServiceLoader iteration - that would poison EVERY derivation in the module, including ones that never use
      // that extension and Hearth's own built-in providers. Instead we skip the failing provider, remember the failure
      // so it can be reported, and keep loading the rest. Each step is guarded, because the failure can surface either
      // while advancing the (lazy) iterator, while resolving the provider's class, or while instantiating it. See #325.
      def loop(acc: Vector[T], failures: Vector[(String, Throwable)]): Loaded[T] =
        Tried(stream.hasNext) match {
          case Left(error)  => Loaded(acc, failures :+ ("<unknown provider>" -> error))
          case Right(false) => Loaded(acc, failures)
          case Right(true)  =>
            Tried(stream.next()) match {
              case Left(error)     => Loaded(acc, failures :+ ("<unknown provider>" -> error))
              case Right(provider) =>
                Tried(provider.`type`) match {
                  case Left(error) => loop(acc, failures :+ ("<unknown provider>" -> error))
                  case Right(providerType) if !condition(providerType) => loop(acc, failures)
                  case Right(providerType)                             =>
                    getService(classLoader, provider) match {
                      case Right(service) => loop(acc :+ service, failures)
                      case Left(error)    => loop(acc, failures :+ (providerType.getName -> error))
                    }
                }
            }
        }

      loop(Vector.empty[T], Vector.empty)
    }

  protected type Tried[A] = Either[Throwable, A]
  protected object Tried {
    def apply[A](value: => A): Tried[A] = Try(value).toEither
  }

  // Caches previous results within the same compilation ClassLoader.
  //
  // Keying on ClassLoader (not Class) provides correct cache scoping: all services loaded from the
  // same compilation ClassLoader share one cache entry, and the WeakHashMap allows cleanup when the
  // ClassLoader is GC'd between compilation units. String class names are used as inner keys since
  // they are stable and won't be collected.

  private val serviceLoadersByClassLoader =
    new java.util.WeakHashMap[ClassLoader, java.util.HashMap[String, Tried[ServiceLoader[?]]]]()

  private def getServiceLoader[T](clazz: Class[T], classLoader: ClassLoader): Tried[ServiceLoader[T]] = {
    var inner = serviceLoadersByClassLoader.get(classLoader)
    if (inner == null) {
      inner = new java.util.HashMap()
      serviceLoadersByClassLoader.put(classLoader, inner): Unit
    }
    val key = clazz.getName
    val cached = inner.get(key)
    if (cached != null) cached.asInstanceOf[Tried[ServiceLoader[T]]]
    else {
      val result = createServiceLoader(clazz, classLoader)
      inner.put(key, result): Unit
      result.asInstanceOf[Tried[ServiceLoader[T]]]
    }
  }

  private val servicesByClassLoader =
    new java.util.WeakHashMap[ClassLoader, java.util.HashMap[String, Tried[Any]]]()

  private def getService[T](classLoader: ClassLoader, provider: ServiceLoader.Provider[T]): Tried[T] = {
    var inner = servicesByClassLoader.get(classLoader)
    if (inner == null) {
      inner = new java.util.HashMap()
      servicesByClassLoader.put(classLoader, inner): Unit
    }
    val key = provider.`type`.getName
    val cached = inner.get(key)
    if (cached != null) cached.asInstanceOf[Tried[T]]
    else {
      val result = Tried(provider.get())
      inner.put(key, result)
      result.asInstanceOf[Tried[T]]
    }
  }
}
