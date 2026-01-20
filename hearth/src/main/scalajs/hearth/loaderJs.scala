package hearth

import java.util.ServiceLoader

private[hearth] trait platformSpecificServiceLoaderCompat { this: platformSpecificServiceLoader.type =>

  protected def createServiceLoader[T](clazz: Class[T], classLoader: ClassLoader): Tried[ServiceLoader[T]] =
    Tried(ServiceLoader.load(clazz, classLoader))
}
