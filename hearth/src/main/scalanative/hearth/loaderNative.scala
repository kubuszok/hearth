package hearth

import java.util.ServiceLoader

private[hearth] trait platformSpecificServiceLoaderCompat { this: platformSpecificServiceLoader.type =>

  protected def createServiceLoader[T](clazz: Class[T], classLoader: ClassLoader): Tried[ServiceLoader[T]] = Tried(
    // Workaround for guard:
    //   Limitation of ScalaNative runtime: first argument of method load needs to be literal constant of class type, use `classOf[T]` instead.
    // which doesn't affect us as ServiceLoader is used only inside macros, which are not compiled to native code.
    classLoader
      .loadClass("java.util.ServiceLoader")
      .getDeclaredMethod("load", classOf[Class[T]], classOf[ClassLoader])
      .invoke(null, clazz, classLoader)
      .asInstanceOf[ServiceLoader[T]]
  )
}
