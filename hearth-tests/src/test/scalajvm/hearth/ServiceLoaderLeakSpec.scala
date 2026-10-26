package hearth

import hearth.fp.ignore
import java.lang.ref.WeakReference
import java.net.URLClassLoader

final class ServiceLoaderLeakSpec extends Suite {

  group("platformSpecificServiceLoader classloader retention") {

    test("old classloader should be GC-eligible after a new classloader is used") {
      val parent = Thread.currentThread().getContextClassLoader

      var cl1: ClassLoader = new URLClassLoader(Array.empty, parent)
      ignore(platformSpecificServiceLoader.load(classOf[Runnable], cl1))
      val witness = new WeakReference[ClassLoader](cl1)
      cl1 = null

      val cl2 = new URLClassLoader(Array.empty, parent)
      ignore(platformSpecificServiceLoader.load(classOf[Runnable], cl2))

      assertCollected(witness)
    }

    test("classloader should be retained while it is the current cache key") {
      val parent = Thread.currentThread().getContextClassLoader

      var cl: ClassLoader = new URLClassLoader(Array.empty, parent)
      ignore(platformSpecificServiceLoader.load(classOf[Runnable], cl))
      val witness = new WeakReference[ClassLoader](cl)
      cl = null

      System.gc()
      assert(witness.get() != null, "classloader was collected while still the active cache key")
    }
  }

  private def assertCollected(ref: WeakReference[?], attempts: Int = 10): Unit = {
    var i = 0
    while (ref.get() != null && i < attempts) {
      System.gc()
      Thread.sleep(50)
      i += 1
    }
    assert(ref.get() == null, s"referent was not collected after $attempts GC attempts")
  }
}
