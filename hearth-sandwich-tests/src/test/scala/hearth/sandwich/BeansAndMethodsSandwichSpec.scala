package hearth
package sandwich

import hearth.data.Data
import hearth.sandwich.examples213.Examples213
import hearth.sandwich.examples3.Examples3
import hearth.typed.ClassesFixtures
import hearth.typed.MethodsFixtures

/** Sandwich tests for typed.Classes and typed.Methods focused on JavaBean
  * accessors and methods with default parameters.
  */
final class BeansAndMethodsSandwichSpec extends MacroSuite {

  private def beanAccessorNames(data: Data): Set[String] = {
    val commons = data.asMap.get.apply("commons").asMap.get
    val methods = commons.apply("methods").asList.get
    methods.iterator
      .flatMap { m =>
        val mm = m.asMap.get
        Some(mm.apply("name").asString.get)
      }
      .toSet
  }

  group("typed.Classes – JavaBeans across Scala versions") {

    test("Scala 2 macros see Scala 3 bean accessors".ignoreOnScala3) {
      val info = ClassesFixtures.testClass[Examples3.BeanExample]()
      val names = beanAccessorNames(info)
      assert(names.contains("getBooleanField"))
      assert(names.contains("setBooleanField"))
      assert(names.contains("getIntField"))
      assert(names.contains("setIntField"))
      assert(names.contains("getStringField"))
      assert(names.contains("setStringField"))
    }

    test("Scala 3 macros see Scala 2 bean accessors".ignoreOnScala2_13) {
      val info = ClassesFixtures.testClass[Examples213.BeanExample]()
      val names = beanAccessorNames(info)
      assert(names.contains("getBooleanField"))
      assert(names.contains("setBooleanField"))
      assert(names.contains("getIntField"))
      assert(names.contains("setIntField"))
      assert(names.contains("getStringField"))
      assert(names.contains("setStringField"))
    }
  }

  group("typed.Methods – methods with default parameters") {

    def hasMethod(data: Data, name: String): Boolean =
      data.asMap.get.contains(name)

    test("Scala 2 macros see Scala 3 methods with defaults".ignoreOnScala3) {
      val methods = MethodsFixtures.testMethodsExtraction[Examples3.MethodsWithDefaults]()
      assert(hasMethod(methods, "addWithDefault(Int)"))
      assert(hasMethod(methods, "scaledAdd(Int, Int)"))
    }

    test("Scala 3 macros see Scala 2 methods with defaults".ignoreOnScala2_13) {
      val methods = MethodsFixtures.testMethodsExtraction[Examples213.MethodsWithDefaults]()
      assert(hasMethod(methods, "addWithDefault(Int)"))
      assert(hasMethod(methods, "scaledAdd(Int, Int)"))
    }
  }
}

