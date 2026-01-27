package hearth
package sandwich

import hearth.data.Data
import hearth.sandwich.examples213.Examples213
import hearth.sandwich.examples3.Examples3
import hearth.typed.ClassesFixtures

/** Sandwich tests for typed.Classes views: Class[A], CaseClass[A], JavaBean[A], and Enum[A].
  */
final class ClassesSandwichSpec extends MacroSuite {

  private def asCaseClassHasFields(data: Data): Boolean =
    data.asMap.exists { m =>
      m.get("asCaseClass") match {
        case Some(value) =>
          value.asMap.exists(_.contains("caseFields"))
        case None =>
          false
      }
    }

  private def asJavaBeanHasDefaultCtor(data: Data): Boolean =
    data.asMap.exists { m =>
      m.get("asJavaBean") match {
        case Some(value) =>
          value.asMap.exists(_.contains("defaultConstructor"))
        case None =>
          false
      }
    }

  private def asEnumChildrenNonEmpty(data: Data): Boolean =
    data.asMap.exists { m =>
      m.get("asEnum") match {
        case Some(value) =>
          value.asMap.exists { enumMap =>
            val directOk =
              enumMap.get("directChildren").exists(child => child.asString.exists(_.nonEmpty))
            val exhaustiveOk =
              enumMap.get("exhaustiveChildren").exists(child => child.asString.exists(_.nonEmpty))
            directOk && exhaustiveOk
          }
        case None =>
          false
      }
    }

  group("typed.Classes – CaseClass view across Scala versions") {

    test("Scala 2 macros see Scala 3 case class as CaseClass".ignoreOnScala3) {
      val data: Data = ClassesFixtures.testClass[Examples3.CaseClassWithDefaults]()
      assert(asCaseClassHasFields(data))
    }

    test("Scala 3 macros see Scala 2 case class as CaseClass".ignoreOnScala2_13) {
      val data: Data = ClassesFixtures.testClass[Examples213.CaseClassWithDefaults]()
      assert(asCaseClassHasFields(data))
    }
  }

  group("typed.Classes – JavaBean view across Scala versions") {

    test("Scala 2 macros see Scala 3 JavaBean".ignoreOnScala3) {
      val data: Data = ClassesFixtures.testClass[Examples3.BeanExample]()
      assert(asJavaBeanHasDefaultCtor(data))
    }

    test("Scala 3 macros see Scala 2 JavaBean".ignoreOnScala2_13) {
      val data: Data = ClassesFixtures.testClass[Examples213.BeanExample]()
      assert(asJavaBeanHasDefaultCtor(data))
    }
  }

  group("typed.Classes – Enum view across Scala versions") {

    test("Scala 2 macros see Scala 3 enum via Enum[A]".ignoreOnScala3) {
      val data: Data = ClassesFixtures.testClass[Examples3.Color3]()
      assert(asEnumChildrenNonEmpty(data))
    }

    test("Scala 3 macros see Scala 2 sealed trait via Enum[A]".ignoreOnScala2_13) {
      val data: Data = ClassesFixtures.testClass[Examples213.Color213]()
      assert(asEnumChildrenNonEmpty(data))
    }
  }
}
