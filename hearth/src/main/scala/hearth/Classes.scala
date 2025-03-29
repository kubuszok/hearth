package hearth

trait Classes { this: MacroCommons =>

  type Definition[A]

  val Definition: DefinitionModule
  trait DefinitionModule { this: Definition.type =>

  }

  implicit class DefinitionMethods[A](private val definition: Definition[A]) {

    def name: String = definition.toString

    def function: Function[A] = ???

    def isVal: Boolean = ???
    def isVar: Boolean = ???
    def isLazy: Boolean = ???
    def isDef: Boolean = ???
    def isInherited: Boolean = ???
    def isImplicit: Boolean = ???

    def isAvailableHere: Boolean = ???
  }

  type ClassData[A]

  val ClassData: ClassDataModule
  trait ClassDataModule { this: ClassData.type =>

  }

  implicit class ClassDataMethods[A](private val classData: ClassData[A]) {

    def simpleName: String = classData.toString
    def fcqn: String = ???
    def prettyPrint: String = ???

    def primaryConstructor: Option[Function[A]] = ???
    def constructorsAvailableHere: List[Function[A]] = ???

    def definitions: Any = ???

    def isAbstract: Boolean = ???
    def isFinal: Boolean = ???
    def isCaseClass: Boolean = ???
    def isJavaBean: Boolean = ???

    def isAvailableHere: Boolean = ???
  }
}
