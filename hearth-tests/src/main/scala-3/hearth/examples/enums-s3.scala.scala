package hearth
package examples

enum ExampleEnum {
  case ExampleEnumClass(a: Int)
  case ExampleEnumValue
}

enum ExampleEnumWithTypeParam[+A] {
  case ExampleEnumWithTypeParamClass(a: A)
  case ExampleEnumWithTypeParamValue
}

enum ExampleEnumGADT[A] {
  case ExampleEnumWithTypeParamClass(str: String) extends ExampleEnumGADT[String]
  case ExampleEnumWithTypeParamValue extends ExampleEnumGADT[Unit]
}

// Chimney #625: LOWERCASE parameterless enum cases. Hearth's `matchOn`/`MatchCase.typeMatch` emits
// `case x @ Ref(sym)` for enum vals; dotty (scala/scala3#20350) reinterprets a bare lowercase `Ident`
// in pattern position as a variable pattern, so the first case swallows everything.
enum LowerEnum1 {
  case solo, team, school
}
enum LowerEnum2 {
  case solo, team, school
}
