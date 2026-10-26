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

// LOWERCASE parameterless enum cases: if `matchOn`/`MatchCase.typeMatch` emitted `case x @ Ref(sym)` for enum vals,
// dotty (scala/scala3#20350) would reinterpret a bare lowercase `Ident` in pattern position as a variable pattern, so
// the first case would swallow everything.
enum LowerEnum1 {
  case solo, team, school
}
enum LowerEnum2 {
  case solo, team, school
}
