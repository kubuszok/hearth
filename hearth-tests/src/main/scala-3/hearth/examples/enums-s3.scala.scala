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
