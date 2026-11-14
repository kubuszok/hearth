package hearth
package examples
package exportaliases

// The symbol of an alias created by `export` is final (a forwarder cannot be overridden), unlike the symbol of an
// alias written by hand, so each exported type here has a hand-written alias next to it.

enum Mode {
  case A, B
}

object Outer {
  export hearth.examples.exportaliases.Mode

  type ModeAlias = hearth.examples.exportaliases.Mode

  final case class ViaExport(mode: Mode)
  final case class ViaAlias(mode: ModeAlias)
}

object api {

  enum ParameterizedEnum(val wire: String) {
    case A extends ParameterizedEnum("a")
    case B extends ParameterizedEnum("b")
  }

  enum ParameterlessEnum {
    case A, B
  }
}

object Agents {
  export api.ParameterizedEnum as ParameterizedEnumExport
  export api.ParameterlessEnum as ParameterlessEnumExport

  type ParameterizedEnumAlias = api.ParameterizedEnum
  type ParameterlessEnumAlias = api.ParameterlessEnum
}
