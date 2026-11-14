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
}
