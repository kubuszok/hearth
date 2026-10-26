package hearth
package typed

/** Scala 2 only: `case class A() { val foo = this.convertInto[B] }` where the implicit materializer reflects on `A`.
  *
  * `Type[A].unsortedMethods` (and forcing member return types) is safe on its own, but when it runs inside an implicit
  * search that fires while an enclosing `val` of `A` is still being inferred, completing `A`'s member signatures
  * re-enters that `val`'s running completer and scala-reflect signals a `CyclicReference`. Leaking it would fail the
  * compilation with `recursive value foo needs type`, so Hearth represents the un-completable member honestly instead:
  * a getter stays listable with no parameters and its unknown return type surfaces as `knownReturning == None`.
  *
  * [[CyclicTC]] is a type class whose materializer reflects on the source. [[PlainTC]] is an identical type class whose
  * materializer does NOT reflect — the control proving the reflection is what turns the `val` recursive.
  */
final class CyclicReferenceScala2Spec extends MacroSuite {

  group("regression: reflecting an enclosing mid-inference member") {

    test("a reflecting materializer no longer breaks an inferred `val` of the reflected type") {
      import hearth.typed.CyclicConvertSyntax.*
      case class B()
      // No explicit type on `foo` - this is what makes `foo` mid-inference while the materializer reflects on `A`.
      case class A() { val foo = this.cyclicConvertInto[B] }
      A().foo ==> null
    }

    test("control: the SAME shape with a non-reflecting materializer compiles (reflection was the trigger)") {
      import hearth.typed.CyclicConvertSyntax.*
      case class B()
      case class A() { val foo = this.plainConvertInto[B] }
      A().foo ==> null
    }

    test("an explicit type on the enclosing member also compiles (the pre-fix workaround still works)") {
      import hearth.typed.CyclicConvertSyntax.*
      case class B()
      case class A() { val foo: B = this.cyclicConvertInto[B] } // explicit type -> no re-entry
      A().foo ==> null
    }

    test("on a FULLY-TYPED type, getters still resolve their return type (the fix only yields None on a real cycle)") {
      val report = CyclicReferenceFixtures.knownReturningReport[FullyTypedForReturning]
      report.contains("name:Some") ==> true
      report.contains("age:Some") ==> true
      report.contains("copy:Some") ==> true
      report.contains("toString:Some") ==> true
      // Generic Object members have an unresolved type parameter, so their return type is unknown regardless of cycles.
      report.split(",").filter(_.endsWith(":None")).map(_.takeWhile(_ != ':')).sorted.toList ==>
        List("asInstanceOf", "isInstanceOf", "synchronized")
    }
  }
}

final case class FullyTypedForReturning(name: String, age: Int)
