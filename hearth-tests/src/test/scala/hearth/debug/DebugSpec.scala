package hearth
package debug

final class DebugSpec extends MacroSuite {

  group("debug.Debug") {

    group("object methods") {

      test("withFinalCodeInIDE passes through value") {
        Debug.withFinalCodeInIDE("hello") ==> "hello"
      }

      test("withFinalASTInIDE passes through value") {
        Debug.withFinalASTInIDE(42) ==> 42
      }

      test("withInferredTypeInIDE passes through value") {
        Debug.withInferredTypeInIDE("test") ==> "test"
      }

      test("withInferredTypeInIDE with complex types") {
        Debug.withInferredTypeInIDE(List(1, 2, 3)) ==> List(1, 2, 3)
      }

      test("withGivenCodeInIDE summons implicit") {
        val o: Ordering[Int] = Debug.withGivenCodeInIDE[Ordering[Int]]
        o.compare(1, 2) ==> -1
      }

      test("withGivenASTInIDE summons implicit") {
        val o: Ordering[Int] = Debug.withGivenASTInIDE[Ordering[Int]]
        o.compare(1, 2) ==> -1
      }

      test("withGivenTypeInIDE summons and reports type") {
        val o: Ordering[Int] = Debug.withGivenTypeInIDE[Ordering[Int]]
        o.compare(1, 2) ==> -1
      }
    }

    group("extension methods") {
      import DebugOps.*

      test("withFinalCodeInIDE extension passes through value") {
        "hello".withFinalCodeInIDE ==> "hello"
      }

      test("withFinalASTInIDE extension passes through value") {
        42.withFinalASTInIDE ==> 42
      }

      test("withInferredTypeInIDE extension passes through value") {
        "test".withInferredTypeInIDE ==> "test"
      }

      test("withInferredTypeInIDE extension with complex types") {
        List(1, 2, 3).withInferredTypeInIDE ==> List(1, 2, 3)
      }
    }

    group("evaluation of the wrapped expression") {

      test("object methods evaluate the expression exactly once") {
        var evaluated = 0
        Debug.withFinalCodeInIDE { evaluated += 1; evaluated } ==> 1
        Debug.withFinalASTInIDE { evaluated += 1; evaluated } ==> 2
        Debug.withInferredTypeInIDE { evaluated += 1; evaluated } ==> 3
        evaluated ==> 3
      }

      test("extension methods evaluate a non-trivial receiver exactly once") {
        import DebugOps.*
        var evaluated = 0
        def next(): Int = { evaluated += 1; evaluated }
        (next() + 10).withFinalCodeInIDE ==> 11
        (next() + 10).withFinalASTInIDE ==> 12
        List(next()).withInferredTypeInIDE ==> List(3)
        evaluated ==> 3
      }
    }

    group("missing implicit") {

      test("withGiven*InIDE fail the compilation when there is no implicit for the type") {
        compileErrors("hearth.debug.Debug.withGivenCodeInIDE[hearth.examples.miscedge.NoInstanceAvailable]")
          .check("NoInstanceAvailable")
        compileErrors("hearth.debug.Debug.withGivenASTInIDE[hearth.examples.miscedge.NoInstanceAvailable]")
          .check("NoInstanceAvailable")
        compileErrors("hearth.debug.Debug.withGivenTypeInIDE[hearth.examples.miscedge.NoInstanceAvailable]")
          .check("NoInstanceAvailable")
      }
    }
  }
}
