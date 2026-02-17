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
  }
}
