package hearth
package source

/** Macro implementation is in [[SourceMacros]] */
final class SourceSpec extends MacroSuite {

  group("source.MethodName") {

    test("should resolve <init> if enclosing method is a constructor") {
      MethodName.derived ==> MethodName.wrap("<init>")
    }

    test("should resolve enclosing method if enclosing method is an actual method") {
      def exampleMethod(): Unit =
        MethodName.derived ==> MethodName.wrap("exampleMethod")
      exampleMethod()
    }
  }

  group("source.Line") {

    test("should resolve the line number") {
      Line.derived ==> Line.wrap(23)
    }
  }

  group("source.File") {

    test("should resolve the file") {
      val file = {
        // We only want to check if it's the whole path, but whatever is before "hearth-tests" is depending on user's environment.
        val value = File.derived
        value.drop(value.indexOf("hearth-tests"))
      }
      file ==> "hearth-tests/src/test/scala/hearth/source/SourceSpec.scala"
    }
  }

  group("source.FileName") {

    test("should resolve the file name") {
      FileName.derived ==> FileName.wrap("SourceSpec.scala")
    }
  }

  group("source.Location") {

    test("should resolve the location") {
      val location = Location.derived

      location.line ==> Line.wrap(49)

      val file = {
        val value = location.file.toString
        value.drop(value.indexOf("hearth-tests"))
      }
      file ==> "hearth-tests/src/test/scala/hearth/source/SourceSpec.scala"

      location.fileName ==> FileName.wrap("SourceSpec.scala")

      val locationString = {
        val value = location.toString
        value.drop(value.indexOf("hearth-tests"))
      }
      locationString ==> "hearth-tests/src/test/scala/hearth/source/SourceSpec.scala:49"
    }
  }

  group("source.Text") {

    test("should capture both the value and the source text of an expression") {
      val a = 1
      val b = 2
      val captured = Text.generate(a + b)
      captured.value ==> 3
      captured.source ==> "a + b"
    }

    test("should capture a method-call expression's source text") {
      val captured = Text.generate("abc".length)
      captured.value ==> 3
      captured.source ==> "\"abc\".length"
    }
  }

  group("source utilities summoned implicitly") {

    def implicitMethodName(): MethodName = implicitly[MethodName]
    def capturedByCallee(implicit name: MethodName, line: Line, location: Location): (MethodName, Line, Location) =
      (name, line, location)
    def caller(): (MethodName, Line, Location) = capturedByCallee

    test("MethodName should be the name of the method summoning it") {
      implicitMethodName() ==> MethodName.wrap("implicitMethodName")
    }

    test("implicit parameters should be materialized at the call site") {
      val (name, line, location) = caller(); val callLine = Line.derived
      name ==> MethodName.wrap("caller")
      // `caller` is defined a few lines above this test, not on the line of this call
      assert(line != callLine, s"Expected the line of the call inside caller, got $line")
      location.line ==> line
      location.fileName ==> FileName.wrap("SourceSpec.scala")
    }
  }

  group("source.MethodName edge cases") {

    def insideLambda(): MethodName = List(1).map(_ => MethodName.derived).head
    def outerMethod(): MethodName = {
      def innerMethod(): MethodName = MethodName.derived
      innerMethod()
    }

    test("should skip a lambda and resolve the enclosing method") {
      insideLambda() ==> MethodName.wrap("insideLambda")
    }

    test("should resolve the closest local method") {
      outerMethod() ==> MethodName.wrap("innerMethod")
    }
  }

  group("source.Text edge cases") {

    def check(condition: Text[Boolean]): Text[Boolean] = condition

    test("should be created by the implicit conversion from an argument expression") {
      val a = 1
      val b = 2
      val captured = check(a + b == 3)
      captured.value ==> true
      captured.source ==> "a + b == 3"
    }

    test("toString should render the source and the value") {
      val a = 1
      val b = 2
      Text.generate(a + b).toString ==> "a + b = 3"
    }
  }
}
