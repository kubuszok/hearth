package hearth
package source

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
      Line.derived ==> Line.wrap(22)
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

      location.line ==> Line.wrap(48)

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
      locationString ==> "hearth-tests/src/test/scala/hearth/source/SourceSpec.scala:48"
    }
  }
}
