package hearth
package treeprinter

final class ShowCodePrettyScala3Spec extends MacroSuite {

  test("Printer FormattedTreeStructureAnsi should support all known trees on Scala 3") {

    // format: off
    @scala.annotation.nowarn
    val printed = ShowCodePrettyFixtures.testExprPrettyAST {
      // Imports of various shapes
      import scala.annotation.tailrec
      import scala.util.{Try as ScalaTry, Success, Failure}
      import scala.collection.mutable.{Map as MutableMap, _}
      import scala.math.{Pi => π}
      import util.{ChainingOps as ChainingMethods}
      import util.{given}              // import givens
      import scala.concurrent.duration.* // wildcard

      // Top-level vals/defs allowed in Scala 3
      val topLevelVal: String = "hello"
      var topLevelVar: Int = 0
      lazy val topLevelLazy: Long = 9999999999L

      // type aliases
      type Id[A] = A
      type StrOrInt = String | Int
      type BothNum[A] = A & Matchable // silly intersection type

      // match type
      type ElemOf[X] = X match {
        case List[t] => t
        case Array[t] => t
        case _ => X
      }

      // given + using + summon
      given givenInt: Int = 123

      given givenOrdering[A]: Ordering[Option[A]] with {
        def compare(x: Option[A], y: Option[A]): Int =
          (x, y) match {
            case (None, None)       => 0
            case (None, Some(_))    => -1
            case (Some(_), None)    => 1
            case (Some(_), Some(_)) => 0 // pretend
          }
      }

      // given alias
      given strShow: Conversion[Int, String] = _.toString

      // simple object
      object Globals {
        val earthGravity: Double = 9.81
        private var internalCounter: Int = 0
        def bump(): Int = {
          internalCounter += 1
          internalCounter
        }

        // export example
        private val numbers = List(1,2,3)
        export numbers.head as firstNumber
      }

      // trait with self-type and abstract/concrete members
      trait Loggable {
        def log(msg: String): Unit
        val loggerName: String
      }

      trait Closeable {
        def close(): Unit = ()
      }

      trait NeedsLogSelf {
        self: Loggable =>
        def info(m: String): Unit = log(s"[info][$loggerName] $m")
      }

      // open so it can be extended outside
      open class BaseService(protected val svcName: String) extends Loggable, Closeable {
        // constructor params become fields
        private var state: Int = 0
        val publicVal: String = "base"
        protected[this] var localOnly: Boolean = false

        // abstract from Loggable
        def log(msg: String): Unit =
          println(s"[$svcName] $msg")

        // regular def
        def inc(by: Int = 1): Int = {
          state += by
          state
        }

        // by-name param, varargs, using param, inline
        inline def computeAll(xs: => Int*)(using scale: Int): Int = {
          var acc = 0
          for x <- xs do
            acc += x * scale
          acc
        }

        // while / if / return / try-catch-finally / throw
        def risky(op: () => Int): Int = {
          var done = false
          var result = 0
          while !done do
            try {
              val r = op()
              if r < 0 then
                throw new IllegalArgumentException("negative!")
              result = r
              done = true
            }
            catch {
              case _: IllegalArgumentException =>
                return -1
            }
            finally
              () // no-op
          result
        }

        // inline match
        inline def classifyInline(inline x: Int): String =
          inline x match {
            case 0 => "zero"
            case _ if x < 0 => "neg"
            case _ => "pos"
          }

        // opaque type inside class
        opaque type Secret = String
        object Secret {
          def apply(s: String): Secret = s
          extension (s: Secret) def reveal: String = s
        }

        // export members of nested object
        private object mathStuff {
          val meaning = 42
          def twice(i: Int) = i * 2
        }
        export mathStuff.*

        // auxiliary constructor
        def this() = {
          this("default-service")
        }
      }

      // Companion object
      object BaseService {
        // extension method
        extension (svc: BaseService) {
          def fullName: String = s"Service(${svc.svcName})"
        }

        // given inside companion
        given defaultScale: Int = 10
      }

      // case class with derives
      case class User(id: Int, name: String) derives CanEqual {
        def greet(prefix: String = "hi"): String =
          s"$prefix, $name"
      }

      // enum with cases (no params, params, and case object style) and methods
      enum Command derives CanEqual {
        case Quit
        case Speak(msg: String)
        case Move(x: Int, y: Int)

        def isMovement: Boolean = this match {
          case Move(_, _) => true
          case _          => false
        }
      }

      // case object
      case object SingletonTag

      // trait to show override / final / abstract vals
      sealed trait Shape {
        def area: Double
      }

      final case class Circle(r: Double) extends Shape {
        def area: Double = math.Pi * r * r
      }

      final case class Rect(w: Double, h: Double) extends Shape {
        def area: Double = w * h
      }

      // class with type params, context bound, using param
      class SortedBuffer[A](init: List[A])(using ord: Ordering[A]) {
        private var data: List[A] = init.sorted
        def insert(a: A): Unit =
          data = (a :: data).sorted
        def all: List[A] = data
      }

      // inline def + recursion + @tailrec
      inline def inlineMax(a: Int, b: Int): Int =
        if a >= b then a else b

      def factorial(n: Int): Int = {
        @tailrec
        def go(cur: Int, acc: Int): Int =
          if cur <= 1 then acc else go(cur - 1, acc * cur)
        go(n, 1)
      }

      // For-comprehension with generator, guard, value binding, yield
      def collectPositives(xs: List[Int]): List[Int] =
        for {
          x <- xs
          if x > 0
          y = x * 2
        }
        yield y

      // Pattern matching showcase
      def describeShape(s: Shape): String =
        s match {
          case c @ Circle(r) if r > 10 =>
            s"Big circle radius=${c.r}"
          case Circle(r) =>
            s"Circle radius=$r"
          case Rect(w, h) if w == h =>
            s"Square side=$w"
          case Rect(w, h) =>
            s"Rect ${w}x${h}"
          case _ =>
            "unknown shape"
        }

      // Typed pattern, sequence pattern, wildcard, tuple destructuring
      def sumFirstTwo(xs: List[Int]): Int =
        xs match {
          case a :: b :: _ =>
            val (x, y) = (a, b) // tuple + pattern val
            x + y
          case _ =>
            0
        }

      // using clause locally + summon
      def scaledSum(nums: Int*)(using scale: Int): Int = {
        val s = summon[Int] // summon given Int
        nums.foldLeft(0)((acc, n) => acc + n * s)
      }

      // lambdas
      val adder: (Int, Int) => Int = (a, b) => a + b

      // multiline / interpolated / numeric / char / null / unit literals
      val literalsDemo: Any = {
        val multi =
          """This is
            |a multiline
            |string.
            |""".stripMargin
        val interp = s"Pi is about ${π}"
        val tuple = (1, "x", true, 2.5f, 3.14d, 'c')
        val arr = Array(1,2,3)
        val list = List(1,2,3)
        val opt: Option[String] = None
        val n: String | Null = null
        ()
        (multi, interp, tuple, arr, list, opt, n)
      }

      def larger[A: Ordering](a: A, b: A): A = {
        import Ordering.Implicits.infixOrderingOps
        if a > b then a else b
      }

      // example class using many features together
      class AppService(name: String, private var counter: Int)(using Ordering[Int])
          extends BaseService(name), NeedsLogSelf {

        override val loggerName: String = s"AppService:$name"

        def tick(): Int = {
          counter = inc() // from BaseService
          counter
        }

        def demoAll(): Unit = {
          info(s"tick -> ${tick()}")
          log(s"classifyInline(0) = ${classifyInline(0)}")
          log(s"bump global = ${Globals.bump()} firstNumber=${Globals.firstNumber}")
          val u = User(1, "Scala")
          log(u.greet())
          val cmd: Command = Command.Move(1,2)
          cmd match {
            case Command.Quit          => log("quit")
            case Command.Speak(msg)    => log(s"say $msg")
            case m @ Command.Move(x,y) => log(s"move $x,$y; isMove=${m.isMovement}")
          }

          val buf = SortedBuffer[Int](List(3,1,2))
          buf.insert(0)
          log(s"sorted buffer: ${buf.all}")

          val pos = collectPositives(List(-1,0,1,2,3))
          log(s"positives doubled: $pos")

          val sum = scaledSum(1,2,3)
          log(s"scaledSum = $sum")

          // for/while/if already shown; try/catch shown in risky
          val risk = risky(() => 5)
          log(s"risky result: $risk")

          val maxv = inlineMax(10, 20)
          log(s"inlineMax = $maxv")

          val areaMsg = describeShape(Circle(2))
          log(areaMsg)

          val s2 = sumFirstTwo(List(10,20,30))
          log(s"sumFirstTwo = $s2")
        }
      }

          // inline if demonstrated indirectly in classifyInline

      // main entry
      object Main {
        def main(args: Array[String]): Unit = {
          given Ordering[Int] = Ordering.Int // local given
          val svc = new AppService("demo", 0)
          svc.demoAll()
          println(svc.fullName) // extension from companion
          println(s"factorial(5) = ${factorial(5)}")
          println(topLevelVal)
          println(topLevelLazy)
          println(literalsDemo)
        }
      }
    }
    // format: on

    // Uncomment and print to preview results.
    // printed.split("\n").foreach(println)

    // Exact code might change between Scala versions, and we don't care about particular output - only that whole tree was handled.
    assert(printed.nonEmpty)
  }

  test("Printer FormattedTreeStructure should support all known trees on Scala 3") {

				// format: off
    @scala.annotation.nowarn
    val printed = ShowCodePrettyFixtures.testExprPlainAST {
      // Imports of various shapes
      import scala.annotation.tailrec
      import scala.util.{Try as ScalaTry, Success, Failure}
      import scala.collection.mutable.{Map as MutableMap, _}
      import scala.math.{Pi => π}
      import util.{ChainingOps as ChainingMethods}
      import util.{given}              // import givens
      import scala.concurrent.duration.* // wildcard

      // Top-level vals/defs allowed in Scala 3
      val topLevelVal: String = "hello"
      var topLevelVar: Int = 0
      lazy val topLevelLazy: Long = 9999999999L

      // type aliases
      type Id[A] = A
      type StrOrInt = String | Int
      type BothNum[A] = A & Matchable // silly intersection type

      // match type
      type ElemOf[X] = X match {
        case List[t] => t
        case Array[t] => t
        case _ => X
      }

      // given + using + summon
      given givenInt: Int = 123

      given givenOrdering[A]: Ordering[Option[A]] with {
        def compare(x: Option[A], y: Option[A]): Int =
          (x, y) match {
            case (None, None)       => 0
            case (None, Some(_))    => -1
            case (Some(_), None)    => 1
            case (Some(_), Some(_)) => 0 // pretend
          }
      }

      // given alias
      given strShow: Conversion[Int, String] = _.toString

      // simple object
      object Globals {
        val earthGravity: Double = 9.81
        private var internalCounter: Int = 0
        def bump(): Int = {
          internalCounter += 1
          internalCounter
        }

        // export example
        private val numbers = List(1,2,3)
        export numbers.head as firstNumber
      }

      // trait with self-type and abstract/concrete members
      trait Loggable {
        def log(msg: String): Unit
        val loggerName: String
      }

      trait Closeable {
        def close(): Unit = ()
      }

      trait NeedsLogSelf {
        self: Loggable =>
        def info(m: String): Unit = log(s"[info][$loggerName] $m")
      }

      // open so it can be extended outside
      open class BaseService(protected val svcName: String) extends Loggable, Closeable {
        // constructor params become fields
        private var state: Int = 0
        val publicVal: String = "base"
        protected[this] var localOnly: Boolean = false

        // abstract from Loggable
        def log(msg: String): Unit =
          println(s"[$svcName] $msg")

        // regular def
        def inc(by: Int = 1): Int = {
          state += by
          state
        }

        // by-name param, varargs, using param, inline
        inline def computeAll(xs: => Int*)(using scale: Int): Int = {
          var acc = 0
          for x <- xs do
            acc += x * scale
          acc
        }

        // while / if / return / try-catch-finally / throw
        def risky(op: () => Int): Int = {
          var done = false
          var result = 0
          while !done do
            try {
              val r = op()
              if r < 0 then
                throw new IllegalArgumentException("negative!")
              result = r
              done = true
            }
            catch {
              case _: IllegalArgumentException =>
                return -1
            }
            finally
              () // no-op
          result
        }

        // inline match
        inline def classifyInline(inline x: Int): String =
          inline x match {
            case 0 => "zero"
            case _ if x < 0 => "neg"
            case _ => "pos"
          }

        // opaque type inside class
        opaque type Secret = String
        object Secret {
          def apply(s: String): Secret = s
          extension (s: Secret) def reveal: String = s
        }

        // export members of nested object
        private object mathStuff {
          val meaning = 42
          def twice(i: Int) = i * 2
        }
        export mathStuff.*

        // auxiliary constructor
        def this() = {
          this("default-service")
        }
      }

      // Companion object
      object BaseService {
        // extension method
        extension (svc: BaseService) {
          def fullName: String = s"Service(${svc.svcName})"
        }

        // given inside companion
        given defaultScale: Int = 10
      }

      // case class with derives
      case class User(id: Int, name: String) derives CanEqual {
        def greet(prefix: String = "hi"): String =
          s"$prefix, $name"
      }

      // enum with cases (no params, params, and case object style) and methods
      enum Command derives CanEqual {
        case Quit
        case Speak(msg: String)
        case Move(x: Int, y: Int)

        def isMovement: Boolean = this match {
          case Move(_, _) => true
          case _          => false
        }
      }

      // case object
      case object SingletonTag

      // trait to show override / final / abstract vals
      sealed trait Shape {
        def area: Double
      }

      final case class Circle(r: Double) extends Shape {
        def area: Double = math.Pi * r * r
      }

      final case class Rect(w: Double, h: Double) extends Shape {
        def area: Double = w * h
      }

      // class with type params, context bound, using param
      class SortedBuffer[A](init: List[A])(using ord: Ordering[A]) {
        private var data: List[A] = init.sorted
        def insert(a: A): Unit =
          data = (a :: data).sorted
        def all: List[A] = data
      }

      // inline def + recursion + @tailrec
      inline def inlineMax(a: Int, b: Int): Int =
        if a >= b then a else b

      def factorial(n: Int): Int = {
        @tailrec
        def go(cur: Int, acc: Int): Int =
          if cur <= 1 then acc else go(cur - 1, acc * cur)
        go(n, 1)
      }

      // For-comprehension with generator, guard, value binding, yield
      def collectPositives(xs: List[Int]): List[Int] =
        for {
          x <- xs
          if x > 0
          y = x * 2
        }
        yield y

      // Pattern matching showcase
      def describeShape(s: Shape): String =
        s match {
          case c @ Circle(r) if r > 10 =>
            s"Big circle radius=${c.r}"
          case Circle(r) =>
            s"Circle radius=$r"
          case Rect(w, h) if w == h =>
            s"Square side=$w"
          case Rect(w, h) =>
            s"Rect ${w}x${h}"
          case _ =>
            "unknown shape"
        }

      // Typed pattern, sequence pattern, wildcard, tuple destructuring
      def sumFirstTwo(xs: List[Int]): Int =
        xs match {
          case a :: b :: _ =>
            val (x, y) = (a, b) // tuple + pattern val
            x + y
          case _ =>
            0
        }

      // using clause locally + summon
      def scaledSum(nums: Int*)(using scale: Int): Int = {
        val s = summon[Int] // summon given Int
        nums.foldLeft(0)((acc, n) => acc + n * s)
      }

      // lambdas
      val adder: (Int, Int) => Int = (a, b) => a + b

      // multiline / interpolated / numeric / char / null / unit literals
      val literalsDemo: Any = {
        val multi =
          """This is
            |a multiline
            |string.
            |""".stripMargin
        val interp = s"Pi is about ${π}"
        val tuple = (1, "x", true, 2.5f, 3.14d, 'c')
        val arr = Array(1,2,3)
        val list = List(1,2,3)
        val opt: Option[String] = None
        val n: String | Null = null
        ()
        (multi, interp, tuple, arr, list, opt, n)
      }

      def larger[A: Ordering](a: A, b: A): A = {
        import Ordering.Implicits.infixOrderingOps
        if a > b then a else b
      }

      // example class using many features together
      class AppService(name: String, private var counter: Int)(using Ordering[Int])
          extends BaseService(name), NeedsLogSelf {

        override val loggerName: String = s"AppService:$name"

        def tick(): Int = {
          counter = inc() // from BaseService
          counter
        }

        def demoAll(): Unit = {
          info(s"tick -> ${tick()}")
          log(s"classifyInline(0) = ${classifyInline(0)}")
          log(s"bump global = ${Globals.bump()} firstNumber=${Globals.firstNumber}")
          val u = User(1, "Scala")
          log(u.greet())
          val cmd: Command = Command.Move(1,2)
          cmd match {
            case Command.Quit          => log("quit")
            case Command.Speak(msg)    => log(s"say $msg")
            case m @ Command.Move(x,y) => log(s"move $x,$y; isMove=${m.isMovement}")
          }

          val buf = SortedBuffer[Int](List(3,1,2))
          buf.insert(0)
          log(s"sorted buffer: ${buf.all}")

          val pos = collectPositives(List(-1,0,1,2,3))
          log(s"positives doubled: $pos")

          val sum = scaledSum(1,2,3)
          log(s"scaledSum = $sum")

          // for/while/if already shown; try/catch shown in risky
          val risk = risky(() => 5)
          log(s"risky result: $risk")

          val maxv = inlineMax(10, 20)
          log(s"inlineMax = $maxv")

          val areaMsg = describeShape(Circle(2))
          log(areaMsg)

          val s2 = sumFirstTwo(List(10,20,30))
          log(s"sumFirstTwo = $s2")
        }
      }

          // inline if demonstrated indirectly in classifyInline

      // main entry
      object Main {
        def main(args: Array[String]): Unit = {
          given Ordering[Int] = Ordering.Int // local given
          val svc = new AppService("demo", 0)
          svc.demoAll()
          println(svc.fullName) // extension from companion
          println(s"factorial(5) = ${factorial(5)}")
          println(topLevelVal)
          println(topLevelLazy)
          println(literalsDemo)
        }
      }
    }
    // format: on

    // Uncomment and print to preview results.
    // printed.split("\n").foreach(println)

    // Exact code might change between Scala versions, and we don't care about particular output - only that whole tree was handled.
    assert(printed.nonEmpty)
  }

  // The monster fixtures above exercise structural breadth but never feed the printer a literal whose characters
  // require escaping. These focused cases pin the literal-escaping branches (escapedStringValue / escapedChar /
  // requiresFormat / quadNibble) that are otherwise cold.

  test("AST printer escapes control/special characters inside String literals on Scala 3") {
    @scala.annotation.nowarn
    val printed = ShowCodePrettyFixtures.testExprPlainAST {
      "tab\tnewline\nquote\"backslash\\carriage\rbackspace\bformnull "
    }
    // Escaped forms must appear verbatim in the rendered AST literal.
    assert(printed.contains("\\t"), s"expected escaped tab in: $printed")
    assert(printed.contains("\\n"), s"expected escaped newline in: $printed")
    assert(printed.contains("\\\""), s"expected escaped quote in: $printed")
    assert(printed.contains("\\\\"), s"expected escaped backslash in: $printed")
    assert(printed.contains("\\r"), s"expected escaped carriage return in: $printed")
    assert(printed.contains("\\b"), s"expected escaped backspace in: $printed")
    // Control chars without a short escape go through the \\uXXXX quad-nibble path.
    assert(printed.contains("\\u0000"), s"expected unicode-escaped NUL in: $printed")
    assert(printed.contains("StringConstant"), s"expected StringConstant node in: $printed")
  }

  test("AST printer escapes a Char literal that needs escaping on Scala 3") {
    @scala.annotation.nowarn
    val printed = ShowCodePrettyFixtures.testExprPlainAST {
      '\n'
    }
    assert(printed.contains("CharConstant"), s"expected CharConstant node in: $printed")
    assert(printed.contains("'\\n'"), s"expected escaped char literal in: $printed")
  }

  test("AST printer renders a plain (non-escaped) Char literal on Scala 3") {
    @scala.annotation.nowarn
    val printed = ShowCodePrettyFixtures.testExprPlainAST {
      'c'
    }
    assert(printed.contains("CharConstant"), s"expected CharConstant node in: $printed")
    assert(printed.contains("'c'"), s"expected unescaped char literal in: $printed")
  }

  test("AST printer renders a Long literal with the L suffix on Scala 3") {
    @scala.annotation.nowarn
    val printed = ShowCodePrettyFixtures.testExprPlainAST {
      9999999999L
    }
    assert(printed.contains("LongConstant"), s"expected LongConstant node in: $printed")
    assert(printed.contains("9999999999L"), s"expected L-suffixed long literal in: $printed")
  }

  test("AST printer renders a Float literal with the f suffix on Scala 3") {
    @scala.annotation.nowarn
    val printed = ShowCodePrettyFixtures.testExprPlainAST {
      3.14f
    }
    assert(printed.contains("FloatConstant"), s"expected FloatConstant node in: $printed")
    assert(printed.contains("3.14f"), s"expected f-suffixed float literal in: $printed")
  }

  test("AST printer renders Boolean / Int / Double / Unit / null literals on Scala 3") {
    @scala.annotation.nowarn
    val printedBool = ShowCodePrettyFixtures.testExprPlainAST(true)
    assert(printedBool.contains("BooleanConstant"), s"expected BooleanConstant in: $printedBool")
    assert(printedBool.contains("true"), s"expected true literal in: $printedBool")

    @scala.annotation.nowarn
    val printedInt = ShowCodePrettyFixtures.testExprPlainAST(42)
    assert(printedInt.contains("IntConstant"), s"expected IntConstant in: $printedInt")
    assert(printedInt.contains("42"), s"expected 42 literal in: $printedInt")

    @scala.annotation.nowarn
    val printedDouble = ShowCodePrettyFixtures.testExprPlainAST(2.5d)
    assert(printedDouble.contains("DoubleConstant"), s"expected DoubleConstant in: $printedDouble")

    @scala.annotation.nowarn
    val printedUnit = ShowCodePrettyFixtures.testExprPlainAST(())
    assert(printedUnit.contains("UnitConstant"), s"expected UnitConstant in: $printedUnit")

    @scala.annotation.nowarn
    val printedNull = ShowCodePrettyFixtures.testExprPlainAST((null: String))
    assert(printedNull.nonEmpty, s"expected non-empty render for null in: $printedNull")
  }

  test("ANSI AST printer produces colored output that matches plain after stripping ANSI on Scala 3") {
    // Use an expression without synthetic lambda params: two separate macro expansions would otherwise pick different
    // counter-based names (`_$3` vs `_$4`), making the comparison flaky.
    @scala.annotation.nowarn
    val ansi = ShowCodePrettyFixtures.testExprPrettyAST {
      val xs = List(1, 2, 3)
      xs
    }
    @scala.annotation.nowarn
    val plain = ShowCodePrettyFixtures.testExprPlainAST {
      val xs = List(1, 2, 3)
      xs
    }
    // ANSI output carries escape codes; stripping them must yield exactly the plain rendering.
    assert(ansi.contains("["), s"expected ANSI escapes in colored output: $ansi")
    ansi.stripANSI <==> plain
  }
}
