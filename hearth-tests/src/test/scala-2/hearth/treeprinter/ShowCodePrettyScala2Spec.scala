package hearth
package treeprinter

final class ShowCodePrettyScala2Spec extends MacroSuite {

  test("showRawPretty(..., SyntaxHighlight.ANSI) should support all known trees on Scala 2") {

    // format: off
    @scala.annotation.nowarn
    val printed = ShowCodePrettyFixtures.testExprPrettyAST {
      // Imports of various shapes
      import scala.annotation.tailrec
      import scala.util.{Try => ScalaTry, Success, Failure}
      import scala.collection.mutable.{Map => MutableMap, ListBuffer}
      import scala.math.{Pi => π}
      import util.{ChainingOps as ChainingMethods}
      import scala.concurrent.duration._ // wildcard import of vals/types
      import scala.language.implicitConversions // to allow implicit conv without warning

      // Top-level vals/vars/defs aren't allowed in Scala 2.13, so we wrap everything in an object.
      // We'll use DemoApp as our "namespace".
      object DemoApp {

        // basic values
        val topLevelVal: String = "hello"
        var topLevelVar: Int = 0
        lazy val topLevelLazy: Long = 9999999999L

        // type aliases
        type Id[A] = A
        type StringList = List[String]

        // existential type
        type HasElem = {
          def head: Any
        }

        // structural type + compound type example
        type ClosableLogger = Logger with Closeable {
          def flush(): Unit
        }

        // implicit values, defs, classes, conversions
        implicit val implicitInt: Int = 123

        implicit def intToString(i: Int): String = i.toString

        implicit class RichInt(private val i: Int) {
          def squared: Int = i * i
        }

        // Trait with abstract/concrete + self type
        trait Logger {
          def log(msg: String): Unit
          val loggerName: String
        }

        trait Closeable {
          def close(): Unit = ()
        }

        trait NeedsLogSelf { self: Logger =>
          def info(m: String): Unit = log("[info][" + loggerName + "] " + m)
        }

        // Base service class
        abstract class BaseService(protected val svcName: String)
            extends Logger with Closeable {

          // constructor params become fields, we can also declare our own
          private var state: Int = 0
          val publicVal: String = "base"
          protected[this] var localOnly: Boolean = false

          // abstract from Logger
          def log(msg: String): Unit = {
            println("[" + svcName + "] " + msg)
          }

          // regular def with default param
          def inc(by: Int = 1): Int = {
            state += by
            state
          }

          // varargs, by-name param, implicit param
          def computeAll(xs: Int*)(onEmpty: => Int)(implicit scale: Int): Int = {
            if (xs.isEmpty) onEmpty else {
              var acc = 0
              for (x <- xs) {
                acc += x * scale
              }
              acc
            }
          }

          // while / do-while / if / return / try-catch-finally / throw
          def risky(op: () => Int): Int = {
            var done = false
            var result = 0
            while (!done) {
              try {
                val r = op()
                if (r < 0) {
                  throw new IllegalArgumentException("negative!")
                }
                result = r
                done = true
              } catch {
                case _: IllegalArgumentException =>
                  return -1
              } finally {
                () // no-op
              }
            }
            // demo do-while
            var x = 0
            do {
              x += 1
            } while (x < 1)
            result
          }

          // simple method using match
          def classify(x: Int): String = x match {
            case 0                       => "zero"
            case n if n < 0              => "neg"
            case _                       => "pos"
          }

          // auxiliary constructor
          def this() = this("default-service")
        }

        // Companion object for BaseService
        object BaseService {
          // extension-like utility
          implicit class ServiceOps(val svc: BaseService) {
            def fullName: String = "Service(" + svc.svcName + ")"
          }

          // factory
          def make(name: String): BaseService =
            new ConcreteService(name, 0)
        }

        // concrete class with override, self-type mixin, additional fields
        class ConcreteService(name: String, private var counter: Int)
            extends BaseService(name) with NeedsLogSelf {

          override val loggerName: String = "ConcreteService:" + name

          def tick(): Int = {
            counter = inc() // from BaseService
            counter
          }

          def demoAll(): Unit = {
            info("tick -> " + tick())
            log("classify(0) = " + classify(0))

            val u = User(1, "Scala")
            log(u.greet())

            val cmd: Command = Move(1, 2)
            cmd match {
              case Quit                => log("quit")
              case Speak(msg)          => log("say " + msg)
              case m @ Move(x, y)      => log("move " + x + "," + y + "; isMove=" + m.isMovement)
            }

            val buf = new SortedBuffer[Int](List(3, 1, 2))
            buf.insert(0)
            log("sorted buffer: " + buf.all)

            val pos = collectPositives(List(-1, 0, 1, 2, 3))
            log("positives doubled: " + pos)

            val sum = scaledSum(1, 2, 3)
            log("scaledSum = " + sum)

            val risk = risky(() => 5)
            log("risky result: " + risk)

            val maxv = max2(10, 20)
            log("max2 = " + maxv)

            val areaMsg = describeShape(Circle(2))
            log(areaMsg)

            val s2 = sumFirstTwo(List(10, 20, 30))
            log("sumFirstTwo = " + s2)

            // implicit class RichInt
            log("7.squared = " + 7.squared)

            // structural type usage
            val hasElem: HasElem = List(1,2,3)
            log("hasElem.head = " + hasElem.head)

            // XML literal
            val xmlDemo = <msg><body>{"Hello " + name}</body></msg>
            log("xmlDemo = " + xmlDemo.toString)
          }
        }

        // case class
        case class User(id: Int, name: String) {
          def greet(prefix: String = "hi"): String =
            prefix + ", " + name
        }

        // "enum-like" ADT in Scala 2.13: sealed trait + case objects / case classes
        sealed trait Command {
          def isMovement: Boolean = this match {
            case Move(_, _) => true
            case _          => false
          }
        }
        case object Quit extends Command
        case class Speak(msg: String) extends Command
        case class Move(x: Int, y: Int) extends Command

        // sealed hierarchy for pattern match
        sealed trait Shape {
          def area: Double
        }

        final case class Circle(r: Double) extends Shape {
          def area: Double = math.Pi * r * r
        }

        final case class Rect(w: Double, h: Double) extends Shape {
          def area: Double = w * h
        }

        // class with type params, context bound, view bound, etc.
        // - context bound [A : Ordering]  expands to (implicit ord: Ordering[A])
        // - view bound [A <% Ordered[A]]  still compiles in 2.13 (deprecated, but legal with -language:implicitConversions)
        //   (no, thet are not working, so I removed it)
        class SortedBuffer[A : Ordering](init: List[A]) {
          private var data: List[A] = init.sorted
          def insert(a: A): Unit = {
            data = (a :: data).sorted
          }
          def all: List[A] = data
        }

        // max2 to show type params + upper/lower bounds
        def max2[A](a: A, b: A)(implicit ord: Ordering[A]): A = {
          if (ord.gteq(a, b)) a else b
        }

        // factorial with @tailrec
        def factorial(n: Int): Int = {
          @tailrec
          def go(cur: Int, acc: Int): Int =
            if (cur <= 1) acc else go(cur - 1, acc * cur)
          go(n, 1)
        }

        // For-comprehension with generator, guard, value binding, yield
        def collectPositives(xs: List[Int]): List[Int] = {
          for {
            x <- xs
            if x > 0
            y = x * 2
          } yield y
        }

        // Pattern matching showcase
        def describeShape(s: Shape): String = s match {
          case c @ Circle(r) if r > 10 =>
            "Big circle radius=" + c.r
          case Circle(r) =>
            "Circle radius=" + r
          case Rect(w, h) if w == h =>
            "Square side=" + w
          case Rect(w, h) =>
            "Rect " + w + "x" + h
          case _ =>
            "unknown shape"
        }

        // Typed pattern, sequence pattern, wildcard, tuple destructuring
        def sumFirstTwo(xs: List[Int]): Int = xs match {
          case a :: b :: _ =>
            val (x, y) = (a, b) // tuple + pattern val
            x + y
          case _ =>
            0
        }

        // implicit param usage (like Scala 3 using/summon)
        def scaledSum(nums: Int*)(implicit scale: Int): Int = {
          nums.foldLeft(0)((acc, n) => acc + n * scale)
        }

        // higher-order function / lambda
        val adder: (Int, Int) => Int = (a, b) => a + b

        // literals demo
        val literalsDemo: Any = {
          val multi =
            """This is
              |a multiline
              |string.
              |""".stripMargin
          val interpS = s"Pi is about $π"
          val interpF = f"Pi ~ $π%.2f"
          val interpRaw = raw"raw\tno-escape"
          val tuple = (1, "x", true, 2.5f, 3.14d, 'c')
          val arr = Array(1,2,3)
          val list = List(1,2,3)
          val opt: Option[String] = None
          val tried: ScalaTry[Int] = ScalaTry(1 / 1)
          // Warning:
          // symbol literal is deprecated; use Symbol("symbolLiteral")
          // that we cannot suppress with @scala.annotation.nowarn
          //val sym = 'symbolLiteral: @scala.annotation.nowarn
          val sym = Symbol("symbolLiteral")
          val n: String = null
          ()
          (multi, interpS, interpF, interpRaw, tuple, arr, list, opt, tried, sym, n)
        }

        // Try/catch with pattern match on Failure
        def safeDivide(a: Int, b: Int): Int = {
          ScalaTry(a / b) match {
            case Success(v) => v
            case Failure(e) => -1
          }
        }

        // main-like entry point
        def main(args: Array[String]): Unit = {

          implicit val scaleImplicit: Int = 10

          val svc = new ConcreteService("demo", 0)
          svc.demoAll()

          println(svc.fullName) // implicit class ServiceOps

          println("factorial(5) = " + factorial(5))
          println(topLevelVal)
          println(topLevelLazy)
          println(literalsDemo)

          println("safeDivide(10,2) = " + safeDivide(10,2))
          println("safeDivide(10,0) = " + safeDivide(10,0))

          // destructuring vals at top level of method
          val (a, b) = (1, "two")
          println("tuple destructure: a=" + a + ", b=" + b)

          // forSome existential usage:
          val someList: List[_ <: AnyRef] forSome { type T <: AnyRef } =
            List("a","b","c")
          println("someList: " + someList)
        }
      }
    }
		// format: on

    // Uncomment and print to preview results.
    // printed.split("\n").foreach(println)

    // Exact code might change between Scala versions, and we don't care about particular output - only that whole tree was handled.
    assert(printed.nonEmpty)
  }

  test("showRawPretty(..., SyntaxHighlight.plain) should support all known trees on Scala 2") {

    // format: off
    @scala.annotation.nowarn
    val printed = ShowCodePrettyFixtures.testExprPlainAST {
      // Imports of various shapes
      import scala.annotation.tailrec
      import scala.util.{Try => ScalaTry, Success, Failure}
      import scala.collection.mutable.{Map => MutableMap, ListBuffer}
      import scala.math.{Pi => π}
      import util.{ChainingOps as ChainingMethods}
      import scala.concurrent.duration._ // wildcard import of vals/types
      import scala.language.implicitConversions // to allow implicit conv without warning

      // Top-level vals/vars/defs aren't allowed in Scala 2.13, so we wrap everything in an object.
      // We'll use DemoApp as our "namespace".
      object DemoApp {

        // basic values
        val topLevelVal: String = "hello"
        var topLevelVar: Int = 0
        lazy val topLevelLazy: Long = 9999999999L

        // type aliases
        type Id[A] = A
        type StringList = List[String]

        // existential type
        type HasElem = {
          def head: Any
        }

        // structural type + compound type example
        type ClosableLogger = Logger with Closeable {
          def flush(): Unit
        }

        // implicit values, defs, classes, conversions
        implicit val implicitInt: Int = 123

        implicit def intToString(i: Int): String = i.toString

        implicit class RichInt(private val i: Int) {
          def squared: Int = i * i
        }

        // Trait with abstract/concrete + self type
        trait Logger {
          def log(msg: String): Unit
          val loggerName: String
        }

        trait Closeable {
          def close(): Unit = ()
        }

        trait NeedsLogSelf { self: Logger =>
          def info(m: String): Unit = log("[info][" + loggerName + "] " + m)
        }

        // Base service class
        abstract class BaseService(protected val svcName: String)
            extends Logger with Closeable {

          // constructor params become fields, we can also declare our own
          private var state: Int = 0
          val publicVal: String = "base"
          protected[this] var localOnly: Boolean = false

          // abstract from Logger
          def log(msg: String): Unit = {
            println("[" + svcName + "] " + msg)
          }

          // regular def with default param
          def inc(by: Int = 1): Int = {
            state += by
            state
          }

          // varargs, by-name param, implicit param
          def computeAll(xs: Int*)(onEmpty: => Int)(implicit scale: Int): Int = {
            if (xs.isEmpty) onEmpty else {
              var acc = 0
              for (x <- xs) {
                acc += x * scale
              }
              acc
            }
          }

          // while / do-while / if / return / try-catch-finally / throw
          def risky(op: () => Int): Int = {
            var done = false
            var result = 0
            while (!done) {
              try {
                val r = op()
                if (r < 0) {
                  throw new IllegalArgumentException("negative!")
                }
                result = r
                done = true
              } catch {
                case _: IllegalArgumentException =>
                  return -1
              } finally {
                () // no-op
              }
            }
            // demo do-while
            var x = 0
            do {
              x += 1
            } while (x < 1)
            result
          }

          // simple method using match
          def classify(x: Int): String = x match {
            case 0                       => "zero"
            case n if n < 0              => "neg"
            case _                       => "pos"
          }

          // auxiliary constructor
          def this() = this("default-service")
        }

        // Companion object for BaseService
        object BaseService {
          // extension-like utility
          implicit class ServiceOps(val svc: BaseService) {
            def fullName: String = "Service(" + svc.svcName + ")"
          }

          // factory
          def make(name: String): BaseService =
            new ConcreteService(name, 0)
        }

        // concrete class with override, self-type mixin, additional fields
        class ConcreteService(name: String, private var counter: Int)
            extends BaseService(name) with NeedsLogSelf {

          override val loggerName: String = "ConcreteService:" + name

          def tick(): Int = {
            counter = inc() // from BaseService
            counter
          }

          def demoAll(): Unit = {
            info("tick -> " + tick())
            log("classify(0) = " + classify(0))

            val u = User(1, "Scala")
            log(u.greet())

            val cmd: Command = Move(1, 2)
            cmd match {
              case Quit                => log("quit")
              case Speak(msg)          => log("say " + msg)
              case m @ Move(x, y)      => log("move " + x + "," + y + "; isMove=" + m.isMovement)
            }

            val buf = new SortedBuffer[Int](List(3, 1, 2))
            buf.insert(0)
            log("sorted buffer: " + buf.all)

            val pos = collectPositives(List(-1, 0, 1, 2, 3))
            log("positives doubled: " + pos)

            val sum = scaledSum(1, 2, 3)
            log("scaledSum = " + sum)

            val risk = risky(() => 5)
            log("risky result: " + risk)

            val maxv = max2(10, 20)
            log("max2 = " + maxv)

            val areaMsg = describeShape(Circle(2))
            log(areaMsg)

            val s2 = sumFirstTwo(List(10, 20, 30))
            log("sumFirstTwo = " + s2)

            // implicit class RichInt
            log("7.squared = " + 7.squared)

            // structural type usage
            val hasElem: HasElem = List(1,2,3)
            log("hasElem.head = " + hasElem.head)

            // XML literal
            val xmlDemo = <msg><body>{"Hello " + name}</body></msg>
            log("xmlDemo = " + xmlDemo.toString)
          }
        }

        // case class
        case class User(id: Int, name: String) {
          def greet(prefix: String = "hi"): String =
            prefix + ", " + name
        }

        // "enum-like" ADT in Scala 2.13: sealed trait + case objects / case classes
        sealed trait Command {
          def isMovement: Boolean = this match {
            case Move(_, _) => true
            case _          => false
          }
        }
        case object Quit extends Command
        case class Speak(msg: String) extends Command
        case class Move(x: Int, y: Int) extends Command

        // sealed hierarchy for pattern match
        sealed trait Shape {
          def area: Double
        }

        final case class Circle(r: Double) extends Shape {
          def area: Double = math.Pi * r * r
        }

        final case class Rect(w: Double, h: Double) extends Shape {
          def area: Double = w * h
        }

        // class with type params, context bound, view bound, etc.
        // - context bound [A : Ordering]  expands to (implicit ord: Ordering[A])
        // - view bound [A <% Ordered[A]]  still compiles in 2.13 (deprecated, but legal with -language:implicitConversions)
        //   (no, thet are not working, so I removed it)
        class SortedBuffer[A : Ordering](init: List[A]) {
          private var data: List[A] = init.sorted
          def insert(a: A): Unit = {
            data = (a :: data).sorted
          }
          def all: List[A] = data
        }

        // max2 to show type params + upper/lower bounds
        def max2[A](a: A, b: A)(implicit ord: Ordering[A]): A = {
          if (ord.gteq(a, b)) a else b
        }

        // factorial with @tailrec
        def factorial(n: Int): Int = {
          @tailrec
          def go(cur: Int, acc: Int): Int =
            if (cur <= 1) acc else go(cur - 1, acc * cur)
          go(n, 1)
        }

        // For-comprehension with generator, guard, value binding, yield
        def collectPositives(xs: List[Int]): List[Int] = {
          for {
            x <- xs
            if x > 0
            y = x * 2
          } yield y
        }

        // Pattern matching showcase
        def describeShape(s: Shape): String = s match {
          case c @ Circle(r) if r > 10 =>
            "Big circle radius=" + c.r
          case Circle(r) =>
            "Circle radius=" + r
          case Rect(w, h) if w == h =>
            "Square side=" + w
          case Rect(w, h) =>
            "Rect " + w + "x" + h
          case _ =>
            "unknown shape"
        }

        // Typed pattern, sequence pattern, wildcard, tuple destructuring
        def sumFirstTwo(xs: List[Int]): Int = xs match {
          case a :: b :: _ =>
            val (x, y) = (a, b) // tuple + pattern val
            x + y
          case _ =>
            0
        }

        // implicit param usage (like Scala 3 using/summon)
        def scaledSum(nums: Int*)(implicit scale: Int): Int = {
          nums.foldLeft(0)((acc, n) => acc + n * scale)
        }

        // higher-order function / lambda
        val adder: (Int, Int) => Int = (a, b) => a + b

        // literals demo
        val literalsDemo: Any = {
          val multi =
            """This is
              |a multiline
              |string.
              |""".stripMargin
          val interpS = s"Pi is about $π"
          val interpF = f"Pi ~ $π%.2f"
          val interpRaw = raw"raw\tno-escape"
          val tuple = (1, "x", true, 2.5f, 3.14d, 'c')
          val arr = Array(1,2,3)
          val list = List(1,2,3)
          val opt: Option[String] = None
          val tried: ScalaTry[Int] = ScalaTry(1 / 1)
          // Warning:
          // symbol literal is deprecated; use Symbol("symbolLiteral")
          // that we cannot suppress with @scala.annotation.nowarn
          //val sym = 'symbolLiteral: @scala.annotation.nowarn
          val sym = Symbol("symbolLiteral")
          val n: String = null
          ()
          (multi, interpS, interpF, interpRaw, tuple, arr, list, opt, tried, sym, n)
        }

        // Try/catch with pattern match on Failure
        def safeDivide(a: Int, b: Int): Int = {
          ScalaTry(a / b) match {
            case Success(v) => v
            case Failure(e) => -1
          }
        }

        // main-like entry point
        def main(args: Array[String]): Unit = {

          implicit val scaleImplicit: Int = 10

          val svc = new ConcreteService("demo", 0)
          svc.demoAll()

          println(svc.fullName) // implicit class ServiceOps

          println("factorial(5) = " + factorial(5))
          println(topLevelVal)
          println(topLevelLazy)
          println(literalsDemo)

          println("safeDivide(10,2) = " + safeDivide(10,2))
          println("safeDivide(10,0) = " + safeDivide(10,0))

          // destructuring vals at top level of method
          val (a, b) = (1, "two")
          println("tuple destructure: a=" + a + ", b=" + b)

          // forSome existential usage:
          val someList: List[_ <: AnyRef] forSome { type T <: AnyRef } =
            List("a","b","c")
          println("someList: " + someList)
        }
      }
    }
		// format: on

    // Uncomment and print to preview results.
    // printed.split("\n").foreach(println)

    // Exact code might change between Scala versions, and we don't care about particular output - only that whole tree was handled.
    assert(printed.nonEmpty)
  }

  test("showCodePretty(..., SyntaxHighlight.ANSI) should support all known trees on Scala 2") {

    // format: off
    @scala.annotation.nowarn
    val printed = ShowCodePrettyFixtures.testExprPrettyPrint {
      // Imports of various shapes
      import scala.annotation.tailrec
      import scala.util.{Try => ScalaTry, Success, Failure}
      import scala.collection.mutable.{Map => MutableMap, ListBuffer}
      import scala.math.{Pi => π}
      import util.{ChainingOps as ChainingMethods}
      import scala.concurrent.duration._ // wildcard import of vals/types
      import scala.language.implicitConversions // to allow implicit conv without warning

      // Top-level vals/vars/defs aren't allowed in Scala 2.13, so we wrap everything in an object.
      // We'll use DemoApp as our "namespace".
      object DemoApp {

        // basic values
        val topLevelVal: String = "hello"
        var topLevelVar: Int = 0
        lazy val topLevelLazy: Long = 9999999999L

        // type aliases
        type Id[A] = A
        type StringList = List[String]

        // existential type
        type HasElem = {
          def head: Any
        }

        // structural type + compound type example
        type ClosableLogger = Logger with Closeable {
          def flush(): Unit
        }

        // implicit values, defs, classes, conversions
        implicit val implicitInt: Int = 123

        implicit def intToString(i: Int): String = i.toString

        implicit class RichInt(private val i: Int) {
          def squared: Int = i * i
        }

        // Trait with abstract/concrete + self type
        trait Logger {
          def log(msg: String): Unit
          val loggerName: String
        }

        trait Closeable {
          def close(): Unit = ()
        }

        trait NeedsLogSelf { self: Logger =>
          def info(m: String): Unit = log("[info][" + loggerName + "] " + m)
        }

        // Base service class
        abstract class BaseService(protected val svcName: String)
            extends Logger with Closeable {

          // constructor params become fields, we can also declare our own
          private var state: Int = 0
          val publicVal: String = "base"
          protected[this] var localOnly: Boolean = false

          // abstract from Logger
          def log(msg: String): Unit = {
            println("[" + svcName + "] " + msg)
          }

          // regular def with default param
          def inc(by: Int = 1): Int = {
            state += by
            state
          }

          // varargs, by-name param, implicit param
          def computeAll(xs: Int*)(onEmpty: => Int)(implicit scale: Int): Int = {
            if (xs.isEmpty) onEmpty else {
              var acc = 0
              for (x <- xs) {
                acc += x * scale
              }
              acc
            }
          }

          // while / do-while / if / return / try-catch-finally / throw
          def risky(op: () => Int): Int = {
            var done = false
            var result = 0
            while (!done) {
              try {
                val r = op()
                if (r < 0) {
                  throw new IllegalArgumentException("negative!")
                }
                result = r
                done = true
              } catch {
                case _: IllegalArgumentException =>
                  return -1
              } finally {
                () // no-op
              }
            }
            // demo do-while
            var x = 0
            do {
              x += 1
            } while (x < 1)
            result
          }

          // simple method using match
          def classify(x: Int): String = x match {
            case 0                       => "zero"
            case n if n < 0              => "neg"
            case _                       => "pos"
          }

          // auxiliary constructor
          def this() = this("default-service")
        }

        // Companion object for BaseService
        object BaseService {
          // extension-like utility
          implicit class ServiceOps(val svc: BaseService) {
            def fullName: String = "Service(" + svc.svcName + ")"
          }

          // factory
          def make(name: String): BaseService =
            new ConcreteService(name, 0)
        }

        // concrete class with override, self-type mixin, additional fields
        class ConcreteService(name: String, private var counter: Int)
            extends BaseService(name) with NeedsLogSelf {

          override val loggerName: String = "ConcreteService:" + name

          def tick(): Int = {
            counter = inc() // from BaseService
            counter
          }

          def demoAll(): Unit = {
            info("tick -> " + tick())
            log("classify(0) = " + classify(0))

            val u = User(1, "Scala")
            log(u.greet())

            val cmd: Command = Move(1, 2)
            cmd match {
              case Quit                => log("quit")
              case Speak(msg)          => log("say " + msg)
              case m @ Move(x, y)      => log("move " + x + "," + y + "; isMove=" + m.isMovement)
            }

            val buf = new SortedBuffer[Int](List(3, 1, 2))
            buf.insert(0)
            log("sorted buffer: " + buf.all)

            val pos = collectPositives(List(-1, 0, 1, 2, 3))
            log("positives doubled: " + pos)

            val sum = scaledSum(1, 2, 3)
            log("scaledSum = " + sum)

            val risk = risky(() => 5)
            log("risky result: " + risk)

            val maxv = max2(10, 20)
            log("max2 = " + maxv)

            val areaMsg = describeShape(Circle(2))
            log(areaMsg)

            val s2 = sumFirstTwo(List(10, 20, 30))
            log("sumFirstTwo = " + s2)

            // implicit class RichInt
            log("7.squared = " + 7.squared)

            // structural type usage
            val hasElem: HasElem = List(1,2,3)
            log("hasElem.head = " + hasElem.head)

            // XML literal
            val xmlDemo = <msg><body>{"Hello " + name}</body></msg>
            log("xmlDemo = " + xmlDemo.toString)
          }
        }

        // case class
        case class User(id: Int, name: String) {
          def greet(prefix: String = "hi"): String =
            prefix + ", " + name
        }

        // "enum-like" ADT in Scala 2.13: sealed trait + case objects / case classes
        sealed trait Command {
          def isMovement: Boolean = this match {
            case Move(_, _) => true
            case _          => false
          }
        }
        case object Quit extends Command
        case class Speak(msg: String) extends Command
        case class Move(x: Int, y: Int) extends Command

        // sealed hierarchy for pattern match
        sealed trait Shape {
          def area: Double
        }

        final case class Circle(r: Double) extends Shape {
          def area: Double = math.Pi * r * r
        }

        final case class Rect(w: Double, h: Double) extends Shape {
          def area: Double = w * h
        }

        // class with type params, context bound, view bound, etc.
        // - context bound [A : Ordering]  expands to (implicit ord: Ordering[A])
        // - view bound [A <% Ordered[A]]  still compiles in 2.13 (deprecated, but legal with -language:implicitConversions)
        //   (no, thet are not working, so I removed it)
        class SortedBuffer[A : Ordering](init: List[A]) {
          private var data: List[A] = init.sorted
          def insert(a: A): Unit = {
            data = (a :: data).sorted
          }
          def all: List[A] = data
        }

        // max2 to show type params + upper/lower bounds
        def max2[A](a: A, b: A)(implicit ord: Ordering[A]): A = {
          if (ord.gteq(a, b)) a else b
        }

        // factorial with @tailrec
        def factorial(n: Int): Int = {
          @tailrec
          def go(cur: Int, acc: Int): Int =
            if (cur <= 1) acc else go(cur - 1, acc * cur)
          go(n, 1)
        }

        // For-comprehension with generator, guard, value binding, yield
        def collectPositives(xs: List[Int]): List[Int] = {
          for {
            x <- xs
            if x > 0
            y = x * 2
          } yield y
        }

        // Pattern matching showcase
        def describeShape(s: Shape): String = s match {
          case c @ Circle(r) if r > 10 =>
            "Big circle radius=" + c.r
          case Circle(r) =>
            "Circle radius=" + r
          case Rect(w, h) if w == h =>
            "Square side=" + w
          case Rect(w, h) =>
            "Rect " + w + "x" + h
          case _ =>
            "unknown shape"
        }

        // Typed pattern, sequence pattern, wildcard, tuple destructuring
        def sumFirstTwo(xs: List[Int]): Int = xs match {
          case a :: b :: _ =>
            val (x, y) = (a, b) // tuple + pattern val
            x + y
          case _ =>
            0
        }

        // implicit param usage (like Scala 3 using/summon)
        def scaledSum(nums: Int*)(implicit scale: Int): Int = {
          nums.foldLeft(0)((acc, n) => acc + n * scale)
        }

        // higher-order function / lambda
        val adder: (Int, Int) => Int = (a, b) => a + b

        // literals demo
        val literalsDemo: Any = {
          val multi =
            """This is
              |a multiline
              |string.
              |""".stripMargin
          val interpS = s"Pi is about $π"
          val interpF = f"Pi ~ $π%.2f"
          val interpRaw = raw"raw\tno-escape"
          val tuple = (1, "x", true, 2.5f, 3.14d, 'c')
          val arr = Array(1,2,3)
          val list = List(1,2,3)
          val opt: Option[String] = None
          val tried: ScalaTry[Int] = ScalaTry(1 / 1)
          // Warning:
          // symbol literal is deprecated; use Symbol("symbolLiteral")
          // that we cannot suppress with @scala.annotation.nowarn
          //val sym = 'symbolLiteral: @scala.annotation.nowarn
          val sym = Symbol("symbolLiteral")
          val n: String = null
          ()
          (multi, interpS, interpF, interpRaw, tuple, arr, list, opt, tried, sym, n)
        }

        // Try/catch with pattern match on Failure
        def safeDivide(a: Int, b: Int): Int = {
          ScalaTry(a / b) match {
            case Success(v) => v
            case Failure(e) => -1
          }
        }

        // main-like entry point
        def main(args: Array[String]): Unit = {

          implicit val scaleImplicit: Int = 10

          val svc = new ConcreteService("demo", 0)
          svc.demoAll()

          println(svc.fullName) // implicit class ServiceOps

          println("factorial(5) = " + factorial(5))
          println(topLevelVal)
          println(topLevelLazy)
          println(literalsDemo)

          println("safeDivide(10,2) = " + safeDivide(10,2))
          println("safeDivide(10,0) = " + safeDivide(10,0))

          // destructuring vals at top level of method
          val (a, b) = (1, "two")
          println("tuple destructure: a=" + a + ", b=" + b)

          // forSome existential usage:
          val someList: List[_ <: AnyRef] forSome { type T <: AnyRef } =
            List("a","b","c")
          println("someList: " + someList)
        }
      }
    }
		// format: on

    // Uncomment and print to preview results.
    // printed.split("\n").foreach(println)

    // Exact code might change between Scala versions, and we don't care about particular output - only that whole tree was handled.
    assert(printed.nonEmpty)
  }

  test("showCodePretty(..., SyntaxHighlight.plain) should support all known trees on Scala 2") {

    // format: off
    @scala.annotation.nowarn
    val printed = ShowCodePrettyFixtures.testExprPlainPrint {
      // Imports of various shapes
      import scala.annotation.tailrec
      import scala.util.{Try => ScalaTry, Success, Failure}
      import scala.collection.mutable.{Map => MutableMap, ListBuffer}
      import scala.math.{Pi => π}
      import util.{ChainingOps as ChainingMethods}
      import scala.concurrent.duration._ // wildcard import of vals/types
      import scala.language.implicitConversions // to allow implicit conv without warning

      // Top-level vals/vars/defs aren't allowed in Scala 2.13, so we wrap everything in an object.
      // We'll use DemoApp as our "namespace".
      object DemoApp {

        // basic values
        val topLevelVal: String = "hello"
        var topLevelVar: Int = 0
        lazy val topLevelLazy: Long = 9999999999L

        // type aliases
        type Id[A] = A
        type StringList = List[String]

        // existential type
        type HasElem = {
          def head: Any
        }

        // structural type + compound type example
        type ClosableLogger = Logger with Closeable {
          def flush(): Unit
        }

        // implicit values, defs, classes, conversions
        implicit val implicitInt: Int = 123

        implicit def intToString(i: Int): String = i.toString

        implicit class RichInt(private val i: Int) {
          def squared: Int = i * i
        }

        // Trait with abstract/concrete + self type
        trait Logger {
          def log(msg: String): Unit
          val loggerName: String
        }

        trait Closeable {
          def close(): Unit = ()
        }

        trait NeedsLogSelf { self: Logger =>
          def info(m: String): Unit = log("[info][" + loggerName + "] " + m)
        }

        // Base service class
        abstract class BaseService(protected val svcName: String)
            extends Logger with Closeable {

          // constructor params become fields, we can also declare our own
          private var state: Int = 0
          val publicVal: String = "base"
          protected[this] var localOnly: Boolean = false

          // abstract from Logger
          def log(msg: String): Unit = {
            println("[" + svcName + "] " + msg)
          }

          // regular def with default param
          def inc(by: Int = 1): Int = {
            state += by
            state
          }

          // varargs, by-name param, implicit param
          def computeAll(xs: Int*)(onEmpty: => Int)(implicit scale: Int): Int = {
            if (xs.isEmpty) onEmpty else {
              var acc = 0
              for (x <- xs) {
                acc += x * scale
              }
              acc
            }
          }

          // while / do-while / if / return / try-catch-finally / throw
          def risky(op: () => Int): Int = {
            var done = false
            var result = 0
            while (!done) {
              try {
                val r = op()
                if (r < 0) {
                  throw new IllegalArgumentException("negative!")
                }
                result = r
                done = true
              } catch {
                case _: IllegalArgumentException =>
                  return -1
              } finally {
                () // no-op
              }
            }
            // demo do-while
            var x = 0
            do {
              x += 1
            } while (x < 1)
            result
          }

          // simple method using match
          def classify(x: Int): String = x match {
            case 0                       => "zero"
            case n if n < 0              => "neg"
            case _                       => "pos"
          }

          // auxiliary constructor
          def this() = this("default-service")
        }

        // Companion object for BaseService
        object BaseService {
          // extension-like utility
          implicit class ServiceOps(val svc: BaseService) {
            def fullName: String = "Service(" + svc.svcName + ")"
          }

          // factory
          def make(name: String): BaseService =
            new ConcreteService(name, 0)
        }

        // concrete class with override, self-type mixin, additional fields
        class ConcreteService(name: String, private var counter: Int)
            extends BaseService(name) with NeedsLogSelf {

          override val loggerName: String = "ConcreteService:" + name

          def tick(): Int = {
            counter = inc() // from BaseService
            counter
          }

          def demoAll(): Unit = {
            info("tick -> " + tick())
            log("classify(0) = " + classify(0))

            val u = User(1, "Scala")
            log(u.greet())

            val cmd: Command = Move(1, 2)
            cmd match {
              case Quit                => log("quit")
              case Speak(msg)          => log("say " + msg)
              case m @ Move(x, y)      => log("move " + x + "," + y + "; isMove=" + m.isMovement)
            }

            val buf = new SortedBuffer[Int](List(3, 1, 2))
            buf.insert(0)
            log("sorted buffer: " + buf.all)

            val pos = collectPositives(List(-1, 0, 1, 2, 3))
            log("positives doubled: " + pos)

            val sum = scaledSum(1, 2, 3)
            log("scaledSum = " + sum)

            val risk = risky(() => 5)
            log("risky result: " + risk)

            val maxv = max2(10, 20)
            log("max2 = " + maxv)

            val areaMsg = describeShape(Circle(2))
            log(areaMsg)

            val s2 = sumFirstTwo(List(10, 20, 30))
            log("sumFirstTwo = " + s2)

            // implicit class RichInt
            log("7.squared = " + 7.squared)

            // structural type usage
            val hasElem: HasElem = List(1,2,3)
            log("hasElem.head = " + hasElem.head)

            // XML literal
            val xmlDemo = <msg><body>{"Hello " + name}</body></msg>
            log("xmlDemo = " + xmlDemo.toString)
          }
        }

        // case class
        case class User(id: Int, name: String) {
          def greet(prefix: String = "hi"): String =
            prefix + ", " + name
        }

        // "enum-like" ADT in Scala 2.13: sealed trait + case objects / case classes
        sealed trait Command {
          def isMovement: Boolean = this match {
            case Move(_, _) => true
            case _          => false
          }
        }
        case object Quit extends Command
        case class Speak(msg: String) extends Command
        case class Move(x: Int, y: Int) extends Command

        // sealed hierarchy for pattern match
        sealed trait Shape {
          def area: Double
        }

        final case class Circle(r: Double) extends Shape {
          def area: Double = math.Pi * r * r
        }

        final case class Rect(w: Double, h: Double) extends Shape {
          def area: Double = w * h
        }

        // class with type params, context bound, view bound, etc.
        // - context bound [A : Ordering]  expands to (implicit ord: Ordering[A])
        // - view bound [A <% Ordered[A]]  still compiles in 2.13 (deprecated, but legal with -language:implicitConversions)
        //   (no, thet are not working, so I removed it)
        class SortedBuffer[A : Ordering](init: List[A]) {
          private var data: List[A] = init.sorted
          def insert(a: A): Unit = {
            data = (a :: data).sorted
          }
          def all: List[A] = data
        }

        // max2 to show type params + upper/lower bounds
        def max2[A](a: A, b: A)(implicit ord: Ordering[A]): A = {
          if (ord.gteq(a, b)) a else b
        }

        // factorial with @tailrec
        def factorial(n: Int): Int = {
          @tailrec
          def go(cur: Int, acc: Int): Int =
            if (cur <= 1) acc else go(cur - 1, acc * cur)
          go(n, 1)
        }

        // For-comprehension with generator, guard, value binding, yield
        def collectPositives(xs: List[Int]): List[Int] = {
          for {
            x <- xs
            if x > 0
            y = x * 2
          } yield y
        }

        // Pattern matching showcase
        def describeShape(s: Shape): String = s match {
          case c @ Circle(r) if r > 10 =>
            "Big circle radius=" + c.r
          case Circle(r) =>
            "Circle radius=" + r
          case Rect(w, h) if w == h =>
            "Square side=" + w
          case Rect(w, h) =>
            "Rect " + w + "x" + h
          case _ =>
            "unknown shape"
        }

        // Typed pattern, sequence pattern, wildcard, tuple destructuring
        def sumFirstTwo(xs: List[Int]): Int = xs match {
          case a :: b :: _ =>
            val (x, y) = (a, b) // tuple + pattern val
            x + y
          case _ =>
            0
        }

        // implicit param usage (like Scala 3 using/summon)
        def scaledSum(nums: Int*)(implicit scale: Int): Int = {
          nums.foldLeft(0)((acc, n) => acc + n * scale)
        }

        // higher-order function / lambda
        val adder: (Int, Int) => Int = (a, b) => a + b

        // literals demo
        val literalsDemo: Any = {
          val multi =
            """This is
              |a multiline
              |string.
              |""".stripMargin
          val interpS = s"Pi is about $π"
          val interpF = f"Pi ~ $π%.2f"
          val interpRaw = raw"raw\tno-escape"
          val tuple = (1, "x", true, 2.5f, 3.14d, 'c')
          val arr = Array(1,2,3)
          val list = List(1,2,3)
          val opt: Option[String] = None
          val tried: ScalaTry[Int] = ScalaTry(1 / 1)
          // Warning:
          // symbol literal is deprecated; use Symbol("symbolLiteral")
          // that we cannot suppress with @scala.annotation.nowarn
          //val sym = 'symbolLiteral: @scala.annotation.nowarn
          val sym = Symbol("symbolLiteral")
          val n: String = null
          ()
          (multi, interpS, interpF, interpRaw, tuple, arr, list, opt, tried, sym, n)
        }

        // Try/catch with pattern match on Failure
        def safeDivide(a: Int, b: Int): Int = {
          ScalaTry(a / b) match {
            case Success(v) => v
            case Failure(e) => -1
          }
        }

        // main-like entry point
        def main(args: Array[String]): Unit = {

          implicit val scaleImplicit: Int = 10

          val svc = new ConcreteService("demo", 0)
          svc.demoAll()

          println(svc.fullName) // implicit class ServiceOps

          println("factorial(5) = " + factorial(5))
          println(topLevelVal)
          println(topLevelLazy)
          println(literalsDemo)

          println("safeDivide(10,2) = " + safeDivide(10,2))
          println("safeDivide(10,0) = " + safeDivide(10,0))

          // destructuring vals at top level of method
          val (a, b) = (1, "two")
          println("tuple destructure: a=" + a + ", b=" + b)

          // forSome existential usage:
          val someList: List[_ <: AnyRef] forSome { type T <: AnyRef } =
            List("a","b","c")
          println("someList: " + someList)
        }
      }
    }
		// format: on

    // Uncomment and print to preview results.
    // printed.split("\n").foreach(println)

    // Exact code might change between Scala versions, and we don't care about particular output - only that whole tree was handled.
    assert(printed.nonEmpty)
  }

  test("showCodePretty(..., SyntaxHighlight.ANSI) should support all known types on Scala 2") {

    @scala.annotation.nowarn
    trait Animal { def name: String }
    @scala.annotation.nowarn
    trait Speak { def speak(): String }
    @scala.annotation.nowarn
    trait Closeable { def close(): Unit }

    trait Outer {
      type Inner <: Animal
      val inner: Inner
    }

    @scala.annotation.nowarn
    object MyOuter extends Outer {
      final case class Pet(name: String) extends Animal
      type Inner = Pet
      val inner: Inner = Pet("Mittens")
    }

    // format: off
    @scala.annotation.nowarn
    val printed = ShowCodePrettyFixtures.testTypePrettyPrint[
      ({
        // Higher-kinded type lambda that we'll project:
        // L[X] = Either[String, X]
        type L[X] = Either[String, X]

        // A dependent/compound/refined monster:
        // T is:
        //   (outer path-dependent type member MyOuter.Inner)
        //   with (existential List[_ <: Animal])
        //   with (structural refinement { def extra: Int })
        //
        // We wrap more stuff inside T so we can refer to it.
        type T =
          (MyOuter.Inner
            with ({ type A0 <: Animal })#A0 // pull in a path-projected abstract upper-bounded member
            with Speak
            with Closeable) {
            def extra: Int
          }

        // U existentializes a higher-kinded application of L,
        // and mentions an annotated type.
        //
        // - `({ type L[X] = Either[String, X] })#L[_ <: Animal]`
        //    => existential with upper bound, inside a type projection
        // - `String @deprecated(...)`
        //
        type U =
          ( ({ type K = String @deprecated("old","0.0") })#K,
            ({ type Q = List[_ <: Animal] })#Q,
            ({ type R = L[_ <: Animal] })#R
          )

        // V pulls in a refinement with a stable singleton type,
        // an abstract type member, and uses Null / Nothing.
        //
        // - singleton type MyOuter.inner.type
        // - abstract type member `type M >: Null <: AnyRef`
        // - Nothing shows up as lower bound of NothingRef
        //
        type V = {
          val pet: MyOuter.inner.type
          type M >: Null <: AnyRef
          type NothingRef >: Nothing <: Any
        }

      })#T
        with (({
          type L[X] = Either[String, X]
          type T =
            (MyOuter.Inner
              with Speak
              with Closeable) {
              def extra: Int
            }
          type U =
            ( ({ type K = String @deprecated("old","0.0") })#K,
              ({ type Q = List[_ <: Animal] })#Q,
              ({ type R = L[_ <: Animal] })#R
            )
          type V = {
            val pet: MyOuter.inner.type
            type M >: Null <: AnyRef
            type NothingRef >: Nothing <: Any
          }
          // Expose them:
          type All = (T, U, V)
        })#All)
        with ({
          // pull in Outer#Inner (type projection independent of path)
          type W = Outer#Inner
        })#W
    ]
		// format: on

    // Uncomment and print to preview results.
    // printed.split("\n").foreach(println)

    // Exact code might change between Scala versions, and we don't care about particular output - only that whole tree was handled.
    assert(printed.nonEmpty)
  }

  test("showCodePretty(..., SyntaxHighlight.plain) should support all known types on Scala 2") {

    @scala.annotation.nowarn
    trait Animal { def name: String }
    @scala.annotation.nowarn
    trait Speak { def speak(): String }
    @scala.annotation.nowarn
    trait Closeable { def close(): Unit }

    trait Outer {
      type Inner <: Animal
      val inner: Inner
    }

    @scala.annotation.nowarn
    object MyOuter extends Outer {
      final case class Pet(name: String) extends Animal
      type Inner = Pet
      val inner: Inner = Pet("Mittens")
    }

    // format: off
    @scala.annotation.nowarn
    val printed = ShowCodePrettyFixtures.testTypePlainPrint[
      ({
        // Higher-kinded type lambda that we'll project:
        // L[X] = Either[String, X]
        type L[X] = Either[String, X]

        // A dependent/compound/refined monster:
        // T is:
        //   (outer path-dependent type member MyOuter.Inner)
        //   with (existential List[_ <: Animal])
        //   with (structural refinement { def extra: Int })
        //
        // We wrap more stuff inside T so we can refer to it.
        type T =
          (MyOuter.Inner
            with ({ type A0 <: Animal })#A0 // pull in a path-projected abstract upper-bounded member
            with Speak
            with Closeable) {
            def extra: Int
          }

        // U existentializes a higher-kinded application of L,
        // and mentions an annotated type.
        //
        // - `({ type L[X] = Either[String, X] })#L[_ <: Animal]`
        //    => existential with upper bound, inside a type projection
        // - `String @deprecated(...)`
        //
        type U =
          ( ({ type K = String @deprecated("old","0.0") })#K,
            ({ type Q = List[_ <: Animal] })#Q,
            ({ type R = L[_ <: Animal] })#R
          )

        // V pulls in a refinement with a stable singleton type,
        // an abstract type member, and uses Null / Nothing.
        //
        // - singleton type MyOuter.inner.type
        // - abstract type member `type M >: Null <: AnyRef`
        // - Nothing shows up as lower bound of NothingRef
        //
        type V = {
          val pet: MyOuter.inner.type
          type M >: Null <: AnyRef
          type NothingRef >: Nothing <: Any
        }

      })#T
        with (({
          type L[X] = Either[String, X]
          type T =
            (MyOuter.Inner
              with Speak
              with Closeable) {
              def extra: Int
            }
          type U =
            ( ({ type K = String @deprecated("old","0.0") })#K,
              ({ type Q = List[_ <: Animal] })#Q,
              ({ type R = L[_ <: Animal] })#R
            )
          type V = {
            val pet: MyOuter.inner.type
            type M >: Null <: AnyRef
            type NothingRef >: Nothing <: Any
          }
          // Expose them:
          type All = (T, U, V)
        })#All)
        with ({
          // pull in Outer#Inner (type projection independent of path)
          type W = Outer#Inner
        })#W
    ]
		// format: on

    // Uncomment and print to preview results.
    // printed.split("\n").foreach(println)

    // Exact code might change between Scala versions, and we don't care about particular output - only that whole tree was handled.
    assert(printed.nonEmpty)
  }
}
