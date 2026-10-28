package hearth
package fp
package effect

import hearth.fp.data.NonEmptyVector
import hearth.fp.instances.*
import hearth.fp.syntax.*

/** Behaviours of `MIO` that the laws cannot express: laziness, exception safety, which branches run, and logging. */
final class MioCombinatorsSpec extends Suite {

  private val error1 = new RuntimeException("error 1")
  private val error2 = new RuntimeException("error 2")
  private val error3 = new RuntimeException("error 3")

  private def messages(result: MResult[?]): Either[Vector[String], Any] =
    result.left.map(_.toVector.map(_.getMessage))

  private def throwing(): Int = throw error1
  private def throwingMio(): MIO[Int] = throw error1

  group("MIO construction") {

    test("MIO.apply, MIO.defer and MIO.suspend should not evaluate anything until the program is run") {
      var evaluated = 0
      val applied = MIO { evaluated += 1; evaluated }
      val deferred = MIO.defer { evaluated += 1; MIO.pure(evaluated) }
      val suspended = MIO.suspend { evaluated += 1; Right(evaluated) }
      evaluated ==> 0
      (applied >> deferred >> suspended).unsafe.runSync._2 ==> Right(3)
    }

    test("running the same MIO value twice should run its effects and logs twice, from scratch") {
      var evaluated = 0
      val program = Log.info("running") >> MIO { evaluated += 1; evaluated }
      val (state1, result1) = program.unsafe.runSync
      val (state2, result2) = program.unsafe.runSync
      result1 ==> Right(1)
      result2 ==> Right(2)
      state1.logs.render.fromInfo("root") <==> state2.logs.render.fromInfo("root")
      state2.logs.render.fromInfo("root") <==> "root:\n└ [Info]  running"
    }

    test("MIO.fail should keep all the given errors in order") {
      messages(MIO.fail[Int](error1, error2, error3).unsafe.runSync._2) ==> Left(
        Vector("error 1", "error 2", "error 3")
      )
      messages(MIO.fail[Int](NonEmptyVector(error1, error2)).unsafe.runSync._2) ==> Left(Vector("error 1", "error 2"))
    }
  }

  group("MIO exception safety") {

    test("non-fatal exceptions thrown inside MIO.apply, MIO.defer, map and flatMap should become failures") {
      messages(MIO(throwing()).unsafe.runSync._2) ==> Left(Vector("error 1"))
      messages(MIO.defer(throwingMio()).unsafe.runSync._2) ==> Left(Vector("error 1"))
      messages(MIO.pure(1).map(_ => throwing()).unsafe.runSync._2) ==> Left(Vector("error 1"))
      messages(MIO.pure(1).flatMap(_ => throwingMio()).unsafe.runSync._2) ==> Left(Vector("error 1"))
    }

    test("an exception thrown inside an error handler should become a failure") {
      messages(MIO.fail[Int](error2).handleError(_ => throwing()).unsafe.runSync._2) ==> Left(Vector("error 1"))
    }
  }

  group("MIO sequencing and fallbacks") {

    test("map2, *>, <* and >> should not run the second MIO after the first one failed") {
      var evaluated = 0
      val second = MIO { evaluated += 1; evaluated }
      messages(MIO.fail[Int](error1).map2(second)(_ + _).unsafe.runSync._2) ==> Left(Vector("error 1"))
      messages((MIO.fail[Int](error1) *> second).unsafe.runSync._2) ==> Left(Vector("error 1"))
      messages((MIO.fail[Int](error1) <* second).unsafe.runSync._2) ==> Left(Vector("error 1"))
      messages((MIO.fail[Int](error1) >> second).unsafe.runSync._2) ==> Left(Vector("error 1"))
      evaluated ==> 0
    }

    test("parMap2, &> and <& should run the second MIO even after the first one failed") {
      var evaluated = 0
      val second = MIO { evaluated += 1; evaluated }
      messages(MIO.fail[Int](error1).parMap2(second)(_ + _).unsafe.runSync._2) ==> Left(Vector("error 1"))
      messages((MIO.fail[Int](error1) &> second).unsafe.runSync._2) ==> Left(Vector("error 1"))
      messages((MIO.fail[Int](error1) <& second).unsafe.runSync._2) ==> Left(Vector("error 1"))
      evaluated ==> 3
    }

    test("parMap2 should aggregate the errors of both branches in order") {
      messages(MIO.fail[Int](error1, error2).parMap2(MIO.fail[Int](error3))(_ + _).unsafe.runSync._2) ==> Left(
        Vector("error 1", "error 2", "error 3")
      )
    }

    test("orElse should not run the fallback when the first MIO succeeds") {
      var evaluated = 0
      val fallback = MIO { evaluated += 1; evaluated }
      MIO.pure(42).orElse(fallback).unsafe.runSync._2 ==> Right(42)
      evaluated ==> 0
    }

    test("firstOf should return the first success and not run the remaining alternatives") {
      var evaluated = 0
      val notRun = MIO { evaluated += 1; evaluated }
      MIO.firstOf(MIO.fail[Int](error1), MIO.pure(2), notRun).unsafe.runSync._2 ==> Right(2)
      MIO.firstOf(MIO.pure(1)).unsafe.runSync._2 ==> Right(1)
      evaluated ==> 0
    }

    test("firstOf should aggregate the errors of all alternatives in order when all of them fail") {
      messages(MIO.firstOf(MIO.fail[Int](error1), MIO.fail[Int](error2), MIO.fail[Int](error3)).unsafe.runSync._2) ==>
        Left(Vector("error 1", "error 2", "error 3"))
    }

    test("flatTap and mapTap should run the side effect and keep the original value") {
      var tapped = 0
      MIO.pure(1).flatTap(a => MIO(tapped += a)).unsafe.runSync._2 ==> Right(1)
      MIO.pure(2).mapTap(a => tapped += a).unsafe.runSync._2 ==> Right(2)
      tapped ==> 3
    }

    test("flatTap should fail when the side effect fails") {
      messages(MIO.pure(1).flatTap(_ => MIO.fail[Int](error1)).unsafe.runSync._2) ==> Left(Vector("error 1"))
    }
  }

  group("MIO with Traverse") {

    def check(visited: scala.collection.mutable.ListBuffer[Int])(i: Int): MIO[Int] = MIO {
      visited += i
      if (i % 2 == 0) i / 2 else throw new RuntimeException(s"$i is odd")
    }

    test("traverse should stop at the first failure") {
      val visited = scala.collection.mutable.ListBuffer.empty[Int]
      messages(List(2, 1, 4, 3).traverse(check(visited)).unsafe.runSync._2) ==> Left(Vector("1 is odd"))
      visited.toList ==> List(2, 1)
    }

    test("parTraverse and parSequence should run every element and aggregate all errors in order") {
      val visited = scala.collection.mutable.ListBuffer.empty[Int]
      messages(List(2, 1, 4, 3).parTraverse(check(visited)).unsafe.runSync._2) ==> Left(
        Vector("1 is odd", "3 is odd")
      )
      visited.toList ==> List(2, 1, 4, 3)
      messages(List(2, 1, 4, 3).map(check(visited)).parSequence.unsafe.runSync._2) ==> Left(
        Vector("1 is odd", "3 is odd")
      )
    }

    test("traverse should keep the logs of every element in order") {
      val program = List(1, 2, 3).traverse(i => Log.info(s"item $i") >> MIO.pure(i))
      val (state, result) = program.unsafe.runSync
      result ==> Right(List(1, 2, 3))
      state.logs.render.fromInfo("root") <==> "root:\n├ [Info]  item 1\n├ [Info]  item 2\n└ [Info]  item 3"
    }

    test("traverse and parTraverse should be stack-safe for long lists") {
      val numbers = List.range(0, 100000)
      List(numbers.traverse(i => MIO.pure(i)), numbers.parTraverse(i => MIO.pure(i))).foreach { program =>
        program.unsafe.runSync._2.map(_.size) ==> Right(100000)
      }
    }
  }

  group("MIO.log") {

    test("info, warn and error should log the message when the MIO succeeds and keep the value") {
      val program = MIO.pure(1).log.info("info message").log.warn("warn message").log.error("error message")
      val (state, result) = program.unsafe.runSync
      result ==> Right(1)
      state.logs.render.fromInfo("root") <==>
        "root:\n├ [Info]  info message\n├ [Warn]  warn message\n└ [Error] error message"
    }

    test("info should not log anything when the MIO fails") {
      val (state, result) = MIO.fail[Int](error1).log.info("not logged").unsafe.runSync
      messages(result) ==> Left(Vector("error 1"))
      state.logs.render.fromInfo("root") <==> "root:"
    }

    test("valueAs* should log a message computed from the value") {
      val program = MIO.pure(1).log.valueAsInfo(a => s"info $a").log.valueAsWarn(a => s"warn $a").log.valueAsError { a =>
        s"error $a"
      }
      val (state, result) = program.unsafe.runSync
      result ==> Right(1)
      state.logs.render.fromInfo("root") <==> "root:\n├ [Info]  info 1\n├ [Warn]  warn 1\n└ [Error] error 1"
    }

    test("errorsAs* should log a message computed from the errors and keep failing") {
      val program = MIO
        .fail[Int](error1, error2)
        .log
        .errorsAsInfo(es => s"info ${es.size}")
        .log
        .errorsAsWarn(es => s"warn ${es.size}")
        .log
        .errorsAsError(es => s"error ${es.size}")
      val (state, result) = program.unsafe.runSync
      messages(result) ==> Left(Vector("error 1", "error 2"))
      state.logs.render.fromInfo("root") <==> "root:\n├ [Info]  info 2\n├ [Warn]  warn 2\n└ [Error] error 2"
    }

    test("errorsAs* should not log anything when the MIO succeeds") {
      val (state, result) = MIO.pure(1).log.errorsAsError(_ => "not logged").unsafe.runSync
      result ==> Right(1)
      state.logs.render.fromInfo("root") <==> "root:"
    }

    test("resultAs* should log a message computed from the result of both a success and a failure") {
      val (successState, successResult) =
        MIO.pure(1).log.resultAsInfo(r => s"result ${r.isRight}").unsafe.runSync
      successResult ==> Right(1)
      successState.logs.render.fromInfo("root") <==> "root:\n└ [Info]  result true"

      val (failureState, failureResult) =
        MIO.fail[Int](error1).log.resultAsWarn(r => s"result ${r.isRight}").unsafe.runSync
      messages(failureResult) ==> Left(Vector("error 1"))
      failureState.logs.render.fromInfo("root") <==> "root:\n└ [Warn]  result false"
    }
  }

  group("Logs rendering") {

    val program = Log.info("info message") >> Log.namedScope("only info inside") {
      Log.info("nested info")
    } >> Log.warn("warn message") >> Log.error("error message")
    lazy val logs = program.unsafe.runSync._1.logs
    // Other specs may switch on the global scope benchmarking, which adds durations to the scope names.
    def withoutDurations(rendered: String): String = rendered.replaceAll(" \\(-?\\d+ns\\)", "")

    test("render with DontRender should give None and with RenderFrom/RenderOnly the filtered tree") {
      logs.render("root", DontRender) ==> None
      logs.render("root", RenderFrom(Log.Level.Warn)) ==> Some(logs.render.fromWarn("root"))
      logs.render("root", RenderOnly(Log.Level.Warn)) ==> Some(logs.render.onlyWarn("root"))
    }

    test("onlyInfo, onlyWarn and onlyError should render just the entries of a single level") {
      withoutDurations(
        logs.render.onlyInfo("root")
      ) <==> "root:\n├ [Info]  info message\n└ only info inside:\n  └ [Info]  nested info"
      logs.render.onlyWarn("root") <==> "root:\n└ [Warn]  warn message"
      logs.render.onlyError("root") <==> "root:\n└ [Error] error message"
    }

    test("a scope without any entries of the rendered levels should not be rendered") {
      logs.render.fromWarn("root") <==> "root:\n├ [Warn]  warn message\n└ [Error] error message"
    }
  }

  group("MLocal") {

    test("should start from the initial value, also in every new run of the same program") {
      val counter = MLocal(10)(identity)(_ max _)
      val program = counter.get.flatMap(i => counter.set(i + 1) >> counter.get)
      program.unsafe.runSync._2 ==> Right(11)
      program.unsafe.runSync._2 ==> Right(11)
    }

    test("two locals created with the same arguments should be independent") {
      val first = MLocal(0)(identity)(_ max _)
      val second = MLocal(0)(identity)(_ max _)
      (first.set(5) >> second.get.map2(first.get)(_ -> _)).unsafe.runSync._2 ==> Right((0, 5))
    }

    test("parMap2 should give each branch a forked copy of a previously written local and join the copies") {
      val local = MLocal(0)(_ * 10)(_ + _)
      val branchA = local.get.flatMap(a => local.set(a + 1).as(a))
      val branchB = local.get.flatMap(b => local.set(b + 2).as(b))
      (local
        .set(1) >> branchA.parTuple(branchB).map2(local.get)((seen, joined) => (seen, joined))).unsafe.runSync._2 ==>
        Right(((10, 10), 11 + 12))
    }

    test("parMap2 should give each branch a forked copy and join the copies afterwards") {
      val local = MLocal(1)(_ * 10)(_ + _)
      val branchA = local.get.flatMap(a => local.set(a + 1).as(a))
      val branchB = local.get.flatMap(b => local.set(b + 2).as(b))
      (branchA.parTuple(branchB).map2(local.get)((seen, joined) => (seen, joined))).unsafe.runSync._2 ==> Right(
        ((10, 10), 11 + 12)
      )
    }
  }
}
