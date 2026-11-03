package hearth
package std

import hearth.fp.data.NonEmptyMap

final class ProviderResultSpec extends Suite {

  private def renderReasons(result: ProviderResult[Any]): List[(String, String)] = result match {
    case ProviderResult.Skipped(reasons) =>
      reasons.toList.map { case (provider, reason) =>
        provider -> reason.fold(error => s"error: ${error.getMessage}", r => r())
      }
    case ProviderResult.Matched(value) => List("matched" -> value.toString)
  }

  group("ProviderResult") {

    test("toOption returns the matched value and discards skip reasons") {
      ProviderResult.Matched(1).toOption ==> Some(1)
      ProviderResult.skipped("provider", "reason").toOption ==> None
    }

    test("map transforms the matched value and preserves skip reasons") {
      ProviderResult.Matched(1).map(_ + 1) ==> ProviderResult.Matched(2)
      renderReasons(ProviderResult.skipped("provider", "reason").map((_: Int) + 1)) ==> List("provider" -> "reason")
    }

    test("flatMap chains off the matched value and short-circuits on skip") {
      ProviderResult.Matched(1).flatMap(i => ProviderResult.Matched(i + 1)) ==> ProviderResult.Matched(2)
      renderReasons(ProviderResult.Matched(1).flatMap(_ => ProviderResult.skipped("next", "declined"))) ==>
        List("next" -> "declined")
      var called = false
      renderReasons(ProviderResult.skipped("provider", "reason").flatMap { (i: Int) =>
        called = true
        ProviderResult.Matched(i)
      }) ==> List("provider" -> "reason")
      called ==> false
    }

    test("skippedLazily does not build the reason until it is read") {
      var built = 0
      val result = ProviderResult.skippedLazily("provider", { built += 1; "expensive reason" })
      built ==> 0
      renderReasons(result) ==> List("provider" -> "expensive reason")
      built ==> 1
    }

    test("failed stores the error under the provider name") {
      renderReasons(ProviderResult.failed("provider", new RuntimeException("boom"))) ==>
        List("provider" -> "error: boom")
    }

    test("Skipped keeps a reason for every provider in order") {
      val reasons = NonEmptyMap(
        "first" -> (Right(() => "reason 1"): Either[Throwable, () => String]),
        "second" -> (Left(new RuntimeException("boom")): Either[Throwable, () => String])
      )
      renderReasons(ProviderResult.Skipped(reasons)) ==> List("first" -> "reason 1", "second" -> "error: boom")
    }
  }

  group("ProviderProvenance") {

    test("isBuiltIn is true only for providers from the hearth.std.extensions package") {
      ProviderProvenance("name", "hearth.std.extensions.IsOptionProviderForScalaOption").isBuiltIn ==> true
      ProviderProvenance("name", "com.example.IsOptionProviderForMyOption").isBuiltIn ==> false
      ProviderProvenance("name", "hearth.std.IsOptionProviderForMyOption").isBuiltIn ==> false
    }
  }
}
