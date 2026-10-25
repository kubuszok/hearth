package hearth
package crossquotes

/** kindlings feedback (2) - see [[ImportOrderReproFixturesImpl]]. */
final class ImportOrderReproSpec extends MacroSuite {

  group("(2) cross-quotes: Type.of followed by `import x.Underlying as D` in the same block") {

    test("import BEFORE Type.of (workaround)") {
      ImportOrderReproFixtures.importBeforeTypeOf(List(1)) ==> "scala.collection.immutable.List[scala.Int] / scala.Any"
    }

    test("import AFTER Type.of (reproducer; fails to COMPILE on Scala 3)") {
      ImportOrderReproFixtures.importAfterTypeOf(List(1)) ==> "scala.collection.immutable.List[scala.Int] / scala.Any"
    }
  }

  group("(2) cross-quotes: Type.of in a class-body member BEFORE `import x.Underlying as D` in the same class body") {

    test("member before the import") {
      ImportOrderReproFixtures.classBodyTypeOfBeforeImport ==> "scala.Option[scala.Int]"
    }

    test("member after the import") {
      ImportOrderReproFixtures.classBodyTypeOfAfterImport ==>
        "scala.collection.immutable.List[java.lang.String] / scala.Option[scala.collection.immutable.List[java.lang.String]]"
    }
  }
}
