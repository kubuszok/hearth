package hearth
package crossquotes

final class ImportOrderReproSpec extends MacroSuite {

  group("cross-quotes: Type.of and `import x.Underlying as D` in the same block") {

    test("import before Type.of") {
      ImportOrderReproFixtures.importBeforeTypeOf(List(1)) ==> "scala.collection.immutable.List[scala.Int] / scala.Any"
    }

    test("import after Type.of") {
      ImportOrderReproFixtures.importAfterTypeOf(List(1)) ==> "scala.collection.immutable.List[scala.Int] / scala.Any"
    }
  }

  group("cross-quotes: Type.of and `import x.Underlying as D` in the same class body") {

    test("member before the import") {
      ImportOrderReproFixtures.classBodyTypeOfBeforeImport ==> "scala.Option[scala.Int]"
    }

    test("member after the import") {
      ImportOrderReproFixtures.classBodyTypeOfAfterImport ==>
        "scala.collection.immutable.List[java.lang.String] / scala.Option[scala.collection.immutable.List[java.lang.String]]"
    }
  }
}
