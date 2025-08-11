package hearth
package typed

import hearth.testdata.{Data, DataSupports}

trait TypesFixturesImpl { this: MacroTypedCommons & DataSupports =>

  def testNamesPrinters[A: Type]: Expr[Data] = Expr(
    Data.map(
      "Type.shortName" -> Data(Type.shortName[A]),
      "Type.fcqn" -> Data(Type.fcqn[A]),
      "Type.plainPrint" -> Data(Type.plainPrint[A]),
      "Type.prettyPrint" -> Data(removeAnsiColors(Type.prettyPrint[A]))
    )
  )

  def testFlags[A: Type]: Expr[Data] = try
    Expr(
      Data.map(
        "Type.isPrimitive" -> Data(Type.isPrimitive[A]),
        "Type.isBuiltIn" -> Data(Type.isBuiltIn[A]),
        "Type.isAbstract" -> Data(Type.isAbstract[A]),
        "Type.isFinal" -> Data(Type.isFinal[A]),
        "Type.isClass" -> Data(Type.isClass[A]),
        "Type.notBuiltInClass" -> Data(Type.notBuiltInClass[A]),
        "Type.isPlainOldJavaObject" -> Data(Type.isPlainOldJavaObject[A]),
        "Type.isJavaBean" -> Data(Type.isJavaBean[A]),
        "Type.isSealed" -> Data(Type.isSealed[A]),
        "Type.isJavaEnum" -> Data(Type.isJavaEnum[A]),
        "Type.isJavaEnumValue" -> Data(Type.isJavaEnumValue[A]),
        "Type.isCase" -> Data(Type.isCase[A]),
        "Type.isObject" -> Data(Type.isObject[A]),
        "Type.isVal" -> Data(Type.isVal[A]),
        "Type.isAvailable(Everywhere)" -> Data(Type.isAvailable[A](Everywhere))
      )
    )
  catch {
    case e: Throwable =>
      e.printStackTrace()
      throw e
  }

  private val optionTest = Type.Ctor1.of[Option]
  // object optionTest extends Type.Ctor1[Option] {

  //   private val ctx = CrossQuotes.ctx[scala.reflect.macros.blackbox.Context]
  //   import ctx.universe.{Type as _, *}

  //   private val Fany = ctx.weakTypeTag[Option[Any]].tpe.typeSymbol

  //   def apply[A >: Nothing <: Any: Type]: Type[Option[A]] = {
  //     implicit val A0: ctx.WeakTypeTag[A] = Type[A].asInstanceOf[ctx.WeakTypeTag[A]]
  //     ctx.weakTypeTag[Option[A]].asInstanceOf[Type[Option[A]]]
  //   }

  //   def unapply[A](tpe: Type[A]): Option[??] =
  //     tpe.asInstanceOf[ctx.WeakTypeTag[A]].tpe.dealias.widen.baseType(Fany) match {
  //       case TypeRef(_, _, List(tp1)) =>
  //         Some(ctx.WeakTypeTag(tp1.dealias.widen).asInstanceOf[Type[Any]].as_<:??<:[Nothing, Any])
  //       case _ => None
  //     }
  // }

  def testTypeCtor[A: Type]: Expr[Data] =
    Type[A] match {
      case optionTest(bParam) =>
        import bParam.Underlying as B
        val String = Type.of[String]
        val optString = optionTest(using String)
        Expr(Data.map("unapplied" -> Data(B.plainPrint), "reapplied" -> Data(optString.plainPrint)))
      case _ => Expr(Data("Not an option"))
    }
}
