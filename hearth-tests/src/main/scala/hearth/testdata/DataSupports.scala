package hearth
package testdata

trait DataSupports { this: MacroCommons =>

  implicit lazy val DataCodec: ExprCodec[Data] = new ExprCodec[Data] {

    val DataType: Type[Data] = Type.of[Data]

    val StringType: Type[String] = Type.of[String]

    def toExpr(value: Data): Expr[Data] = value.fold(
      onNull = Expr.quote(Data()),
      onInt = i => {
        val inner: Expr[Int] = Expr(i)
        Expr.quote(Data(Expr.splice(inner)))
      },
      onLong = l => {
        val inner: Expr[Long] = Expr(l)
        Expr.quote(Data(Expr.splice(inner)))
      },
      onFloat = f => {
        val inner: Expr[Float] = Expr(f)
        Expr.quote(Data(Expr.splice(inner)))
      },
      onDouble = d => {
        val inner: Expr[Double] = Expr(d)
        Expr.quote(Data(Expr.splice(inner)))
      },
      onBoolean = b => {
        val inner: Expr[Boolean] = Expr(b)
        Expr.quote(Data(Expr.splice(inner)))
      },
      onString = s => {
        val inner: Expr[String] = Expr(s)
        Expr.quote(Data(Expr.splice(inner)))
      },
      onList = l => {
        implicit val dt: Type[Data] = DataType
        val inner: Expr[List[Data]] = Expr(l)
        Expr.quote(Data((Expr.splice(inner))))
      },
      onMap = m => {
        implicit val st: Type[String] = StringType
        implicit val dt: Type[Data] = DataType
        val inner: Expr[Map[String, Data]] = Expr(m)
        Expr.quote(Data(Expr.splice(inner)))
      }
    )
    def fromExpr(expr: Expr[Data]): Option[Data] = None // TODO
  }
}
