package hearth

private[hearth] trait LanguageVersionCompanionCompat { this: LanguageVersion.type =>

  /** Against which version of Hearth the library was compiled. */
  val byHearth: LanguageVersion = LanguageVersion.Scala3
}

import scala.quoted.*

final private[hearth] class VersionsMacros(val q: Quotes) extends MacroCommonsScala3(using q), VersionsMacrosImpl
private[hearth] object VersionsMacros {

  def compiletimeJDKVersionImpl(using q: Quotes): Expr[JDKVersion] = new VersionsMacros(q).jdkVersionByJVMCompileTime
  def byCompileTimeImpl(using q: Quotes): Expr[ScalaVersion] = new VersionsMacros(q).scalaVersionByJVMCompileTime
}

private[hearth] trait JDKVersionsCompanionCompat { this: JDKVersion.type =>

  inline def compiletimeJDKVersion: JDKVersion = ${ VersionsMacros.compiletimeJDKVersionImpl }
}

private[hearth] trait ScalaVersionsCompanionCompat { this: ScalaVersion.type =>

  inline def byCompileTime: ScalaVersion = ${ VersionsMacros.byCompileTimeImpl }
}
