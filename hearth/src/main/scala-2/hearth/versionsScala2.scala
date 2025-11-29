package hearth

private[hearth] trait LanguageVersionCompanionCompat { this: LanguageVersion.type =>

  /** Against which version of Hearth the library was compiled. */
  val byHearth: LanguageVersion = LanguageVersion.Scala2_13
}

import scala.language.experimental.macros
import scala.reflect.macros.blackbox

final private[hearth] class VersionsMacros(val c: blackbox.Context) extends MacroCommonsScala2 with VersionsMacrosImpl {

  def scalaVersionByJVMCompileTimeImpl: c.Expr[ScalaVersion] =
    scalaVersionByJVMCompileTime

  def jdkVersionByJVMCompileTimeImpl: c.Expr[JDKVersion] =
    jdkVersionByJVMCompileTime
}

private[hearth] trait JDKVersionsCompanionCompat { this: JDKVersion.type =>

  def compiletimeJDKVersion: JDKVersion = macro VersionsMacros.jdkVersionByJVMCompileTimeImpl
}

private[hearth] trait ScalaVersionsCompanionCompat { this: ScalaVersion.type =>

  def byCompileTime: ScalaVersion = macro VersionsMacros.scalaVersionByJVMCompileTimeImpl
}
