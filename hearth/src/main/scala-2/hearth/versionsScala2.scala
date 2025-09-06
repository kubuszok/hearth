package hearth

private[hearth] trait LanguageVersionCompanionCompat { this: LanguageVersion.type =>

  /** Against which version of Hearth the library was compiled. */
  val byHearth: LanguageVersion = LanguageVersion.Scala2_13
}
