package hearth

private[hearth] trait PlatformCompanionCompat { this: Platform.type =>

  /** Against which version of Hearth the library was compiled. */
  val byHearth: Platform = Platform.Jvm
}
