package hearth
package fp
package effect

/** How to render logs.
  *
  * Allows avoiding hardcoding log level for rendering.
  *
  * @since 0.1.0
  */
sealed trait LogRendering extends Product with Serializable

/** Don't render any logs.
  *
  * @since 0.1.0
  */
case object DontRender extends LogRendering

/** Render logs from the given level and above.
  *
  * @since 0.1.0
  */
final case class RenderFrom(level: Log.Level) extends LogRendering

/** Render logs only from the given level.
  *
  * @since 0.1.0
  */
final case class RenderOnly(level: Log.Level) extends LogRendering
