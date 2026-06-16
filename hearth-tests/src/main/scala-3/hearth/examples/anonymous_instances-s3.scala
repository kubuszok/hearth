package hearth
package examples
package anonymous_instances

/** Context-function return (`Int ?=> String`) — Scala 3-only. The override's result must stay a context function, not
  * be flattened into a trailing `using` parameter clause (which would change the member's arity).
  */
trait TraitWithContextFunctionReturn {
  def build(k: String): Int ?=> String
}
