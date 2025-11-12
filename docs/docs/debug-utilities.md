# Debug Utilities

If you want to preview:

 - the final code (after all macros expansions, implicit summoning etc)
 - the final code resolved as `implicit`/`given` for some
 - or the AST of such code

try `hearth.debug.Debug`.

!!! example "`Debug` utilities"

    ```scala
    //> using scala {{ scala.3 }}
    //> using dep com.kubuszok::hearth::{{ hearth_version() }}
    import hearth.debug.Debug

    Debug.withFinalCodeInIDE {
      val a = 10
      s"Some example: $a"
    }

    Debug.withFinalASTInIDE {
      val a = 10
      s"Another example: $a"
    }

    inline def example[A]: String = {
      Debug.withFinalCodeInIDE {
        import scala.compiletime.*

        inline erasedValue[A] match {
          case _: Option[a] => "yes, it's an option"
          case _            => "no, it's not an option"
        }
      }
    }

    example[Option[String]]

    Debug.withGivenCodeInIDE[scala.collection.Factory[Int, List[Int]]]

    Debug.withGivenASTInIDE[scala.collection.Factory[Int, List[Int]]]
    ```

`Debug` uses the same printers as [better-printers](better-printers.md) module, but exposes them directly to user,
so that you can check how some code looks like without modifying a macro. It can also be used in REPL, Scastie or IDE.

Even if you don't want to use Hearth for your macros, you can still use these utils as a temporary dependency to
check how some AST looks like, to improve your hearth-less macros.
