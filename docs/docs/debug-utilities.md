# Debug Utilities

Debug utilities help you inspect macro expansions during compilation. They allow you to preview:

 - the final code (after all macro expansions, implicit summoning, etc.)
 - the final code resolved as `implicit`/`given` for some type
 - or the AST (Abstract Syntax Tree) of such code

!!! important "Compiler Diagnostics, Not Runtime Logging"
    These utilities use **compiler reporting mechanisms** (`Environment.reportInfo`), not structured logging or runtime output. The output appears:
    
    - As **IDE hints** (blue squiggly lines in VS Code/IntelliJ - hover to see the output)
    - In **compilation logs** (when compiling from command line)
    - In **Scastie** (as hover hints)
    
    The output is **only visible during compilation**, not at runtime. This makes them perfect for debugging macro implementations without modifying your macro code.

## When to Use What

- **`withFinalCodeInIDE`**: Use to see the final expanded code after all macro transformations. Shows human-readable Scala code.
- **`withFinalASTInIDE`**: Use to see the AST structure of the final expanded code. Useful for understanding the internal representation.
- **`withGivenCodeInIDE[T]`**: Use to see what implicit/given value is being summoned for type `T`. Shows the code that implements the implicit.
- **`withGivenASTInIDE[T]`**: Use to see the AST structure of the implicit/given value being summoned.

!!! tip
    Start with `withFinalCodeInIDE` or `withGivenCodeInIDE` for most debugging scenarios. Use the AST variants (`withFinalASTInIDE`, `withGivenASTInIDE`) when you need to understand the internal tree structure, such as when debugging macro implementations.

!!! example "`Debug` utilities - Scala 3"

    ```scala
    //> using scala {{ scala.2_13 }} {{ scala.3 }}
    //> using dep com.kubuszok::hearth::{{ hearth_version() }}
    import hearth.debug.Debug

    // Preview final expanded code
    Debug.withFinalCodeInIDE {
      val a = 10
      s"Some example: $a"
    }
    // Hover over the code above in your IDE to see the expanded version

    // Preview AST structure
    Debug.withFinalASTInIDE {
      val a = 10
      s"Another example: $a"
    }

    // Preview macro expansion inside inline methods
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

    // Preview implicit/given resolution
    Debug.withGivenCodeInIDE[scala.collection.Factory[Int, List[Int]]]

    Debug.withGivenASTInIDE[scala.collection.Factory[Int, List[Int]]]
    ```


## How It Works

`Debug` utilities use the same printers as the [better-printers](better-printers.md) module, but expose them directly to users through compiler diagnostics. This allows you to check how code looks without modifying your macro implementation.

The output appears as compiler informational messages, which IDEs and build tools can display as hints. This makes them perfect for:

- **Debugging macro expansions** without adding `println` statements
- **Understanding implicit resolution** by seeing what code is actually being used
- **Learning macro internals** by inspecting AST structures
- **Temporary debugging** - you can add these utilities temporarily to understand code, then remove them

Even if you don't want to use Hearth for your macros, you can still use these utilities as a temporary dependency to check how some AST looks, which can help improve your macro implementations.

## Troubleshooting

**Q: I don't see any output in my IDE**

- Make sure you're hovering over the code wrapped in `Debug.withFinalCodeInIDE` (or similar)
- Check your IDE's settings for compiler hints/diagnostics
- Try compiling from command line - the output will appear in compilation logs
- In VS Code with Metals, ensure diagnostics are enabled

**Q: The output is too verbose**

- Use `withFinalCodeInIDE` instead of `withFinalASTInIDE` for more readable output
- The AST variants show the full tree structure, which can be very detailed

**Q: Can I use this in production code?**

- These utilities are designed for debugging during development
- They add compiler diagnostics but don't affect runtime behavior
- However, it's recommended to remove them before committing production code
