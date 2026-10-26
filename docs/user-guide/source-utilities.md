# Source Utilities

Source utilities provide access to source location information (method name, line number, file path, etc.) at runtime, similar to Python's `__name__`, C++'s `__LINE__`, or Ruby's `__FILE__`. These are useful for debugging, error messages, logging, and providing automatic diagnostics.

!!! important "Reference Implementation"
    These utilities are a **reference implementation** demonstrating how to build [sourcecode](https://github.com/com-lihaoyi/sourcecode)-like functionality using Hearth's macro API. They are intended to be used **outside of macros** (in regular application code), not inside macro implementations. The macros expand at the call site to capture source location information, making them usable in any regular Scala code.
    
    If you need source location utilities for production use, consider using the [sourcecode](https://github.com/com-lihaoyi/sourcecode) library directly, which is more mature and widely used. These utilities serve as a learning example and demonstration of Hearth's capabilities.

## Available Utilities

Source utilities expose the following information as `implicit`/`given` values:

- **`MethodName`**: The name of the enclosing method (or `<init>` for constructors)
- **`Line`**: The current line number in the source file
- **`File`**: The full path to the current source file
- **`FileName`**: Just the filename (without path) of the current source file
- **`Location`**: A combination of `File` and `Line`, useful for error messages

and one implicit conversion:

- **`Text[T]`** (since 0.4.0): a value together with the source code of the expression that computed it

!!! example "`Source` utilities - Scala 3"

    ```scala
    //> using scala {{ scala.3 }}
    //> using dep com.kubuszok::hearth::{{ hearth_version() }}
    import hearth.source.*

    def exampleMethod(): Unit = {
      println(summon[MethodName])  // Prints: exampleMethod
      println(summon[Line])        // Prints: 42 (or whatever line this is on)
      println(summon[File])        // Prints: /path/to/MyFile.scala
      println(summon[FileName])     // Prints: MyFile.scala
      println(summon[Location])    // Prints: /path/to/MyFile.scala:42
    }
    ```

!!! example "`Source` utilities - Scala 2"

    ```scala
    //> using scala {{ scala.2_13 }}
    //> using dep com.kubuszok::hearth::{{ hearth_version() }}
    import hearth.source._

    def exampleMethod(): Unit = {
      println(implicitly[MethodName])  // Prints: exampleMethod
      println(implicitly[Line])        // Prints: 42
      println(implicitly[File])        // Prints: /path/to/MyFile.scala
      println(implicitly[FileName])     // Prints: MyFile.scala
      println(implicitly[Location])    // Prints: /path/to/MyFile.scala:42
    }
    ```

## Use Cases

### 1. Enhanced Error Messages

!!! example

    Add source location information to error messages for better debugging:

    ```scala
    import hearth.source.*
    
    def validateInput[A](value: A)(implicit loc: Location): Either[String, A] = {
      if (value == null) {
        Left(s"Validation failed at ${loc}: value cannot be null")
      } else {
        Right(value)
      }
    }
    ```

### 2. Debug Logging

!!! example

    Include source location in debug output:

    ```scala
    import hearth.source.*
    
    def debugLog(message: String)(implicit method: MethodName, line: Line): Unit = {
      println(s"[DEBUG] ${method}(${line}): ${message}")
    }
    ```

### 3. Automatic Diagnostics

!!! example

    Automatically capture source context for diagnostics:
    
    ```scala
    import hearth.source.*
    
    def processData(data: List[Int])(implicit loc: Location): List[Int] = {
      if (data.isEmpty) {
        throw new IllegalArgumentException(s"Empty data at ${loc}")
      }
      data.map(_ * 2)
    }
    ```

### 4. Location Tracking

!!! example

    The `Location` type combines file and line information:
    
    ```scala
    import hearth.source.*
    
    def trackLocation[A](value: A)(implicit loc: Location): A = {
      println(s"Processing value at ${loc}")
      value
    }
    
    // Location also provides convenient access to fileName
    def logWithFileName(message: String)(implicit loc: Location): Unit = {
      println(s"[${loc.fileName}:${loc.line}] $message")
    }
    ```

### 5. Showing the asserted expression

!!! example

    `Text[T]` captures both the value and the source code of an argument, so an assert-like method can show what failed:

    ```scala
    import hearth.source.*

    def check(condition: Text[Boolean]): Unit =
      if (!condition.value) throw new AssertionError(s"Failed: ${condition.source}")

    val a = 1
    val b = 2
    check(a + b == 4) // AssertionError: Failed: a + b == 4

    // the conversion can also be called explicitly
    val captured = Text.generate(a + b)
    captured.value  // 3
    captured.source // "a + b"
    ```

## Understanding the Types

All source utilities use phantom types for type safety:

- **`MethodName`**: A subtype of `String` - use `MethodName.wrap(name)` to create, or `summon[MethodName]` to get the current value
- **`Line`**: A subtype of `Int` - represents line numbers (1-based)
- **`File`**: A subtype of `String` - full file path (normalized with forward slashes)
- **`FileName`**: A subtype of `String` - just the filename without path
- **`Location`**: A case class containing `File` and `Line` - can be derived implicitly if both `File` and `Line` are available
- **`Text[T]`**: A case class containing `value: T` and `source: String` - created by the implicit conversion
  `Text.generate`, which uses [`expr.sourceCode`](basic-utilities.md#expression-operations) (and falls back to the
  printed expression when the source is not available)

## How It Works

Source utilities use Hearth's macro API internally to capture source location information at compile time. When you use `summon[MethodName]` or `MethodName.derived` in your code, a macro expands at that call site to capture the current source location. This is similar to how the [sourcecode](https://github.com/com-lihaoyi/sourcecode) library works, but implemented using Hearth's utilities.

The values are determined at compile time but available at runtime, making them perfect for logging, error messages, and debugging without manual string literals.

## Limitations

- **Method context required**: `MethodName` requires an enclosing method (will fail in top-level code or object initialization)
- **File context required**: `File` and `FileName` require file position information (may fail in some REPL contexts)
- **Compile-time expansion**: The macros expand at compile time, so the values reflect the source location where the macro is called, not where it's defined

## Relationship with Other Utilities

Source utilities demonstrate how to use Hearth's macro API to build runtime utilities:

- **[Environment](basic-utilities.md#reporting)**: Source utilities use `Environment.currentPosition` internally to extract location information
- **[Better Printers](better-printers.md)**: The macro implementations use Better Printers for error messages
- **[sourcecode library](https://github.com/com-lihaoyi/sourcecode)**: Source utilities are a reference implementation showing how to build similar functionality using Hearth's API

If you're building your own source location utilities or need to capture source information in macros, you can use Hearth's `Environment.currentPosition` API directly, similar to how Source utilities do it internally.
