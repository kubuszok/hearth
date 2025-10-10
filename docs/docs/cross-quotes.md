# Cross Quotes

Cross Quotes is a library that provides a unified way to write quoted expressions and type operations that can be compiled on both Scala 2 and Scala 3. It bridges the gap between the different metaprogramming APIs in Scala 2 and Scala 3, allowing you to write code that works across both versions.

!!! warning "Work in Progress - follow [hearth#56](https://github.com/MateuszKubuszok/hearth/issues/56) to see the progress of documenting Hearth."

## Installation

[![Hearth Cross Quotes JVM versions](https://index.scala-lang.org/MateuszKubuszok/hearth/hearth-cross-quotes/latest.svg?platform=jvm)](https://central.sonatype.com/search?q=hearth&namespace=com.kubuszok&name=hearth-cross-quotes_3) <br>

!!! example "[sbt](https://www.scala-sbt.org/)"

    ```scala
    libraryDependencies ++= CrossVersion.partialVersion(scalaVersion.value) match {
      case Some((3, _)) => Seq(compilerPlugin("com.kubuszok" % "hearth-cross-quotes" % "{{ hearth_version() }}_3"))
      case _            => Seq()
    }
    ```

!!! example "[Scala CLI](https://scala-cli.virtuslab.org/)"
    
    ```scala
    //> using target.scala {{ scala.3 }}
    //> using plugin com.kubuszok::hearth-cross-quotes::{{ hearth_version() }}
    ```

!!! warning

    Cross Quotes are implemented as macros on Scala 2 (so they are automatically pulled in with the rest of the library),
    and as a compiler plugin on Scala 3 - so they have to be added **only** on Scala 3.

## The Problem

Scala 2 and Scala 3 have fundamentally different metaprogramming systems:

- **Scala 2**: Uses macros with `scala.reflect.macros.blackbox.Context` and quasiquotes (`q""`, `tq""`, etc)
- **Scala 3**: Uses the new quotes system with `scala.quoted.Expr`, `scala.quoted.Type`, `scala.quoted.Quotes` and proper quotes
  and splices (`'{}` and `${}`)

This means that code using metaprogramming features cannot be shared between Scala 2 and Scala 3 without significant duplication or complex abstractions.

## The Solution

Cross Quotes provides a unified API that gets transformed into the appropriate native syntax for each Scala version:

- **Scala 2**: Uses a macro to transform Cross Quotes syntax into native quasiquotes
- **Scala 3**: Uses a compiler plugin to transform Cross Quotes syntax into native quotes

Additionally, under the hood, it passes around and sets some local copy of `blackbox.Context`/`scala.quoted.Quotes`,
that makes the whole process possible.

## Usage

### Basic Type Operations

#### `Type.of[A]`

Get a type representation that works in both Scala 2 and Scala 3:

!!! example "Type without arguments"

    ```scala
    val StringType = Type.of[String]
    ```

This gets transformed to:

!!! example "Scala 2 macro expansion result"

    ```scala
    val StringType = weakTypeTag[String].asInstanceOf[Type[String]]
    ```

!!! example "Scala 3 compiler plugin rewrite"

    ```scala
    val StringType = scala.quoted.Type.of[String]
    ```

If there are some type parameters:

!!! example "Type with type parameters"

    ```scala
    def OptionType[A: Type] = Type.of[Option[A]]
    ```

then it would also handle converting them as well:

!!! example "Scala 2 macro expansion result"

    ```scala
    def OptionType[A: Type] = {
      implicit val A: WeakTypeTag[A] = Type[A].asInstanceOf[WeakTypeTag[A]]
      weakTypeTag[Option[A]].asInstanceOf[Type[Option[A]]]
    }
    ```

!!! example "Scala 3 compiler plugin rewrite"

    ```scala
    def OptionType[A: Type] = {
      given A: scala.quoted.Type[A] = Type[A].asInstanceOf[scala.quoted.Type[A]]
      scala.quoted.Type.of[Option[A]]
    }
    ```

!!! warning

    Both `weakTypeTag[A]` and `scala.quoted.Type.of[A]` would pick up implicit if it was in the scope.

    So writing code like this:

    ```scala
    implicit val A: Type[A] = Type.of[A]
    ```

    generates:

    ```scala
    implicit val A: Type[A] = A // infinite recursion
    ```

    Therefore I **strongly** recommend to run `Type.of` in the scope where its results are **not** being pulled in as `implicit`s/`given`s.

#### Type Constructor Operations

Cross Quotes supports type constructors with up to 22 type parameters:

```scala
// For type constructors with 1 type parameter
val optionTest = Type.Ctor1.of[Option]

// For type constructors with 2 type parameters  
val eitherTest = Type.Ctor2.of[Either]

// For type constructors with 3 type parameters
val tuple3Test = Type.Ctor3.of[Tuple3]

// And so on up to Type.Ctor22.of
```

Type constructors provide `.apply` and `.unapply` methods to create `Type` and match on it.

You can also specify bounds:

```scala
// Upper bounded
val upperBounded = Type.Ctor1.UpperBounded.of[String, Option]

// Bounded with both lower and upper bounds
val bounded = Type.Ctor1.Bounded.of[Nothing, String, Option]
```

### Expression Quoting and Splicing

#### `Expr.quote`

Quote an expression to create a typed expression:

```scala
def simpleExpr: Expr[String] = Expr.quote {
  "Hello, World!"
}

def complexExpr: Expr[Int] = Expr.quote {
  val x = 1
  val y = 2
  x + y
}
```

These gets transformed to:
- **Scala 2**:
  ```scala
  c.Expr[String](q"""
    "Hello, World"
  """).asInstanceOf[Expr[String]]

  c.Expr[Int](q"""
    val x = 1
    val y = 2
    x + y
  """).asInstanceOf[Expr[Int]]
  ```
- **Scala 3**:
  ```scala
  '{
    "Hello, World"
  }.asInstanceOf[Expr[String]]

  '{
    val x = 1
    val y = 2
    x + y
  }.asInstanceOf[Expr[Int]]
  ```

#### `Expr.splice`

Splice a quoted expression back into code:

```scala
def combineExpressions: Expr[String] = {
  val e1 = Expr.quote(1)
  val e2 = Expr.quote(2)
  
  Expr.quote {
    val a = Expr.splice(e1) + Expr.splice(e2)
    a.toString
  }
}
```

This gets transformed to:
- **Scala 2**:
  ```scala
  c.Expr[String](q"""
    val a = ${e1} + ${e2}
    a.toString
  """).asInstanceOf[Expr[String]]
  ```
- **Scala 3**:
  ```scala
  '{
    val a = ${e1} + ${e2}
    a.toString
  }.asInstanceOf[Expr[String]]
  ```

### Working with Type Parameters

Cross Quotes handles type parameters automatically:

```scala
def genericExpr[A: Type](e: Expr[A]): Expr[String] = Expr.quote {
  Expr.splice(e).toString
}
```

The `[A: Type]` context bound gets automatically converted to the appropriate syntax for each Scala version.

### Pattern Matching on Types

You can pattern match on types using the type constructor operations:

```scala
val optionTest = Type.Ctor1.of[Option]
val eitherTest = Type.Ctor2.of[Either]

def analyzeType[In: Type]: Expr[String] = Type[In] match {
  case optionTest(aParam) =>
    import aParam.Underlying as A
    Expr(s"Option of ${A.plainPrint}")
    
  case eitherTest(aParam, bParam) =>
    import aParam.Underlying as A
    import bParam.Underlying as B
    Expr(s"Either of ${A.plainPrint} and ${B.plainPrint}")
    
  case _ => Expr("Unknown type")
}
```

### Nested Expressions

Cross Quotes handles nested expressions correctly:

```scala
def nestedExpr: Expr[String] = {
  def intToString(i: Expr[Int]): Expr[String] = Expr.quote {
    Expr.splice(i).toString
  }

  Expr.quote {
    def localMethod(i: Int): String = Expr.splice(intToString(Expr.quote(i)))
    localMethod(42)
  }
}
```

## Requirements

### Compiler Setup

#### Scala 2

For Scala 2, you only need to use the core hearth plugin, macros would be expanded automatically.

#### Scala 3

For Scala 3, you need to enable the Cross Quotes compiler plugin:

```scala
addCompilerPlugin("com.kubuszok" %% "hearth-cross-quotes" % "{{ hearth_version() }}")
```

### Code Structure

Cross Quotes can only be used inside traits that depend on `MicroCommons`:

```scala
trait MyMacros { this: MacroTypedCommons => // or just this: MacroCommons =>
  // Your Cross Quotes code here
}
```

## Examples

### Simple Type Analysis

```scala
trait TypeAnalysis { this: MacroTypedCommons =>
  def analyzeOption[A: Type]: Expr[String] = {
    val optionTest = Type.Ctor1.of[Option]
    
    Type[A] match {
      case optionTest(aParam) =>
        import aParam.Underlying as A
        Expr(s"Option[${A.plainPrint}]")
      case _ => Expr("Not an Option")
    }
  }
}
```

### Generic Expression Building

```scala
trait ExpressionBuilder { this: MacroTypedCommons =>
  def buildList[A: Type](elements: List[Expr[A]]): Expr[List[A]] = {
    if (elements.isEmpty) {
      Expr.quote { List.empty[A] }
    } else {
      val head = elements.head
      val tail = buildList(elements.tail)
      
      Expr.quote {
        Expr.splice(head) :: Expr.splice(tail)
      }
    }
  }
}
```

### Complex Type Matching

```scala
trait ComplexTypeMatching { this: MacroTypedCommons =>
  def analyzeTuple[In: Type]: Expr[String] = {
    val tuple2Test = Type.Ctor2.of[Tuple2]
    val tuple3Test = Type.Ctor3.of[Tuple3]
    
    Type[In] match {
      case tuple2Test(aParam, bParam) =>
        import aParam.Underlying as A
        import bParam.Underlying as B
        Expr(s"Tuple2[${A.plainPrint}, ${B.plainPrint}]")
        
      case tuple3Test(aParam, bParam, cParam) =>
        import aParam.Underlying as A
        import bParam.Underlying as B
        import cParam.Underlying as C
        Expr(s"Tuple3[${A.plainPrint}, ${B.plainPrint}, ${C.plainPrint}]")
        
      case _ => Expr("Not a supported tuple")
    }
  }
}
```

## Limitations

1. **Early Version**: Cross Quotes is still in early development, so some complex expressions may fail with cryptic errors.

2. **MicroCommons Dependency**: Can only be used inside traits that depend on `MicroCommons`.

3. **Type Constructor Limits**: Type constructors are limited to 22 type parameters.

4. **Complex Anonymous Classes**: Some complex anonymous class patterns may not work correctly.

### Known Issues

- Anonymous classes with complex type parameters may cause parsing issues
- Very deeply nested expressions might hit compiler limits
- Some edge cases in type parameter handling may not work as expected

## Debugging

You can enable logging to see how Cross Quotes transforms your code:

```scala
// In your build.sbt

// For Scala 2 with CrossQuotesMacros
scalacOptions += "-Xmacro-settings:hearth.cross-quotes.logging=true"

// For Scala 3 with CrossQuotesPlugin
scalacOptions += "-P:hearth:cross-quotes:logging=true"
```

This will show you the transformation from Cross Quotes syntax to the native Scala 2/3 syntax.
