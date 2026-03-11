# Type Name Utilities

The `hearth.typename` package provides `TypeName[A]` — a type class that produces human-readable type names at runtime.

!!! note "Reference Implementation"
    Like [Source Utilities](source-utilities.md), `TypeName[A]` is both a useful utility and a reference
    implementation showing how to build auto-derived type classes on top of Hearth's runtime-aware type printing.

## The `TypeName[A]` Type Class

`TypeName[A]` provides 4 different ways to print the name of a type:

| Method        | Example (`Option[String]`)                 | Description                                        |
|---------------|--------------------------------------------|----------------------------------------------------|
| `prettyPrint` | `"\u001b[...scala.Option[java.lang.String]\u001b[...]"` | ANSI colored, fully qualified names     |
| `plainPrint`  | `"scala.Option[java.lang.String]"`         | no ANSI, fully qualified names                     |
| `simplePrint` | `"Option[String]"`                         | no ANSI, short names with type parameters          |
| `shortPrint`  | `"Option"`                                 | no ANSI, short name only, no type parameters       |

Instances are **auto-derived** — no boilerplate needed:

!!! example

    ```scala
    import hearth.typename.TypeName

    // Scala 2 — TypeName is resolved via `implicit def`
    // Scala 3 — TypeName is resolved via `inline given`
    val tn = TypeName[Option[String]]
    println(tn.plainPrint)  // "scala.Option[java.lang.String]"
    println(tn.simplePrint) // "Option[String]"
    println(tn.shortPrint)  // "Option"
    ```
    
## Injection from Existing Instances

When a `TypeName` instance exists for a type argument, the derived instance uses it instead of the default printing.
This allows you to customize how types appear inside larger type expressions:

!!! example

    ```scala
    implicit val customStr: TypeName[String] = new TypeName[String] {
      def prettyPrint = "Str"
      def plainPrint  = "Str"
      def simplePrint = "Str"
      def shortPrint  = "Str"
    }
    
    // The derived TypeName[Option[String]] picks up the custom TypeName[String]
    TypeName[Option[String]].plainPrint // "scala.Option[Str]"
    ```

### Context Bound Injection

The injection pattern is especially useful with context bounds — when deriving a `TypeName` for a type with
abstract type parameters, existing `TypeName` instances for those parameters are injected automatically:

!!! example

    ```scala
    def typeName[A: TypeName]: TypeName[List[A]] = TypeName.derived
    typeName[String].plainPrint // "scala.collection.immutable.List[java.lang.String]"
    ```

!!! warning "Scala Version Requirements for Injection"
    **Injection from existing `TypeName` instances requires:**

    - **Scala 2.13.17+** (uses `c.inferImplicitValueIgnoring`)
    - **Scala 3.7.0+** (uses `Implicits.searchIgnoring`)

    On older Scala versions (2.13.16, 3.3.x–3.6.x), `TypeName` still works correctly for basic
    derivation (all 4 print methods produce accurate output), but **injection is disabled** — existing
    `TypeName` instances for type arguments are not picked up. The macro falls back to default
    type printing for all type arguments.

    This limitation exists because safe implicit summoning (that avoids infinite recursion with the
    auto-derived `TypeName.derived`) requires `summonImplicitIgnoring`, which is only available on
    newer compiler versions.

## Use Cases

### 1. Error Messages

!!! example

    ```scala
    def validate[A: TypeName](value: A): Either[String, A] =
      if (isValid(value)) Right(value)
      else Left(s"Invalid ${TypeName[A].simplePrint}: $value")
    ```

### 2. Debug Logging

!!! example

    ```scala
    def logType[A: TypeName](value: A): Unit =
      println(s"[${TypeName[A].shortPrint}] $value")
    ```

### 3. Type Class Derivation Libraries

!!! example

    `TypeName[A]` can be used inside macro-derived type class instances to produce better error messages:

    ```scala
    // Inside a macro:
    val name = Type.plainPrint[A]  // compile-time string
    // vs.
    // runtime string, with injection
    val nameExpr: Expr[String] =
      Expr.quote(TypeName[A].plainPrint)
    ```

## How It Works

`TypeName[A]` is derived using a macro that:

1. Calls `Type.runtimePrettyPrint`, `Type.runtimePlainPrint`, and `Type.runtimeShortPrint` to build
   the 4 print variants
2. For each type argument, attempts to summon an existing `TypeName` instance (using
   `Expr.summonImplicitIgnoring` to avoid recursion with the auto-derived `TypeName.derived`)
3. If found, caches the summoned instance as a `lazy val` and calls the corresponding method on it
4. Constructs an anonymous `TypeName[A]` with the 4 computed strings

See [`TypeNameMacros.scala`](https://github.com/MateuszKubuszok/hearth/blob/master/hearth/src/main/scala/hearth/typename/TypeNameMacros.scala)
for the shared macro implementation.

## Relationship with Other Utilities

- **[Basic Utilities](basic-utilities.md)**: `TypeName` is built on `Type.runtimePlainPrint`, `Type.runtimePrettyPrint`, and `Type.runtimeShortPrint`
- **[Better Printers](better-printers.md)**: The runtime-aware type printing infrastructure lives in Better Printers
- **[Source Utilities](source-utilities.md)**: Similar auto-derivation pattern (`implicit def`/`inline given` in companion)
