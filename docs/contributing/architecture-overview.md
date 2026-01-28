# Architecture overview

Since this library should make it easier to work with macros, let us remember how to call a macro.

## How macros are usually called

Let's say you need:

```scala
def method[A, B](value: A): B = ??? // this should be implemented by a macro
```

When working with macros on Scala 2 you'd have to do:

```scala
// If the logic of macro is a single method (very rare case):

import scala.reflect.macros.blackbox

def methodImpl[A: c.WeakTypeTag, B: c.WeakTypeTag](c: blackbox.Context)(value: c.Expr[A]): c.Expr[B] = {
  import c.universe._
  // code that would create c.Expr[B]
  ???
}

import scala.language.experimental.macros

// 1. You need `macro` keyword.
// 2. You do not pass arguments, only types (optionally).
// 3. Names of method arguments and their grouping in parameter lists should be the same:
//    in the `method` signature and `methodImpl`
//    (plus extra `(c: blackbox.Context)` or `(c: whitebox.Context)`).
def method[A, B](value: A): B = macro methodImpl[A, B]
```

```scala
// If the logic of macro is a whole class with multiple methods (more probably),
// it's known as a "macro bundle":

import scala.reflect.macros.blackbox

// The sole argument of the constructor has to be `blackbox.Context` or `whitebox.Context`.
class ClassWithMacroImpl(val c: blackbox.Context) {

  import c.universe._

  def methodImpl[A: c.WeakTypeTag, B: c.WeakTypeTag](value: c.Expr[A]): c.Expr[B] = {
    // code that would create c.Expr[B]
    ???
  }
}

import scala.language.experimental.macros

// 1. You need `macro` keyword.
// 2. Again, you do not pass arguments, only types (optionally). And there is no `new`.
// 3. Names of method arguments and their grouping in parameter lists should be the same:
//    in the `method` signature and `methodImpl`.
def method[A, B](value: A): B = macro ClassWithMacroImpl.methodImpl[A, B]
```

meanwhile on Scala 3 you'd do:

```scala
import scala.quoted.*

object MacroImpl {
  
  // This code CANNOT be placed in a class, inline def can only unquote something
  // that is top-level definition or in an object.
  def methodImpl[A: Type, B: Type](using q: Quotes)(argument: Expr[A]): Expr[B] = {
    import q.reflect.*
    // code that would create c.Expr[B]
    ???
  }
}

// 1. You need to use `inline def` (or transparent `inline def`)
// 2. You need to unquote something that is top-level definition or method of top-level object.
// 3. You need to quote arguments yourself, but it allows them to be renamed and reordered.
inline def method[A, B](value: A): B = ${ MacroImpl.methodImpl[A, B]('{ value }) }
```

It seems that even at this level APIs seem incompatible, and sharing any logic would be out of question.

But it is.

## Sharing implementation between Scala 2 and Scala 3

There is a paper [C. Hofer et al., _Polymorphic Embedding of DSLs_, GPCE, 2008](https://www.informatik.uni-marburg.de/~rendel/hofer08polymorphic.pdf) which describes how we could define _algebras_ (interfaces) and then operate on them, deferring their implementation to the mixins.

For our case, it could look like this, for the shared part:

```scala
// Abstract types and definitions.
// Because it uses path-dependent types, the types and methods can refer each other.
trait MacroTypesAndOperations {

  // abstract types
  type Type[A]
  type Expr[A]

  // abstract operations
  def prettyPrintType[A](tpe: Type[A]): String
  def prettyPrintExpr[A](expr: Expr[A]): String

  def stringToExpr(str: String): Expr[String]
}


// Shared logic of some macro implementation using abstract types and operations.
trait ActualMacroLogic { this: MacroTypesAndOperations =>

  def printTypeAndExpr[A: Type](expr: Expr[A]): Expr[String] = {
    val typeName = prettyPrintType(implicitly[Type[A]])
    val exprCode = prettyPrintExpr(expr)
    stringToExpr(s"$exprCode: $typeName")
  }
}
```

(It would go into `src/main/scala` directory).

How could we implement it on Scala 2? Like that:

```scala
// Implements abstract types and operations for Scala 2.
trait MacroTypesAndOperationsScala2 extends MacroTypesAndOperations {

  val c: blackbox.Context

  type Type[A] = c.WeakTypeTag[A]
  type Expr[A] = c.Expr[A]

  def prettyPrintType[A](tpe: Type[A]): String = tpe.toString
  def prettyPrintExpr[A](expr: Expr[A]): String = showCode(expr)

  def stringToExpr(str: String): Expr[String] = c.Expr(q"$str")
}

// Wire everything together for Scala 2:

class MacroImpl(val c: blackbox.Context)
  extends MacroTypesAndOperationsScala2
  with ActualMacroLogic {

  // Unfortunatelly, macros aren't smart enough to notice that Type=c.WeakTypeTag and Expr=c.Expr
  // so we have to help them:
  def printTypeAndExprImpl[A: c.WeakTypeTag](expr: c.Expr[A]): c.Expr[String] = 
    printTypeAndExpr(expr)
}

def printTypeAndExpr[A](expr: A): String = macro MacroImpl.printTypeAndExprImpl[A]
```

(It would go into `src/main/scala-2` directory).

And on Scala 3? Like this:

```scala
// Implements abstract types and operations for Scala 3.
abstract class MacroTypesAndOperationsScala3(using q: Quotes) extends MacroTypesAndOperations {

  type Type[A] = scala.quoted.Type[A]
  type Expr[A] = scala.quoted.Expr[A]

  def prettyPrintType[A](tpe: Type[A]): String = tpe.show
  def prettyPrintExpr[A](expr: Expr[A]): String = expr.show

  def stringToExpr(str: String): Expr[String] = scala.quoted.Expr("str")
}

// Wire everything together for Scala 3:

class MacroImpl(q: Quotes)
  extends MacroTypesAndOperationsScala3(using q)
  with ActualMacroLogic

object MacroImpl {

  // Unfortunatelly, because this method has to be in an object, we need to manually
  // formard it to class construction and its method call.
  def printTypeAndExpr[A: Type](using q: Quotes)(expr: Expr[A]): Expr[String] =
    new MacroImpl(using q).printTypeAndExpr[A](expr)
}

inline def printTypeAndExpr[A](inline expr: A): String = ${ MacroImpl.printTypeAndExpr[A]('{ expr }) }
```

(It would go into `src/main/scala-3` directory).

While it (currently) looks like a lot of work:

 - if _someone else_ handle both the abstract API and its implementation then all overhead of this approach
   boils down to manual forwarding of methods on Scala 2 and Scala 3
 - in the future, we might be able to do something to decrease this overhead (macro annotations? compiler plugins?)
 - the more complicated the logic of the macro, that we would like to run on both Scala 2 and 3,
   the more shared code we would write, so that overhead quickly becomes negligible to the benefit of having
   virtually single codebase for all non-trivial macro logic.

So the whole codebase implements this approach:

 - `src/main/scala` contains the interfaces which should be implemented by both macro implementations
 - `src/main/scala-2` contains implementations for Scala 2 (and Scala 2-specific utilities)
 - `src/main/scala-3` contains implementations for Scala 3 (and Scala 3-specific utilities)

We should strive to implement tests for this logic in `src/test/scala` and doing as little as possible in `src/test/scala-2`
and `src/test/scala-3`, to ensure that behavior of both versions of Scala is aligned as much as possible.

## Cross-Quotes

Scala 2 has quasi-quotes - `String`-interpolation with a special handling by the compiler:

```scala
val expr: c.Expr[B] = ...
c.Expr[String](
  q"""
  ${ expr }.toString
  """
)
```

 - when we write quasi-quotes the compiler turns this `String` into an AST
 - interpolating in quasi-quotes tells the compiler to splice `c.Expr`/`c.universe.Tree` into the built AST

Scala 3 has proper quotes and splices:

```scala
val expr: Expr[B] = ...
'{
  ${ expr }.toString
}
```

These are rather incompatible, and would normally force us to define an abstract method for any expr-gluing,
and implementing it separately for Scala 2 and 3, which is why we developed experimental **cross-quotes**
(with macros on Scala 2 and a compiler plugin on Scala 3):

```scala
val expr: Expr[B] = ...
Expr.quote {
  Expr.splice { expr }.toString
}
```

This allows us to keep even more code shared.

## FP module

While not strictly required for writing macros it is very convenient to:

 * use some data type for storing derivation errors, and even better, to be able to aggregate them
 * be able to `.traverse` or `.parTraverse` while composing callable computations
 * be able to build-up some logs within a macro, so that, when needed, we could investigate how expansion works for some use case
 * reuse our intuitions and experience from libraries like Cats or ZIO (if it's your cup of tea)
 * without having a whole Cats/ZIO ecosystem inside the macro's classpath (it invites some evictions and version conflicts during compilation!)

The FP module does just that: provides a few type classes, `NonEmpty` collections, and `MIO` - macrio IO that let you reuse your
experience with Cats IO while developing macros.

Usage of FP module is mostly optional, (and certainly the usage of `MIO`). You can develop your code using direct-style, exceptions,
plain `Either`s or your own structures if that's your preference.
