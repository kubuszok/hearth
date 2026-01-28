# Instruction for fixing `forward reference to value` caused by implicit

It happens when you write:

```scala
// With Cross-Quotes
implicit val someType: Type[A] = Type.of[A]
```

Depends on the platform it translates to:

```scala
// Scala 2
implicit val someType: Type[A] = c.weakTypeTag[A]
```

or

```scala
// Scala 3
implicit val someType: Type[A] = scala.quoted.Type.of[A]
```

In both cases the native macro utility summons `Type[A]` - since we are just declaring `implicit`/`given` of that type
we are creating a circular dependency. We have to prevent the utility from summoning its own result by:

```scala
// In one scope, that does not define implicit Type[A]:
private val SomeType = Type.of[A]

locally {
  // In another scope, where we can expose a computed result as an implicit:
  implicit val someType: Type[A] = SomeType
}
```

While it's a bit mundane, it makes it rather explicit which types we are using or not, and where.

**Example:**
```
forward reference to value ... defined on line ... extends over definition of value ...
```
