# Instruction for developing a basic utility

This instruction covers adding new utilities or extending support for new types in the hearth library.

## Adding a new utility method

When adding a new utility, both the API and implementations must be updated:

1. **Add the method to the shared API:**
   - Add the method signature to the appropriate trait in `hearth/src/main/scala/hearth/`
   - If applicable, add a corresponding extension method

2. **Implement for both Scala versions:**
   - Add Scala 2 implementation in `hearth/src/main/scala-2/hearth/`
   - Add Scala 3 implementation in `hearth/src/main/scala-3/hearth/`

3. **Add or update test fixtures:**
   - If possible, use an existing test fixture rather than creating a new one
   - For example, if adding a flag to `Type[A]` interface, add it to `TypeFixturesImpl.scala` in the `testFlags` method
   - Update the corresponding expected values in all test cases that use this fixture

4. **Update tests:**
   - Update all usages of the modified fixture to expect the new value
   - Run the tests to verify

## Adding support for a previously unsupported type

When adding support for a new type:

1. **Create an example:**
   - Add the example type in the appropriate location:
     - `hearthTests/src/main/scala/` - for cross-platform, cross-version types
     - `hearthTests/src/main/scalajvm/` - for JVM-only types
     - `hearthTests/src/main/scala-3/` - for Scala 3-only types

2. **Add or expand test suite:**
   - Add test cases in the appropriate location:
     - `hearthTests/src/test/scala/` - for cross-platform, cross-version tests
     - `hearthTests/src/test/scalajvm/` - for JVM-only tests
     - `hearthTests/src/test/scala-3/` - for Scala 3-only tests

## Testing the changes

Unless you encounter cross-quotes issues, testing involves recompiling:
- `hearth` module
- `hearth-tests` module
- `hearth-sandwich-tests` module

for both Scala 2 and Scala 3.

**Prefer MCP over sbt commands:**

The MCP server provides:
- Compilation errors
- Linter issues
- List of available methods, their types, and visibility

This shortens the feedback loop significantly compared to running sbt commands directly.

**Example workflow:**
```
1. Make changes to the API and implementations
2. Use MCP to compile hearth module (for the Scala version you're working on)
3. Fix any compilation errors reported by MCP
4. Use MCP to compile hearth-tests
5. Use MCP to run the relevant tests
6. Repeat for the other Scala version
7. Use MCP to compile and test hearth-sandwich-tests
```

## Common patterns

**Adding a flag method to `Type[A]`:**
1. Add `def isNewFlag: Boolean` to `Type` trait in shared API
2. Implement in both `TypeScala2` and `TypeScala3`
3. Add `"isNewFlag" -> Data.fromBoolean(tpe.isNewFlag)` to `testFlags` in `TypeFixturesImpl.scala`
4. Update expected `Data` values in `TypesSpec.scala`

**Adding an extension method:**
1. Add the extension method to the appropriate `*Extensions` object in shared API
2. Ensure the implementation works for both Scala versions (or provide version-specific implementations)
3. Add test cases to the corresponding `*Spec` test suite
