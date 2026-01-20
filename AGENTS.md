# AGENTS.md - AI Agent Guidelines for Hearth

This document provides guidelines for AI agents (like Claude Code, GitHub Copilot, Cursor, etc.) working with the Hearth codebase.

## Project Overview

**Hearth** is the first Scala macros' standard library, designed to make macro development easier and more maintainable across Scala 2.13 and Scala 3.

- **Language**: Scala (pure Scala library)
- **Scala Versions**: 2.13.16, 3.3.7 (primary); 2.13.18, 3.8.0-RC3 (regression testing)
- **Platforms**: JVM, Scala.js, Scala Native
- **Build Tool**: SBT (but see restrictions below)
- **License**: Apache 2.0

## Critical Restrictions

### 🚫 FORBIDDEN: Running SBT Commands Directly

**NEVER** run SBT commands directly, even as a fallback. This includes:

```bash
# ❌ FORBIDDEN - Do NOT run these commands
sbt compile
sbt test
sbt clean
./sbt compile
sbt "project hearth" compile
sbt ci-jvm-2_13
sbt quick-test
```

If MCP is not working, and user informs us that it cannot be fixed, only then we can ASK for permission to run sbt in client mode (`sbt --client`).

**Why?** New SBT shell has a slow start up, but MCP would use a build server that is already running. The project uses Metals (via MCP) for all build operations.

### 🚫 FORBIDDEN: Running Git Commit/Push Commands

**NEVER** run git commit, push, or other destructive git operations. This includes:

```bash
# ❌ FORBIDDEN - Do NOT run these commands
git commit
git commit -m "message"
git commit --amend
git push
git push --force
git rebase
git merge
git reset --hard
```

**Why?** Commit and push operations should be initiated by the human developer, not automated agents. Agents should prepare changes but let humans decide when to commit.

**Allowed git operations:**
- `git status` - Check repository state
- `git diff` - View changes
- `git log` - View history
- `git branch` - List branches
- `git show` - Show commits

### ✅ REQUIRED: Use hearth-metals MCP Server

**ALL** compilation, testing, and diagnostics MUST go through the `hearth-metals` MCP server.

#### Finding the MCP Server Address

The MCP server configuration is stored in one of these files:
- `.metals/mcp.json` (standard Metals configuration)
- `.cursor/mcp.json` (Cursor IDE configuration)

Example configuration:
```json
{
  "servers": {
    "hearth-metals": {
      "url": "http://localhost:58885/sse"
    }
  }
}
```

#### What to Do If MCP Server Is Not Running

If you cannot connect to the MCP server or the configuration files are missing:

1. **Check for configuration files:**
   ```bash
   cat .metals/mcp.json
   cat .cursor/mcp.json
   ```

2. **If files exist but server is unreachable:**
   - Inform the user: "The hearth-metals MCP server is configured at `<URL>` but is not responding. Please start the Metals server in your IDE (VS Code/Cursor)."
   - Do NOT attempt to start the server yourself
   - Do NOT fall back to running SBT commands

3. **If configuration files don't exist:**
   - Inform the user: "No MCP server configuration found. Please ensure Metals is running in your IDE and the MCP server is configured."
   - Ask the user to verify their IDE setup

4. **Never proceed without MCP access:**
   - Do NOT say "I'll compile it manually with SBT as a fallback"
   - Do NOT attempt workarounds
   - Wait for the user to start the MCP server
   - Only if user explicitly state that MCP cannot be fixed ask for permission to use `sbt --client` (only user can run sbt in server mode).

#### Using the MCP Server

Once connected to the MCP server, use it for:
- **Compilation**: Request compilation through MCP tools
- **Testing**: Run tests through MCP tools
- **Diagnostics**: Get compiler errors, warnings, and type information
- **Code navigation**: Find definitions, references, implementations
- **Type information**: Query types at specific positions

#### MCP Server Project Configuration

**IMPORTANT:** The MCP server is configured to use **only ONE Scala version** (either 2.13 OR 3) and **only ONE platform** (JVM, JS, or Native) at a time.

**Why this limitation?**
- IDEs (VS Code, Cursor, IntelliJ) get confused when a single file belongs to multiple projects
- This prevents duplicate errors, inconsistent type information, and navigation issues
- The configuration is controlled by `dev.properties`

**Before starting work, ALWAYS:**

1. **Check which projects are currently enabled:**
   ```bash
   cat dev.properties
   ```
   Look at the `ide.scala` and `ide.platform` settings.

2. **Verify you're working on the correct version/platform:**
   - If working on Scala 3-specific features, ensure `ide.scala = 3`
   - If working on Scala 2.13-specific features, ensure `ide.scala = 2.13`
   - If working on platform-specific code, ensure `ide.platform` matches

3. **If the configuration doesn't match your work:**
   - **SUGGEST** to the user: "I notice `dev.properties` is set to Scala X.X and platform Y, but we're working on Scala A.B / platform Z code. Should I update `dev.properties` to match?"
   - **Wait for confirmation** before proceeding
   - **After changing `dev.properties`:** The user must reload the Metals build server in their IDE
   - **DO NOT commit** the `dev.properties` changes

**Example workflow:**
```
Agent: I see we're working on Scala 3-specific code in `src/main/scala-3/`, but dev.properties
       is configured for Scala 2.13. Should I update dev.properties to use Scala 3 and ask you
       to reload the build server?

User: Yes, go ahead

Agent: [Updates dev.properties to set ide.scala = 3]
       Please reload the Metals build server in your IDE for the changes to take effect.
```

## Repository Structure

```
hearth/
├── hearth/                          # Main library module
│   └── src/main/
│       ├── scala/                   # Shared code (all versions/platforms)
│       ├── scala-2/                 # Scala 2.13-specific implementations
│       ├── scala-3/                 # Scala 3-specific implementations
│       ├── scalajs/                 # Scala.js platform code
│       ├── scalajvm/                # JVM platform code
│       └── scalanative/             # Scala Native platform code
│
├── hearth-better-printers/          # Better alternatives to showCode/showRaw
├── hearth-cross-quotes/             # Cross-platform quoting utilities
├── hearth-micro-fp/                 # Lightweight FP utilities
│
├── hearth-tests/                    # Test suite for main library
├── hearth-sandwich-tests/           # Cross-compilation tests (Scala 2 ↔ 3)
├── hearth-sandwich-examples-213/    # Scala 2.13 test cases
├── hearth-sandwich-examples-3/      # Scala 3 test cases
│
├── docs/                            # MkDocs documentation
├── project/                         # SBT build configuration
├── scripts/                         # Utility scripts
│
├── build.sbt                        # Main build configuration
├── dev.properties                   # Local IDE settings (DO NOT COMMIT)
├── CONTRIBUTING.md                  # Contribution guidelines (620+ lines)
└── README.md                        # Project introduction
```

### Module Dependencies

**Published modules:**
1. `hearth-better-printers` - Base printer utilities
2. `hearth-cross-quotes` - Core cross-platform quoting (depends on better-printers)
3. `hearth-micro-fp` - FP utilities (no dependencies)
4. `hearth` - Main library (depends on micro-fp and better-printers)

**Test modules:**
- `hearth-tests` - Main test suite
- `hearth-sandwich-tests` - Cross-compilation validation

#### Critical Dependency Chain for Testing

**IMPORTANT:** `hearth-better-printers` is used by `hearth-cross-quotes` on Scala 2 to generate quasiquotes.

This means:
- **Fixes to `cross-quotes` might require fixes to `better-printers`**
- **Testing changes to either module requires compiling the full dependency chain:**
  1. Compile `hearth-better-printers`
  2. Compile `hearth-cross-quotes` (depends on better-printers)
  3. Compile `hearth` (depends on both modules)
  4. Compile and run `hearth-tests` (ensures changes are actually safe)

**When working on `cross-quotes` or `better-printers`:**
- Be aware that changes in `better-printers` will affect `cross-quotes` code generation on Scala 2
- Always test the complete chain before considering the work done
- Use MCP server to compile each module in order
- Run `hearth-tests` as the final verification step

**If a new behavior needs to be tested:**
- It would need a test in `hearth-tests` (depending on the use case: in shared, Scala 2-only, Scala 3-only, or JVM-only part)
- This test would most likely need an new fixture or an update to existing ones - they are defined in implementations on `main` source files
  and matched with adapters in Scala 2 and Scala 3 `main` source files
- It may or may not need new test classes (probably placed in `main` source files of `hearth-tests` but ask!)
- Fixtures is what actually calls macro utilities defined in `hearth` module

**Example workflow:**
```
Agent: I've updated better-printers to improve AST printing. Since cross-quotes uses
       better-printers for Scala 2 quasiquote generation, I need to verify the full chain:
       1. Compiling hearth-better-printers...
       2. Compiling hearth-cross-quotes...
       3. Compiling hearth...
       4. Running hearth-tests to verify safety...
```

#### Debugging Cross-Quotes Errors on Scala 2

When you see compilation errors with paths like:
```
[error] [path to project]/hearth/<macro>:[row]:[column] ...
```

This indicates an error in a **generated quasiquote** by `cross-quotes` on Scala 2.

**What this means:**
- The `<macro>` portion in the path shows it's generated macro code, not a regular source file
- The error is in code that `cross-quotes` generated using `better-printers`
- The issue is likely in how `better-printers` is converting AST to quasiquote syntax

**How to debug:**

1. **Identify the source:**
   - Look at the error message and stack trace to find which macro is failing
   - Find the corresponding code in `hearth-cross-quotes` that generates this quasiquote

2. **Check the quasiquote generation:**
   - The issue is in how `better-printers` formats the AST for Scala 2 quasiquotes
   - Look for recent changes to `better-printers` that might affect output format
   - Check if the generated code is syntactically valid Scala 2

3. **Enable logging (optional):**
   - Set `log.cross-quotes = true` in `dev.properties` to see generated code
   - This helps visualize what quasiquote is being generated
   - Remember to reload the build server after changing this setting

4. **Fix the issue:**
   - Update `better-printers` to generate correct Scala 2 syntax
   - Test the full dependency chain (better-printers → cross-quotes → hearth → hearth-tests)
   - Verify the fix works for all affected macros

**Example:**
```
[error] /Users/dev/hearth/<macro>:15:23: expected ")" but found identifier
```
This suggests `better-printers` generated a quasiquote with unbalanced parentheses or incorrect syntax at that position.

## Development Guidelines

### Code Organization Patterns

1. **Cross-version code sharing:**
   - Shared code goes in `src/main/scala/`
   - Scala 2-specific code in `src/main/scala-2/`
   - Scala 3-specific code in `src/main/scala-3/`
   - Newest Scala 3 features in `src/main/scala-newest-3/`

2. **Cross-platform code sharing:**
   - Shared platform code in base `scala/` directory
   - Platform-specific code in `scalajs/`, `scalajvm/`, `scalanative/`

3. **Architectural pattern:**
   - Uses trait-based polymorphic embedding (Hofer et al., GPCE 2008)
   - Abstract traits define operations and types
   - Platform/version-specific implementations via trait mixins
   - Shared logic inherits from abstract traits

### Code Style Requirements

**Mandatory rules (enforced by CI):**

1. **No braceless Scala 3 syntax** - Always use braces for blocks
   ```scala
   // ❌ WRONG
   if condition then
     doSomething()

   // ✅ CORRECT
   if (condition) {
     doSomething()
   }
   ```

2. **Nested package declarations** (not dot notation)
   ```scala
   // ❌ WRONG
   package hearth.std.extensions

   // ✅ CORRECT
   package hearth
   package std
   package extensions
   ```

   **Rationale:** Reminds us that package visibility is hierarchical in Scala

3. **Scalafmt compliance:**
   - Scala 2 shared code: `-Xsource:3` syntax (enables some Scala 3 features)
   - Scala 3-specific code: Full Scala 3 syntax
   - Configuration in `.scalafmt.conf`

4. **Cross-compilation is mandatory:**
   - Code must work on both Scala 2.13 and Scala 3
   - If a solution only works on one version, it's not acceptable
   - Even Scala 3-only features must not break cross-compilation

5. **No external dependencies:**
   - Only stdlib + macro infrastructure allowed
   - FP utilities already provided in `hearth-micro-fp`

### Development Configuration

The `dev.properties` file controls IDE settings:

```properties
# Choose IDE Scala version: 2.13 or 3
ide.scala = 3

# Choose IDE platform: jvm, js, or native
ide.platform = jvm

# Enable cross-quotes logging: true, false, or file names
log.cross-quotes = false
```

**Important:**
- DO NOT commit changes to `dev.properties`
- Consider running: `git update-index --assume-unchanged dev.properties`
- After changing settings, reload build in IDE

### Making Code Changes

When modifying code:

1. **Read before editing:**
   - NEVER propose changes to code you haven't read
   - Understand existing patterns before suggesting modifications
   - Check both Scala 2 and Scala 3 implementations if they exist

2. **Respect the Pareto principle:**
   - 80% of use cases with 20% of effort
   - Simple things should be easy
   - Don't over-engineer for edge cases
   - Fallback to normal macros is acceptable for complex cases

3. **Test cross-version compatibility:**
   - Changes must compile on both Scala 2.13 and 3
   - Use MCP server to verify compilation on both versions
   - Check platform compatibility (JVM, JS, Native) if relevant

4. **Follow existing patterns:**
   - Study similar implementations in the codebase
   - Use the polymorphic embedding pattern for cross-version code
   - Match the style and structure of surrounding code

5. **Documentation:**
   - Update relevant documentation in `docs/` if adding features
   - Follow the existing MkDocs structure
   - Include code examples in Scala 2 and Scala 3 syntax

### Current Work Context

Based on recent commits, the project is currently working on:
- **Standard extensions for Java types** (Iterator, Optional, Boolean, etc.)
- **Provider patterns** for IsValueType, IsOption, IsCollection, IsEither, IsMap
- **Service resource loading** for extension discovery

When working in this area:
- Follow the established provider pattern
- Add implementations for both Scala 2 and 3
- Update service resources in `META-INF/services/`
- Write tests in `hearth-tests`

## Testing Strategy

### Test Framework
- **MUnit** for test framework
- **ScalaCheck** for property testing
- Test fixtures in separate `FixturesImpl` (not library code)

### Running Tests

Use the MCP server to:
1. Run all tests for a specific module
2. Run specific test suites
3. Check test results and failures

DO NOT use SBT commands like `sbt test` or `sbt quick-test`.

### Test Organization
- Tests for newest Scala versions catch regressions
- Coverage tracking on JVM only
- Sandwich tests verify cross-compilation between Scala 2 ↔ 3

## Common Tasks

### Adding a New Feature

1. Read `CONTRIBUTING.md` for detailed guidelines
2. Check if similar features exist in the codebase
3. Plan implementation for both Scala 2 and 3
4. Create abstract traits in shared code
5. Implement version-specific code in `scala-2/` and `scala-3/`
6. Add tests in `hearth-tests`
7. Use MCP server to compile and test
8. Update documentation if needed

### Fixing a Bug

1. Reproduce the issue if possible
2. Locate the bug in shared or version-specific code
3. Fix in both Scala 2 and 3 implementations if needed
4. Add regression test
5. Verify with MCP server
6. DO NOT commit - let the user review and commit

### Fixing Scala 2 Cross-Quotes

1. Print the AST of the invalid code in the `.tap { ... }` section of `CrossQuotesMacros.scala`, right before `.pipe(c.parse(_))`,
   It should use condition like `if (source.contains("invalid code")) { println(s"source: ${source}\nast: {showRawPretty(tree, SyntaxHighlight.plain)}") }`
2. Identify the pattern that prints the incorrect code, and modify `ShowCodePretty2.scala` to print all the cases,
   both correct and invalid ones
3. Analyze the output to see where do they differs (e.g. Symbol flags)
4. Provide a special handling of the invalid cases
5. Test if all code up to hearthTests tests compile and whether their result is correct
6. If not, undo the changes and try to identify the patter and what distincts invalid cases again

### Fixing Scala 3 Cross-Quotes

0. `betterPrinters` module: `ShowCodePretty2.scala` and `ShowCodePretty3.scala` are irrelevant - Cross-Quotes on Scala 3 uses
   a compiler plugin that does not rely on these
1. Consider enabling `log.cross-quotes = true` or `log.cross-quotes = failing-file.scala` or similar in `dev.properties`
2. Read generated diagnostics and use them to figure out what code is being generated by the plugin
3. Try to figure out where are the issues in the generated code

### Refactoring Code

1. Understand the current architecture thoroughly
2. Ensure refactoring maintains cross-version compatibility
3. Keep changes minimal and focused
4. Run full test suite via MCP server
5. Verify no binary compatibility issues (MiMa checks)

## Documentation

### Existing Documentation

- **README.md** - Project introduction
- **CONTRIBUTING.md** - Comprehensive contribution guide (620+ lines)
- **docs/** - MkDocs documentation with Material theme
  - `basic-utilities.md` - Basic macro utilities
  - `better-printers.md` - AST printing utilities
  - `cross-quotes.md` - Cross-platform quoting
  - `micro-fp.md` - FP utilities
  - `debug-utilities.md` - Debugging tools
  - `source-utilities.md` - Source code utilities
  - `best-practices.md` - Best practices guide
  - `faq.md` - Frequently asked questions

### When to Update Documentation

Update documentation when:
- Adding new public APIs
- Changing existing behavior
- Adding new modules or significant features
- Fixing documentation errors

DO NOT create new documentation files unless explicitly requested.

## Binary Compatibility

The project uses **MiMa** (Migration Manager) for binary compatibility checking.

When making changes:
- Be aware of binary compatibility constraints
- Breaking changes require major version bumps
- MiMa checks run automatically via CI
- Consult MCP server for compatibility reports

## CI/CD Pipeline

The GitHub Actions CI pipeline includes:
- **Formatting checks** - Scalafmt compliance
- **Snippet validation** - Scala CLI snippets in docs
- **Matrix testing** - 2 Scala versions × 3 platforms × 4 JDK versions
- **Code coverage** - JVM only
- **Binary compatibility** - MiMa checks

Agents should be aware of these checks but NOT attempt to run them locally.

## Common Pitfalls to Avoid

1. **Running SBT commands** - Always use MCP server instead
2. **Committing changes** - Prepare changes, don't commit them
3. **Breaking cross-compilation** - Always test both Scala 2 and 3
4. **Modifying `dev.properties`** - Local config, never commit
5. **Using braceless syntax** - Always use braces
6. **Adding external dependencies** - Not allowed
7. **Ignoring platform differences** - Consider JVM, JS, Native
8. **Over-engineering** - Follow Pareto principle
9. **Bypassing MCP server** - No direct compilation/testing

## Getting Help

If you encounter issues or need clarification:

1. **Check existing documentation:**
   - Read `CONTRIBUTING.md` thoroughly
   - Review docs in `docs/` directory
   - Look at similar code in the codebase

2. **Ask the user:**
   - For architectural decisions
   - When multiple valid approaches exist
   - If MCP server is unavailable

3. **Report issues:**
   - GitHub Issues: https://github.com/MateuszKubuszok/hearth/issues
   - GitHub Discussions: https://github.com/MateuszKubuszok/hearth/discussions

## Summary - Quick Reference

| Task | How to Do It | What NOT to Do |
|------|--------------|----------------|
| Compile code | Use MCP server | ❌ `sbt compile` |
| Run tests | Use MCP server | ❌ `sbt test` |
| Check diagnostics | Use MCP server | ❌ `sbt compile` |
| View git status | `git status` | ✅ OK |
| Commit changes | Ask user to commit | ❌ `git commit` |
| Push changes | Ask user to push | ❌ `git push` |
| Format code | Scalafmt (via MCP) | ✅ OK |
| Add dependency | Don't - not allowed | ❌ Any external deps |
| Modify dev.properties | For local dev only | ❌ Never commit |

## File Reference Quick Links

Key files to consult:
- [CONTRIBUTING.md](CONTRIBUTING.md) - Comprehensive contribution guidelines
- [README.md](README.md) - Project overview
- [build.sbt](build.sbt) - Build configuration
- [dev.properties](dev.properties) - Local IDE settings
- [.scalafmt.conf](.scalafmt.conf) - Code formatting rules
- [.metals/mcp.json](.metals/mcp.json) - MCP server configuration
- [docs/mkdocs.yml](docs/mkdocs.yml) - Documentation configuration

---

**Remember:** The goal is to help developers work efficiently with Hearth while respecting the project's architecture, cross-compilation requirements, and workflow constraints. When in doubt, ask the user rather than making assumptions or bypassing established practices.
