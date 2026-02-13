# AI Agent Guidelines for Hearth

This document provides guidelines for AI agents (like Claude Code, GitHub Copilot, Cursor, etc.) working with the Hearth codebase.

## Project Overview

**Hearth** is the first Scala macros' standard library, designed to make macro development easier and more maintainable across Scala 2.13 and Scala 3.

- **Language**: Scala (pure Scala library)
- **Scala Versions**: 2.13.16, 3.3.7 (primary); 2.13.18, 3.8.1 (regression testing)
- **Platforms**: JVM, Scala.js, Scala Native
- **Build Tool**: SBT (but see restrictions below)
- **License**: Apache 2.0

### Repository Structure

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
├── guidelines/                      # Instructions for contributors or AI agents
├── project/                         # SBT build configuration
├── scripts/                         # Utility scripts
│
├── build.sbt                        # Main build configuration
├── dev.properties                   # Local IDE settings (DO NOT COMMIT)
├── CONTRIBUTING.md                  # Contribution guidelines (620+ lines)
└── README.md                        # Project introduction
```

### Module dependencies

**Published modules:**
1. `hearth-better-printers` - Base printer utilities
2. `hearth-cross-quotes` - Core cross-platform quoting (depends on `hearth-better-printers`)
3. `hearth-micro-fp` - FP utilities (no dependencies)
4. `hearth` - Main library (depends on `hearth-micro-fp` and `hearth-better-printers`)

**Test modules:**
- `hearth-tests` - Main test suite (depends on `hearth`)
- `hearth-sandwich-tests` - Cross-compilation validation (depends on `hearth-tests`, both `compile` and `test` config)

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

## Global rules

 - ✅ **allowed operations**:

    ```bash
    git status
    git diff
    git log
    git branch
    git show
    sbt --client "..."
    ```

 - ❌ **forbidden operations**:

    ```bash
    git commit
    git commit -m "message"
    git commit --amend
    git push
    git push --force
    git rebase
    git merge
    git reset --hard
    sbt          # bare sbt without --client is forbidden
    ```

 - **prefer working with MCP server** when available, it allows for:

    - **Compilation**: Request compilation through MCP tools
    - **Testing**: Run tests through MCP tools
    - **Diagnostics**: Get compiler errors, warnings, and type information
    - **Code navigation**: Find definitions, references, implementations
    - **Type information**: Query types at specific positions

 - **use `sbt --client` when MCP is insufficient** — e.g. when the task requires both Scala 2 and Scala 3
   (MCP exposes only 1 version at a time), or when MCP is down. Always use `--client` flag as sbt startup
   is expensive and kills the feedback loop — **never run bare `sbt` without `--client`**
 - **do not modify `dev.properties`** - it's primarily used by the developer to focus on working on one platform in their IDE without the issues related to shared sources
   (IDE not being able to identify whether source file should be treated as a part of Scala 2 project or Scala 3 project, JVM or JS or Native)

### Finding the MCP Server Address

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

### MCP Server Project Configuration

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

## Quality Assurance

### Binary Compatibility

The project uses **MiMa** (Migration Manager) for binary compatibility checking.

When making changes:
- Be aware of binary compatibility constraints
- Breaking changes require major version bumps
- MiMa checks run automatically via CI
- Consult MCP server for compatibility reports

### CI/CD Pipeline

The GitHub Actions CI pipeline includes:
- **Formatting checks** - Scalafmt compliance
- **Snippet validation** - Scala CLI snippets in docs
- **Matrix testing** - 2 Scala versions × 3 platforms × 2 JDK versions each
- **Code coverage** - JVM only
- **Binary compatibility** - MiMa checks

Agents should:
- make sure that the code was actually compiled by sbt and that `quick-tests` succeeded
- make sure that new snippets where runnable Scala CLI examples that pass tests
- coverage and MiMa can be skipped
- **before reporting that a task is done**, run `sbt --client "quick-clean ; quick-test"` as the final
  verification step — while MCP speeds up the development loop, `sbt --client` compiles and tests across
  both Scala versions and is the more reliable way to confirm a fix

If an agent was used to generate the code (e.g. following GitHub issue instructions),
but an agent was not able to run the compilation and tests (e.g. because GitHub sandboxing
prevents downloading artifacts), it should inform user that any PR they would attempt to make
will be closed immediatelly.

## Bug Fix Workflow

For the complete bug-fix workflow (reproduce → fix → verify), see [Instruction for fixing a bug](docs/contributing/instruction-for-fixing-a-bug.md).

**Quick reference — clean commands by changed module** (all using `sbt --client`):

| What changed | Clean commands |
|---|---|
| `hearth-better-printers` | `hearthBetterPrinters/clean ; hearthBetterPrinters3/clean ; hearthCrossQuotes/clean ; hearthCrossQuotes3/clean ; hearth/clean ; hearth3/clean ; quick-clean` |
| `hearth-cross-quotes` | `hearthCrossQuotes/clean ; hearthCrossQuotes3/clean ; hearth/clean ; hearth3/clean ; quick-clean` |
| `hearth` | `hearth/clean ; hearth3/clean ; quick-clean` |
| `hearth-tests` only | `quick-clean` |

**Key reminders:**
- Always clean after macro changes — incremental compilation does NOT re-expand macros
- `quick-clean` then `quick-test` is the standard verify cycle
- MCP supports only 1 Scala version at a time — use `sbt --client` for the other version

## Skills

Are available in [contributing documentation](docs/contributing/_index.md).

The bug fix workflow is documented in [instruction for fixing a bug](docs/contributing/instruction-for-fixing-a-bug.md).
