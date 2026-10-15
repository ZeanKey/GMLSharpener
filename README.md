# GMLSharpener

[简体中文](README.zh-CN.md) | **English**

GMLSharpener (GameMaker Language Sharpener) is a GML compiler library built with C# and .NET 8. It can read, parse, and execute a project's plain-text GameMaker Language (GMLv2) code at runtime. It parses source into LINQ expression trees, which can then be compiled into executable code using .NET's expression compilation facilities.

GMLSharpener can export source text as a standalone callable function and also supports exporting nested functions and methods defined within that function.

The project depends on **GMLib**, a separate runtime library that provides instances, script assets, native functions, and other runtime support. Features tied to the game lifecycle, such as rooms, only work within a raylib-cs game application driven by GMLib and require the corresponding game runtime environment.

> This project is under development and does not yet implement all of GMLv2. Existing features also require further verification; full compatibility with GameMaker is not guaranteed.

## How it works

```text
GML source
   ↓ Lexer: lexical analysis
Token sequence
   ↓ ExpressionParser: syntax analysis
LINQ expression tree
   ↓ Compile()
Executable delegate (requires GMLib at runtime)
```

`RValue` wraps runtime values and provides operations, indexing, and member access.

## Development requirements

- .NET 8 SDK.
- A C#/.NET development environment, such as JetBrains Rider.
- The GMLib source and its dependencies.

GMLib is currently referenced through a `ProjectReference` to a sibling directory:

```text
workspace/
├── GMLib/
│   └── GMLib.csproj
└── GMLSharpener/
    ├── GMLSharpener.sln
    └── GMLSharpener.csproj
```

GMLib must be obtained separately; cloning this repository alone is not sufficient to build the library. This repository does not currently pin a compatible GMLib version, so make sure the APIs of both projects match.

## Building

With the directories arranged as shown above, run the following from the GMLSharpener root:

```sh
dotnet restore GMLSharpener.csproj
dotnet build GMLSharpener.csproj --no-restore
```

Alternatively, open `GMLSharpener.sln` in Rider, restore dependencies, and build. To browse and debug GMLib alongside GMLSharpener, you can add GMLib to the solution as an existing project. Each library can remain in its own Git repository.

If the build reports missing project information for GMLib, first check that `../GMLib/GMLib.csproj` exists, then check the dependency restore results. Update the project reference path if you move the source directories.

## Main entry points

Construct `ExpressionParser` with an `IDictionary<string, object?>` representing the global context bound to the parser.

| API | Purpose |
| --- | --- |
| `ParseScript` | Parses a script into an expression tree; the returned lambda can be compiled into a parameterless delegate |
| `ParseObjectEvent` | Parses an object event into an event lambda that receives instance context |
| `ParseExportScriptPreCompile` | Registers function names from an exported script ahead of compilation |
| `ParseExportScript` | Compiles exported functions and updates the registered script assets |

Exported scripts use a two-stage process: register functions during precompilation, then compile and update them. `ParseLambda` is not yet implemented. The host must also prepare the GMLib runtime context required for its use case before executing the resulting code.

## Feature status

The current code includes implementations for variables and expressions, conditionals and loops, functions and constructors, structs, comments, increment and decrement operations, and partial integration with instances and script assets.

Features that still need work include `with`, array declarations, ternary expressions, enums, macros, and some special keywords. See [TODO.md](TODO.md) for development notes, currently in Chinese. Checked items track implementation progress and do not indicate comprehensive compatibility testing.

The repository does not currently include an automated test project or a standalone runnable example. A successful build confirms compilation, not correct GML behavior.

## Repository structure

```text
GMLSharpener/
├── GMLSharpener.sln       # Solution
├── GMLSharpener.csproj    # Class library project and dependency configuration
├── Lexer.cs              # Token definitions and lexical analysis
├── Parser.cs             # Parsing, expression tree generation, and script registration
├── RValue.cs             # Runtime values, operations, and accessors
├── TODO.md               # Feature development notes
├── COPYRIGHT.md          # Copyright notice
└── LICENSE.md            # Full GPL license text
```

## License

GMLSharpener is licensed under the **GNU GPL v3.0 or later**. See [COPYRIGHT.md](COPYRIGHT.md) and [LICENSE.md](LICENSE.md).
