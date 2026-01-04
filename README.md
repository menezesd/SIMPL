# SIMPL — LiveOak3 to SAM Compiler

A modern **Scala 3** compiler implementation that transforms LiveOak3 (object-oriented educational language) programs into SAM (Stack Abstract Machine) assembly code.

## Overview

This compiler demonstrates advanced Scala 3 programming techniques and functional compiler design patterns:

- **Pure Scala 3**: Leverages modern language features (enums, union types, extension methods, context functions)
- **Immutable architecture**: Functional data structures throughout
- **Diagnostic-first error handling**: Type-safe error propagation with `Either[Diag, A]`
- **Modular design**: Separation of parsing, semantic analysis, and code generation phases

## Features

### Language Support
- **Classes & Objects**: User-defined classes with fields and methods
- **Inheritance**: Object-oriented programming with implicit `this`
- **Control Flow**: `if/else`, `while`, `break`, `return`, ternary expressions
- **Expressions**: Binary operations, unary operations, method calls, field access
- **Type System**: Primitives (`int`, `bool`, `String`) and object references
- **String Operations**: Concatenation (`+`), repetition (`*`), comparison

### Compiler Features
- **Symbol Table**: Immutable symbol tables with class, method, and variable tracking
- **Semantic Analysis**: Type checking, name resolution, and scope validation
- **Constant Folding**: Compile-time evaluation of constant expressions
- **Code Generation**: Efficient SAM assembly emission with optimizations
- **Diagnostics**: Precise error reporting with line/column information

## Architecture

### Compilation Pipeline

```
Source Code (.lo)
    ↓
[Tokenizer] (SamTokenizer)
    ↓
[Symbol Table Builder] (SymbolTableBuilder)
    ↓
[Parser] (ProgramParser → AST)
    ↓
[Semantic Analysis] (IdiomaticSemantic)
    ↓
[Constant Folding] (IdiomaticConstFolder)
    ↓
[Code Generation] (IdiomaticCodegen)
    ↓
SAM Assembly (.sam)
```

### Key Modules

- **`CompilerContext`**: Immutable compilation context with symbol table
- **`ProgramParser`**: Recursive descent parser producing high-level AST
- **`SymbolTableBuilder`**: Builds immutable symbol tables from source
- **`IdiomaticSemantic`**: Type checking and semantic validation
- **`IdiomaticCodegen`**: SAM assembly code generation
- **`Result`**: Error handling with diagnostic-first Either monad

### Modern Scala 3 Idioms

- **Enums**: `ValueType`, `BinaryOpCategory`, `CompilationStage`, `Diag`
- **Opaque Types**: `Label`, `FieldOffset`, `StackOffset`
- **Extension Methods**: `Result` extensions, `Code` helpers, `Option.toResult`
- **Context Functions**: `type WithRecorder[A] = RecorderContext ?=> A`
- **Union Types**: Flexible label handling with `String | Label`
- **Inline Methods**: Zero-overhead abstractions
- **Pattern Matching**: Exhaustive matching with compiler verification

## Building

### Prerequisites

- **Java 11+**: Required for Gradle and runtime
- **Gradle 8.8**: Included via wrapper (`./gradlew`)
- **Scala 3.5.0**: Managed by Gradle

### Build Commands

```bash
# Build the entire project
./gradlew build

# Run tests
./gradlew test

# Build JAR (outputs to build/libs/compiler.jar)
./gradlew jar

# Clean build artifacts
./gradlew clean

# Build without running tests
./gradlew build -x test
```

## Usage

### As a Library

```scala
import assignment3.LiveOak3Compiler

// Compile a LiveOak3 source file
val result = LiveOak3Compiler.compileD("path/to/source.lo")

result match
  case CompilationResult.Success(samCode) =>
    println(s"Generated SAM:\n$samCode")
  case CompilationResult.Failure(diag) =>
    println(s"Error: ${diag.message} at ${diag.line}:${diag.column}")
```

### Command Line

```bash
# Compile source.lo to output.sam
java -jar build/libs/compiler.jar source.lo output.sam

# Enable debug output
java -Dliveoak.debug=true -jar build/libs/compiler.jar source.lo output.sam
```

### Debug Flags

Set JVM properties to enable diagnostic output:

```bash
# Enable all debug output
-Dliveoak.debug=true

# Individual debug flags
-Dliveoak.debug.symbols=true   # Print symbol table
-Dliveoak.debug.tokens=true    # Print token stream
-Dliveoak.debug.sam=true       # Print SAM code size
-Dliveoak.debug.stage=true     # Print compilation stages
```

## Testing

### Test Structure

```
src/test/
├── java/assignment3/           # JUnit test harnesses
│   ├── LiveOak3CompilerTest.java
│   ├── StringRuntimeTest.java
│   └── OperatorUtilsTest.java
├── scala/assignment3/          # Scala unit tests
│   ├── SemanticTest.scala
│   ├── TypeUtilsTest.scala
│   └── ResultTest.scala
└── resources/LO-3/             # Test programs
    ├── ValidPrograms/
    └── InvalidPrograms/
```

### Running Tests

```bash
# Run all tests
./gradlew test

# Run specific test class
./gradlew test --tests "assignment3.LiveOak3CompilerTest"

# Run with verbose output
./gradlew test --info

# View test report
open build/reports/tests/test/index.html
```

## Language Specification

### LiveOak3 Syntax

```liveoak
class Main {
  int field;

  void main(Main this) {
    this.field = 42;
    int result = this.compute(10);
    return result;
  }

  int compute(Main this, int x) {
    if (x > 0) {
      return x * 2;
    } else {
      return 0;
    }
  }
}
```

### Entry Point Requirements

- **Class**: Must have class named `Main`
- **Method**: Must have method `void main(Main this)` or `int main(Main this)`
- **Parameters**: No user parameters allowed (only implicit `this`)
- **Validation**: Checked at compile time via `ValidationStage`


## Development

### Project Structure

```
src/main/scala/assignment3/
├── LiveOak3Compiler.scala       # Main compiler entry point
├── CompilerContext.scala        # Immutable compilation context
├── CompilerUtils.scala          # Tokenizer utilities
├── ProgramParser.scala          # Top-level parser
├── ProgramCodegen.scala         # Code generation orchestration
├── ValidationStage.scala        # Entry point validation
├── SamBuilder.scala             # SAM assembly builder
├── StringRuntime.scala          # String operation helpers
├── Types.scala                  # Type system definitions
├── ast/                         # AST definitions and visitors
│   ├── Ast.scala                # Expression and statement nodes
│   ├── AstParser.scala          # AST parser
│   ├── Semantic.scala           # Type checking
│   ├── Codegen.scala            # Code generation
│   ├── TypeUtils.scala          # Type analysis utilities
│   ├── ConstFolder.scala        # Constant folding pass
│   └── high/                    # High-level AST nodes
└── symbol/                      # Symbol table implementation
    ├── ProgramSymbols.scala     # Program-level symbols
    ├── ClassSymbol.scala        # Class definitions
    ├── MethodSymbol.scala       # Method signatures
    ├── VarSymbol.scala          # Variable bindings
    └── SymbolTableBuilder.scala # Symbol table construction
```

### Design Principles

1. **Immutability**: Prefer `val` over `var`, case classes over mutable data
2. **Functional**: Use `map`, `flatMap`, `fold` over loops
3. **Type Safety**: Leverage Scala 3's type system (enums, opaque types, union types)
4. **Error Handling**: Use `Either[Diag, A]` for all fallible operations
5. **Separation of Concerns**: Parser, semantic analysis, and codegen are independent
6. **Diagnostic-First**: All public APIs return `Result[A]` or `CompilationResult`

### Contributing

When adding features or fixing bugs:

1. **Follow existing patterns**: Use `Result` for error handling, enums for ADTs
2. **Add tests**: Include both positive and negative test cases
3. **Update diagnostics**: Provide helpful error messages with line/column info
4. **Document**: Add ScalaDoc comments for public APIs
5. **Run tests**: Ensure `./gradlew test` passes before committing

## References

- **SAM Specification**: Stack Abstract Machine documentation
- **LiveOak Language**: Educational object-oriented language
- **Scala 3 Docs**: https://docs.scala-lang.org/scala3/
- **Gradle**: https://gradle.org/

