# SIMPL — Live Oak 3 to SAM Compiler
Object-oriented Live Oak 3 compiler that emits SAM assembly. LO-2 support has been removed.

## Gradle Installation

This project includes a Gradle build and JUnit test suite.
You should follow the gradle installation instructions for your system at [https://gradle.org/install/](https://gradle.org/install/)
and make sure you have Java version 11 or later installed as the build file targets Java 11.

## Building Jar

You can build the JAR file with 
```sh
gradle build
```
which will leave the jar file at `build/libs/compiler.jar`

If you have some tests failing and want to build a jar anyway, you can skip tests using
```shell
gradle build -x test
```

On some platforms you may need to use the included `gradlew` or `gradlew.bat` scripts instead of calling `gradle` directly.

## Running Tests

Any IDE with Gradle support can run tests, or use the command line:

```sh
gradle test
```

## Notes

- This repository targets Live Oak 3 only. Legacy LO-2 compiler code has been removed. Some historical LO-2 sample resources may still be present under `src/test/resources/LO-2`, but they are not used by the compiler.
- The compiler entry point for tests is `assignment3.LiveOak3Compiler`.
- Entrypoint policy: zero user parameters are allowed (the entry method may return a value; it need not be void). This is validated during compilation.
- Debugging: set `-Dliveoak.debug=true` when running tests or your app to dump program symbols, recorded tokens, and generated SAM size to stderr.

