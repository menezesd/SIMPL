package assignment3.symbol

import assignment3._
import assignment3.ast.{Diag, SyntaxDiag, TypeDiag, ResolveDiag, Result}
import edu.utexas.cs.sam.io.{SamTokenizer, Tokenizer}
import Tokenizer.TokenType
import scala.collection.mutable

/** Scala port of first-pass symbol table builder. */
final class SymbolTableBuilder {
  private val program = new ProgramSymbols()

  // Diagnostic-first helpers
  private inline def ok[A](a: A): Result[A] = Right(a)
  private inline def err[A](d: Diag): Result[A] = Left(d)
  private inline def syntax[A](msg: String, tz: SamTokenizer): Result[A] = Left(SyntaxDiag(msg, tz.lineNo(), CompilerUtils.column(tz)))
  private inline def typeE[A](msg: String, line: Int, col: Int): Result[A] = Left(TypeDiag(msg, line, col))

  // Safe wrappers around CompilerUtils throwers
  private def expectCharR(tz: SamTokenizer, ch: Char): Result[Unit] =
    try { CompilerUtils.expectChar(tz, ch, tz.lineNo()); ok(()) }
    catch case se: SyntaxErrorException => Left(SyntaxDiag(se.getMessage, se.line, se.column))

  private def expectWordR(tz: SamTokenizer, word: String): Result[Unit] =
    try { CompilerUtils.expectWord(tz, word, tz.lineNo()); ok(()) }
    catch case se: SyntaxErrorException => Left(SyntaxDiag(se.getMessage, se.line, se.column))

  private def getIdentifierR(tz: SamTokenizer): Result[String] =
    try ok(CompilerUtils.getIdentifier(tz))
    catch case se: SyntaxErrorException => Left(SyntaxDiag(se.getMessage, se.line, se.column))

  private def getWordR(tz: SamTokenizer): Result[String] =
    try ok(CompilerUtils.getWord(tz))
    catch case se: SyntaxErrorException => Left(SyntaxDiag(se.getMessage, se.line, se.column))

  private def isTypeToken(tz: SamTokenizer, current: ClassSymbol): Boolean =
    CompilerUtils.isTypeWord(
      tz,
      program,
      current.getName,
      /*allowUnknownNames*/ true,
      /*excludeStmtStarters*/ true
    )

  private def buildR(tokenizer: SamTokenizer): Result[ProgramSymbols] =
    def outer(): Result[Unit] =
      if (tokenizer.peekAtKind() != TokenType.EOF) then
        parseClassR(tokenizer).flatMap(_ => outer())
      else ok(())
    for
      _ <- outer()
      _ <- validateTypesR()
    yield program

  /** Diagnostic-first variant: never throws; returns Either[Diag, ProgramSymbols]. */
  def buildD(tokenizer: SamTokenizer): Either[Diag, ProgramSymbols] = buildR(tokenizer)

  private def parseClassR(tz: SamTokenizer): Result[Unit] =
    for
      _ <- expectWordR(tz, "class")
      className <- getIdentifierR(tz)
      classSym = new ClassSymbol(className)
      _ <-
        if (CompilerUtils.check(tz, '(')) then
          if (!CompilerUtils.check(tz, ')')) then
            def fieldsLoop(): Result[Unit] =
              if (tz.peekAtKind() != TokenType.WORD) then
                if (CompilerUtils.check(tz, ')')) ok(()) else syntax(") expected after field declarations", tz)
              else
                for
                  rawType <- getWordR(tz)
                  vt = CompilerUtils.parseTypeOrObjectName(rawType, tz.lineNo())
                  varTypeOpt = if (vt.isPrimitive) Some(vt.getPrimitive) else None
                  classRefOpt = if (vt.isObject) Some(vt.getObject.getClassName) else None
                  name <- getIdentifierR(tz)
                  line = tz.lineNo(); col = CompilerUtils.column(tz)
                  valueType = classRefOpt.map(ValueType.ofObject).orElse(varTypeOpt.map(ValueType.ofPrimitive)).get
                  _ = classSym.addField(new VarSymbol(name, valueType, false, -1, line, col))
                  _ <-
                    def moreLoop(): Result[Unit] =
                      if (CompilerUtils.check(tz, ',')) then
                        for
                          n2 <- getIdentifierR(tz)
                          line2 = tz.lineNo(); col2 = CompilerUtils.column(tz)
                          vt2 = classRefOpt.map(ValueType.ofObject).orElse(varTypeOpt.map(ValueType.ofPrimitive)).get
                          _ = classSym.addField(new VarSymbol(n2, vt2, false, -1, line2, col2))
                          _ <- moreLoop()
                        yield ()
                      else ok(())
                    moreLoop()
                  _ <- if (!CompilerUtils.check(tz, ';')) then syntax("Expected ';' in field declaration", tz) else ok(())
                  res <- if (CompilerUtils.check(tz, ')')) then ok(()) else fieldsLoop()
                yield res
            fieldsLoop()
          else ok(())
        else ok(())
      _ <- if (!CompilerUtils.check(tz, '{')) then syntax(s"Expected '{' after class header for class '$className'", tz) else ok(())
      _ <-
        def methodsLoop(): Result[Unit] =
          if (!CompilerUtils.check(tz, '}')) then parseMethodR(tz, classSym).flatMap(_ => methodsLoop()) else ok(())
        methodsLoop()
      _ = program.addClass(classSym)
    yield ()

  private def parseMethodR(tz: SamTokenizer, classSym: ClassSymbol): Result[Unit] =
    for
      typeWord <- getWordR(tz)
      rtLine = tz.lineNo(); rtCol = CompilerUtils.column(tz)
      (returnTypeOpt, classReturnTypeNameOpt) =
        if (typeWord == "void") (None, None)
        else
          try (Some(Type.parse(typeWord, tz.lineNo())), None)
          catch case _: TypeErrorException => (Some(Type.INT), Some(typeWord))
      name <- getIdentifierR(tz)
      method = (returnTypeOpt, classReturnTypeNameOpt) match
        case (None, None) => new MethodSymbol(name, None)
        case (_, Some(className)) => new MethodSymbol(name, Some(ValueType.ofObject(className)))
        case (Some(rt), None) => new MethodSymbol(name, Some(ValueType.ofPrimitive(rt)))
      _ <- if (classSym.method(name).isDefined) then syntax(s"Method '$name' already defined in class '${classSym.getName}'", tz) else ok(())
      _ <- expectCharR(tz, '(')
      _ = method.setReturnTypePosition(rtLine, rtCol)
      _ = method.addParameterObject("this", classSym.getName)
      _ <- if (classSym.getName == "Main" && name == "main" && tz.peekAtKind() == TokenType.WORD)
        then syntax("Main.main method must not have parameters", tz) else ok(())
      _ <-
        def paramsLoop(): Result[Unit] =
          if (isTypeToken(tz, classSym)) then
            for
              rawType <- getWordR(tz)
              vt = CompilerUtils.parseTypeOrObjectName(rawType, tz.lineNo())
              pName <- getIdentifierR(tz)
              line = tz.lineNo(); col = CompilerUtils.column(tz)
              _ = if (vt.isObject) method.addParameterObject(pName, vt.getObject.getClassName, line, col)
                  else method.addParameter(pName, vt.getPrimitive, line, col)
              _ = if (CompilerUtils.check(tz, ',')) () else ()
              _ <- paramsLoop()
            yield ()
          else ok(())
        paramsLoop()
      _ <- expectCharR(tz, ')')
      _ <- expectCharR(tz, '{')
      _ = method.setBodyStartLine(tz.lineNo())
      _ <-
        def localsLoop(): Result[Unit] =
          if (isTypeToken(tz, classSym)) then
            for
              rawType <- getWordR(tz)
              vt = CompilerUtils.parseTypeOrObjectName(rawType, tz.lineNo())
              _ <-
                def inner(more: Boolean = true): Result[Unit] =
                  if (more) then
                    for
                      lName <- getIdentifierR(tz)
                      line = tz.lineNo(); col = CompilerUtils.column(tz)
                      _ = if (vt.isObject) method.addLocalObject(lName, vt.getObject.getClassName, line, col)
                          else method.addLocal(lName, vt.getPrimitive, line, col)
                      res <-
                        if (CompilerUtils.check(tz, ',')) then inner(true)
                        else if (CompilerUtils.check(tz, ';')) then ok(())
                        else syntax("Expected ',' or ';' in variable declaration", tz)
                    yield res
                  else ok(())
                inner()
              _ <- localsLoop()
            yield ()
          else ok(())
        localsLoop()
      _ <- skipBodyRemainderR(tz)
      _ = classSym.addMethod(method)
    yield ()

  private def skipBodyRemainderR(tz: SamTokenizer): Result[Unit] = {
    val stack = new java.util.ArrayDeque[Char]()
    stack.push('{')
    while (!stack.isEmpty && tz.peekAtKind() != TokenType.EOF) {
      if (CompilerUtils.check(tz, '{')) stack.push('{')
      else if (CompilerUtils.check(tz, '}')) stack.pop()
      else CompilerUtils.skipToken(tz)
    }
    if (!stack.isEmpty) Left(SyntaxDiag("Unbalanced braces in method body", tz.lineNo(), CompilerUtils.column(tz))) else Right(())
  }

  private def validateTypesR(): Result[Unit] = {
    import assignment3.ast.high.ReturnSig

    // Helper functions to validate, short-circuiting on first error
    def checkField(cls: ClassSymbol, f: VarSymbol): Result[Unit] =
      if (f.isObject) then
        val cn = f.classTypeNameOpt.getOrElse(f.getClassTypeName)
        if (!program.existsClass(cn)) then err(TypeDiag(s"Unknown type '${cn}' for field '${f.getName}' in class '${cls.getName}'", f.getLine, f.getColumn))
        else ok(())
      else ok(())

    def checkParam(cls: ClassSymbol, m: MethodSymbol, p: VarSymbol): Result[Unit] =
      if (p.isObject) then
        val cn = p.classTypeNameOpt.getOrElse(p.getClassTypeName)
        if (!program.existsClass(cn)) then err(TypeDiag(s"Unknown parameter type '${cn}' for parameter '${p.getName}' in method '${cls.getName}.${m.getName}'", p.getLine, p.getColumn))
        else ok(())
      else ok(())

    def checkLocal(cls: ClassSymbol, m: MethodSymbol, v: VarSymbol): Result[Unit] =
      if (v.isObject) then
        val cn = v.classTypeNameOpt.getOrElse(v.getClassTypeName)
        if (!program.existsClass(cn)) then err(TypeDiag(s"Unknown local type '${cn}' for variable '${v.getName}' in method '${cls.getName}.${m.getName}'", v.getLine, v.getColumn))
        else ok(())
      else ok(())

    def checkMethodReturn(cls: ClassSymbol, m: MethodSymbol): Result[Unit] = m.getReturnSig match
      case ReturnSig.Obj(cn) => if (!program.existsClass(cn)) then err(TypeDiag(s"Unknown return type '${cn}' in method '${cls.getName}.${m.getName}'", m.getReturnTypeLine(), m.getReturnTypeColumn())) else ok(())
      case _ => ok(())

    def loopFields(it: Iterator[VarSymbol], cls: ClassSymbol): Result[Unit] =
      if (!it.hasNext) ok(()) else checkField(cls, it.next()).flatMap(_ => loopFields(it, cls))

    def loopParams(it: Iterator[VarSymbol], cls: ClassSymbol, m: MethodSymbol): Result[Unit] =
      if (!it.hasNext) ok(()) else checkParam(cls, m, it.next()).flatMap(_ => loopParams(it, cls, m))

    def loopLocals(it: Iterator[VarSymbol], cls: ClassSymbol, m: MethodSymbol): Result[Unit] =
      if (!it.hasNext) ok(()) else checkLocal(cls, m, it.next()).flatMap(_ => loopLocals(it, cls, m))

  def loopMethods(it: Iterator[MethodSymbol], cls: ClassSymbol): Result[Unit] =
      if (!it.hasNext) ok(())
      else
        val m = it.next()
        checkMethodReturn(cls, m)
          .flatMap(_ => loopParams(m.parameters.iterator, cls, m))
          .flatMap(_ => loopLocals(m.locals.iterator, cls, m))
          .flatMap(_ => loopMethods(it, cls))

  def loopClasses(it: Iterator[ClassSymbol]): Result[Unit] =
      if (!it.hasNext) ok(())
      else
        val cls = it.next()
        loopFields(cls.allFields.iterator, cls)
          .flatMap(_ => loopMethods(cls.allMethods.iterator, cls))
          .flatMap(_ => loopClasses(it))

    loopClasses(program.allClasses.iterator)
  }
}
