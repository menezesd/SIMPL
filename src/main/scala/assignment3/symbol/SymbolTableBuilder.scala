package assignment3.symbol

import assignment3._
import edu.utexas.cs.sam.io.{SamTokenizer, Tokenizer}
import Tokenizer.TokenType
import scala.collection.mutable

/** Scala port of first-pass symbol table builder. */
final class SymbolTableBuilder {
  private val program = new ProgramSymbols()

  private def isTypeToken(tz: SamTokenizer, current: ClassSymbol): Boolean =
    CompilerUtils.isTypeWord(
      tz,
      program,
      current.getName,
      /*allowUnknownNames*/ true,
      /*excludeStmtStarters*/ true
    )

  @throws[CompilerException]
  def build(tokenizer: SamTokenizer): ProgramSymbols = {
    while (tokenizer.peekAtKind() != TokenType.EOF) {
      parseClass(tokenizer)
    }
    validateTypes()
    program
  }

  private def parseClass(tz: SamTokenizer): Unit = {
    CompilerUtils.expectWord(tz, "class", tz.lineNo())
    val className = CompilerUtils.getIdentifier(tz)
  val classSym = new ClassSymbol(className)
    if (CompilerUtils.check(tz, '(')) {
      if (!CompilerUtils.check(tz, ')')) {
        var done = false
        while (!done) {
          if (tz.peekAtKind() != TokenType.WORD) {
            if (CompilerUtils.check(tz, ')')) done = true
            else throw new SyntaxErrorException("Expected ')' after field declarations", tz.lineNo())
          } else {
            val rawType = CompilerUtils.getWord(tz)
            val vt = CompilerUtils.parseTypeOrObjectName(rawType, tz.lineNo())
            val varTypeOpt = if (vt.isPrimitive) Some(vt.getPrimitive) else None
            val classRefOpt = if (vt.isObject) Some(vt.getObject.getClassName) else None
            val name = CompilerUtils.getIdentifier(tz)
            val line = tz.lineNo(); val col = CompilerUtils.column(tz)
            classRefOpt match {
              case Some(classRef) => classSym.addField(new VarSymbol(name, classRef, false, -1, line, col))
              case None => classSym.addField(new VarSymbol(name, varTypeOpt.orNull, false, -1, line, col))
            }
            while (CompilerUtils.check(tz, ',')) {
              val n2 = CompilerUtils.getIdentifier(tz)
              val line2 = tz.lineNo(); val col2 = CompilerUtils.column(tz)
              classRefOpt match {
                case Some(classRef) => classSym.addField(new VarSymbol(n2, classRef, false, -1, line2, col2))
                case None => classSym.addField(new VarSymbol(n2, varTypeOpt.orNull, false, -1, line2, col2))
              }
            }
            if (!CompilerUtils.check(tz, ';')) throw new SyntaxErrorException("Expected ';' in field declaration", tz.lineNo())
            if (CompilerUtils.check(tz, ')')) done = true
          }
        }
      }
    }
    if (!CompilerUtils.check(tz, '{')) throw new CompilerException(s"Expected '{' after class header for class '$className'", tz.lineNo())
    while (!CompilerUtils.check(tz, '}')) { parseMethod(tz, classSym) }
    program.addClass(classSym)
  }

  private def parseMethod(tz: SamTokenizer, classSym: ClassSymbol): Unit = {
    val typeWord = CompilerUtils.getWord(tz)
    var returnTypeOpt: Option[Type] = None
    var classReturnTypeNameOpt: Option[String] = None
    val rtLine = tz.lineNo(); val rtCol = CompilerUtils.column(tz)
    if (typeWord == "void") returnTypeOpt = None
    else {
      try returnTypeOpt = Some(Type.parse(typeWord, tz.lineNo()))
      catch { case _: TypeErrorException => returnTypeOpt = Some(Type.INT); classReturnTypeNameOpt = Some(typeWord) }
    }
    val name = CompilerUtils.getIdentifier(tz)
    val method =
      (returnTypeOpt, classReturnTypeNameOpt) match {
        case (None, None) => new MethodSymbol(name, null: ValueType)
        case (_, Some(className)) => new MethodSymbol(name, className)
        case (Some(rt), None) => new MethodSymbol(name, rt)
      }
    if (classSym.getMethod(name).isDefined)
      throw new CompilerException(s"Method '$name' already defined in class '${classSym.getName}'", tz.lineNo())
    CompilerUtils.expectChar(tz, '(', tz.lineNo())
    method.setReturnTypePosition(rtLine, rtCol)
    method.addParameterObject("this", classSym.getName)
    if (classSym.getName == "Main" && name == "main" && tz.peekAtKind() == TokenType.WORD)
      throw new CompilerException("Main.main method must not have parameters", tz.lineNo())
    // Parameters
    var parsingParams = true
    while (parsingParams && isTypeToken(tz, classSym)) {
      val rawType = CompilerUtils.getWord(tz)
      val vt = CompilerUtils.parseTypeOrObjectName(rawType, tz.lineNo())
      val pName = CompilerUtils.getIdentifier(tz)
      val line = tz.lineNo(); val col = CompilerUtils.column(tz)
      if (vt.isObject) method.addParameterObject(pName, vt.getObject.getClassName, line, col)
      else method.addParameter(pName, vt.getPrimitive, line, col)
      if (!CompilerUtils.check(tz, ',')) parsingParams = false
    }
    CompilerUtils.expectChar(tz, ')', tz.lineNo())
    CompilerUtils.expectChar(tz, '{', tz.lineNo())
    method.setBodyStartLine(tz.lineNo())
    while (isTypeToken(tz, classSym)) {
      val rawType = CompilerUtils.getWord(tz)
      val vt = CompilerUtils.parseTypeOrObjectName(rawType, tz.lineNo())
      var more = true
      while (more) {
        val lName = CompilerUtils.getIdentifier(tz)
        val line = tz.lineNo(); val col = CompilerUtils.column(tz)
        if (vt.isObject) method.addLocalObject(lName, vt.getObject.getClassName, line, col)
        else method.addLocal(lName, vt.getPrimitive, line, col)
        if (CompilerUtils.check(tz, ',')) ()
        else if (CompilerUtils.check(tz, ';')) more = false
        else throw new SyntaxErrorException("Expected ',' or ';' in variable declaration", tz.lineNo())
      }
    }
    skipBodyRemainder(tz)
    classSym.addMethod(method)
  }

  private def skipBodyRemainder(tz: SamTokenizer): Unit = {
    val stack = new java.util.ArrayDeque[Char]()
    stack.push('{')
    while (!stack.isEmpty && tz.peekAtKind() != TokenType.EOF) {
      if (CompilerUtils.check(tz, '{')) stack.push('{')
      else if (CompilerUtils.check(tz, '}')) stack.pop()
      else CompilerUtils.skipToken(tz)
    }
    if (!stack.isEmpty) throw new SyntaxErrorException("Unbalanced braces in method body", tz.lineNo())
  }

  private def validateTypes(): Unit = {
    val clsIter = program.allClasses().iterator()
    while (clsIter.hasNext) {
      val cls = clsIter.next()
      val fIt = cls.allFields.iterator()
      while (fIt.hasNext) {
        val f = fIt.next()
        if (f.isObject && !program.existsClass(f.getClassTypeName))
          throw new TypeErrorException(s"Unknown type '${f.getClassTypeName}' for field '${f.getName}' in class '${cls.getName}'", f.getLine, f.getColumn)
      }
      val mIt = cls.allMethods.iterator()
      while (mIt.hasNext) {
        val m = mIt.next()
        m.getReturnSig match {
          case assignment3.ast.high.ReturnSig.Obj(cn) =>
            if (!program.existsClass(cn))
              throw new TypeErrorException(s"Unknown return type '${cn}' in method '${cls.getName}.${m.getName}'", m.getReturnTypeLine(), m.getReturnTypeColumn())
          case _ => ()
        }
        val pIt = m.getParameters.iterator()
        while (pIt.hasNext) {
          val p = pIt.next()
          if (p.isObject && !program.existsClass(p.getClassTypeName))
            throw new TypeErrorException(s"Unknown parameter type '${p.getClassTypeName}' for parameter '${p.getName}' in method '${cls.getName}.${m.getName}'", p.getLine, p.getColumn)
        }
        val lIt = m.getLocals.iterator()
        while (lIt.hasNext) {
          val v = lIt.next()
          if (v.isObject && !program.existsClass(v.getClassTypeName))
            throw new TypeErrorException(s"Unknown local type '${v.getClassTypeName}' for variable '${v.getName}' in method '${cls.getName}.${m.getName}'", v.getLine, v.getColumn)
        }
      }
    }
  }
}
