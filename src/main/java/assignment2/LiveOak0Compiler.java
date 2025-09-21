package assignment2;
import edu.utexas.cs.sam.io.SamTokenizer;
import edu.utexas.cs.sam.io.Tokenizer;
import edu.utexas.cs.sam.io.Tokenizer.TokenType;
import java.io.BufferedWriter;
import java.io.FileWriter;
import java.io.IOException;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.regex.Pattern;
import assignment2.ast.*;

/**
 * LiveOak0 compiler front-end now producing an AST; includes constant folding integration.
 */
public class LiveOak0Compiler {

    public static void main(String[] args) throws IOException {
        if (args.length < 2) {
            System.err.println("Usage: java ProgramName inputFile outputFile");
            System.exit(1);
        }

        String inFileName = args[0];
        String outFileName = args[1];

        try {
            String samCode = compiler(inFileName);
            try (
                BufferedWriter writer = new BufferedWriter(
                    new FileWriter(outFileName)
                )
            ) {
                writer.write(samCode);
            }
        } catch (CompilerException e) {
            System.err.println("Compiler error: " + e.getMessage());
            e.printStackTrace();
        } catch (IOException e) {
            System.err.println("Error processing files: " + e.getMessage());
            e.printStackTrace();
        } catch (Exception e) {
            System.err.println("Unexpected error: " + e.getMessage());
            e.printStackTrace();
        }
    }

    //             globalNode
    //                 |
    //             mainMethod
    //             /         \
    //          local1       local2 ...

    public static Node globalNode = new Node();
    public static MethodNode mainMethod = MainMethod.getInstance();

    static {
        globalNode.addChild(mainMethod);
    }

    public static void reset() {
        CompilerUtils.clearTokens();
        globalNode = new Node();
        MainMethod.resetInstance();
        mainMethod = MainMethod.getInstance();
        globalNode.addChild(mainMethod);
    }

    static String compiler(String fileName) throws Exception {
        reset(); // Clear the list before starting

        //returns SaM code for program in file
        try {
            SamTokenizer f = new SamTokenizer(
                fileName,
                SamTokenizer.TokenizerOptions.PROCESS_STRINGS
            );
            String pgm = getProgram(f);

            return pgm;
        } catch (CompilerException e) {
            String errorMessage = String.format(
                "Failed to compile %s.\nError Message: %s\n",
                fileName,
                e.getMessage()
            );
            System.err.println(errorMessage);
            CompilerUtils.printTokens();
            throw new Error(errorMessage, e);
        } catch (Exception e) {
            String errorMessage = String.format(
                "Failed to compile %s.\nError Message: %s\n",
                fileName,
                e.getMessage()
            );
            System.err.println(errorMessage);
            CompilerUtils.printTokens();
            throw new Error(errorMessage, e);
        }
    }

    static String getProgram(SamTokenizer f) throws CompilerException {
        // Parse entire program body into AST (top-level decls + main block)
        BlockStmt bodyAst = parseBody(f);
        // Generate prologue
        SamBuilder sb = new SamBuilder();
        sb.append("PUSHIMM 0\n");
        sb.append("LINK\n");
        sb.append("JSR main\n");
        sb.append("UNLINK\n");
        sb.append("STOP\n");
        // main label
        sb.label("main");
        // Generate statement code
        CodegenStmtVisitor stmtGen = new CodegenStmtVisitor(mainMethod);
        try {
            sb.append(bodyAst.accept(stmtGen));
        } catch (Exception e) {
            throw new CompilerException("Statement codegen failed: " + e.getMessage(), -1);
        }
        // Return value convention: duplicate TOS into return slot, adjust stack, RST
        sb.append("DUP\n");
        sb.append("STOREOFF -1\n");
        sb.append("ADDSP -" + mainMethod.numLocalVariables() + "\n");
        sb.append("RST\n");
        // Emit string runtime helpers once
        sb.append(StringRuntime.emitAllStringFunctions());
        return sb.toString();
    }

    /*** Recursive operations
     ***/
    // ===== Statement Parsing to AST =====
    private static BlockStmt parseBody(SamTokenizer f) throws CompilerException {
        java.util.ArrayList<Stmt> topLevel = new java.util.ArrayList<>();
        // Parse variable declarations appearing before the main block
        while (f.peekAtKind() == TokenType.WORD && isTypeWord(f)) {
            topLevel.addAll(parseVarDecls(f));
        }
        // If EOF reached, wrap declarations in a block and return
        if (f.peekAtKind() == TokenType.EOF) {
            return new BlockStmt(topLevel);
        }
        // Otherwise parse the main block and append its statements
        BlockStmt block = parseBlock(f);
        topLevel.addAll(block.getStatements());
        return new BlockStmt(topLevel);
    }

    private static boolean isTypeWord(SamTokenizer f) {
        if (f.peekAtKind() != TokenType.WORD) return false;
        String w = f.getWord();
        f.pushBack();
        return w.equals("int") || w.equals("bool") || w.equals("String");
    }

    private static java.util.List<Stmt> parseVarDecls(SamTokenizer f) throws CompilerException {
        Type varType = getType(f);
        java.util.List<Stmt> decls = new java.util.ArrayList<>();
        boolean more;
        do {
            String varName = getIdentifier(f);
            // register variable
            if (mainMethod.existSymbol(varName)) {
                throw new CompilerException("Variable '" + varName + "' already defined", f.lineNo());
            }
            VariableNode variable = new VariableNode(varName, varType, false);
            mainMethod.addChild(variable);
            decls.add(new VarDeclStmt(varName, varType, null));
            more = CompilerUtils.consumeIf(f, ',');
        } while (more);
        CompilerUtils.expectChar(f, ';', f.lineNo());
        return decls;
    }

    private static BlockStmt parseBlock(SamTokenizer f) throws CompilerException {
        CompilerUtils.expectChar(f, '{', f.lineNo());
        java.util.ArrayList<Stmt> stmts = new java.util.ArrayList<>();
        while (!CompilerUtils.check(f, '}')) {
            stmts.add(parseStmt(f));
        }
        return new BlockStmt(stmts);
    }

    private static Stmt parseStmt(SamTokenizer f) throws CompilerException {
        if (CompilerUtils.check(f, ';')) return new BlockStmt(java.util.Collections.emptyList()); // null statement as empty block
        if (f.peekAtKind() != TokenType.WORD) {
            throw new SyntaxErrorException("Statement must begin with identifier/keyword", f.lineNo());
        }
        if (f.test("if")) return parseIf(f);
        if (f.test("while")) return parseWhile(f);
        // assignment
        String ident = CompilerUtils.getWord(f);
        Node var = CompilerUtils.requireVar(mainMethod, ident, f);
        CompilerUtils.expect(f, '=', f.lineNo());
        Expr value = parseExpr(f);
        CompilerUtils.expect(f, ';', f.lineNo());
        return new AssignStmt(ident, value);
    }

    private static Stmt parseIf(SamTokenizer f) throws CompilerException {
        CompilerUtils.expect(f, '(', f.lineNo());
        Expr cond = parseExpr(f);
        CompilerUtils.expect(f, ')', f.lineNo());
        Stmt thenBranch = parseBlock(f);
        CompilerUtils.expectWord(f, "else", f.lineNo());
        Stmt elseBranch = parseBlock(f);
        return new IfStmt(cond, thenBranch, elseBranch);
    }

    private static Stmt parseWhile(SamTokenizer f) throws CompilerException {
        CompilerUtils.expect(f, '(', f.lineNo());
        Expr cond = parseExpr(f);
        CompilerUtils.expect(f, ')', f.lineNo());
        Stmt body = parseBlock(f);
        return new WhileStmt(cond, body);
    }

    static Type getType(SamTokenizer f) throws CompilerException {
        // typeString = "int" | "bool" | "String"
        String typeString = CompilerUtils.getWord(f);
        return Type.parse(typeString, f.lineNo());
    }

    // ===== AST expression helpers =====
    private static Expr parseExpr(SamTokenizer f) throws CompilerException {
        if (CompilerUtils.check(f, '(')) {
            if (f.test('~') || f.test('!')) { // unary
                char op = CompilerUtils.getOp(f);
                Expr inner = parseExpr(f);
                CompilerUtils.expectChar(f, ')', f.lineNo());
                Type t = inner.getType();
                if (op == '~') {
                    if (t != Type.INT && t != Type.STRING) {
                        throw new TypeErrorException("'~' requires INT or STRING", f.lineNo());
                    }
                    return new UnaryExpr(op, inner, (t == Type.STRING) ? Type.STRING : Type.INT);
                } else {
                    if (t != Type.BOOL) throw new TypeErrorException("'!' requires BOOL", f.lineNo());
                    return new UnaryExpr(op, inner, Type.BOOL);
                }
            }
            Expr first = parseExpr(f);
            if (f.peekAtKind() != TokenType.OPERATOR) {
                throw new SyntaxErrorException("Expected operator/?/) after expression", f.lineNo());
            }
            if (CompilerUtils.check(f, ')')) return assignment2.ast.ConstantFolder.fold(first); // grouping
            if (CompilerUtils.check(f, '?')) { // ternary
                Expr thenE = parseExpr(f);
                CompilerUtils.expectChar(f, ':', f.lineNo());
                Expr elseE = parseExpr(f);
                if (!thenE.getType().isCompatibleWith(elseE.getType())) {
                    throw new TypeErrorException("Ternary branch type mismatch", f.lineNo());
                }
                CompilerUtils.expectChar(f, ')', f.lineNo());
                return assignment2.ast.ConstantFolder.fold(new TernaryExpr(first, thenE, elseE, thenE.getType()));
            }
            char op = CompilerUtils.getOp(f);
            Expr second = parseExpr(f);
            CompilerUtils.expectChar(f, ')', f.lineNo());
            return assignment2.ast.ConstantFolder.fold(buildBinary(first, op, second, f));
        }
        return assignment2.ast.ConstantFolder.fold(parseTerminal(f));
    }

    private static Expr parseTerminal(SamTokenizer f) throws CompilerException {
        TokenType tk = f.peekAtKind();
        switch (tk) {
            case INTEGER:
                return new IntLitExpr(CompilerUtils.getInt(f));
            case STRING:
                return new StrLitExpr(CompilerUtils.getString(f));
            case WORD: {
                String w = CompilerUtils.getWord(f);
                if (w.equals("true")) return new BoolLitExpr(true);
                if (w.equals("false")) return new BoolLitExpr(false);
                Node var = CompilerUtils.requireVar(mainMethod, w, f);
                return new VarExpr(w, var.getAddress(), var.getType());
            }
            default:
                throw new SyntaxErrorException("Unexpected token in expression", f.lineNo());
        }
    }

    private static Expr buildBinary(Expr left, char op, Expr right, SamTokenizer f) throws CompilerException {
        Type lt = left.getType();
        Type rt = right.getType();
        if (op == '*' && ((lt == Type.STRING && rt == Type.INT) || (lt == Type.INT && rt == Type.STRING))) return new BinaryExpr(left, op, right, Type.STRING);
        if (op == '+' && lt == Type.STRING && rt == Type.STRING) return new BinaryExpr(left, op, right, Type.STRING);
        if ((op == '<' || op == '>' || op == '=') && lt == Type.STRING && rt == Type.STRING) return new BinaryExpr(left, op, right, Type.BOOL);
        if (op == '=' && lt.isCompatibleWith(rt)) return new BinaryExpr(left, op, right, Type.BOOL);
        if (op == '&' || op == '|') {
            if (lt != Type.BOOL || rt != Type.BOOL) throw new TypeErrorException("Logical op requires BOOL operands", f.lineNo());
            return new BinaryExpr(left, op, right, Type.BOOL);
        }
        if (op == '+' || op == '-' || op == '*' || op == '/' || op == '%') {
            if (lt != Type.INT || rt != Type.INT) throw new TypeErrorException("Arithmetic op requires INT operands", f.lineNo());
            return new BinaryExpr(left, op, right, Type.INT);
        }
        if (op == '<' || op == '>' || op == '=') {
            if (!lt.isCompatibleWith(rt)) throw new TypeErrorException("Comparison requires matching types", f.lineNo());
            return new BinaryExpr(left, op, right, Type.BOOL);
        }
        throw new TypeErrorException("Unsupported operator: " + op, f.lineNo());
    }


    static Node getVar(SamTokenizer f) throws CompilerException {
        if (f.peekAtKind() != TokenType.WORD) {
            throw new SyntaxErrorException("Variable reference must start with identifier", f.lineNo());
        }
        String name = CompilerUtils.getWord(f);
        return CompilerUtils.requireVar(mainMethod, name, f);
    }

    static String getIdentifier(SamTokenizer f) throws CompilerException {
        String identifier = CompilerUtils.getWord(f);
        if (!IDENTIFIER_PATTERN.matcher(identifier).matches()) {
            throw new SyntaxErrorException(
                "Invalid identifier: " + identifier,
                f.lineNo()
            );
        }
        return identifier;
    }

    /*** HELPERS
     ***/
    public static final Pattern IDENTIFIER_PATTERN = Pattern.compile(
        "^[A-Za-z][A-Za-z0-9_]*$"
    );

    public static String getUnop(char op) throws CompilerException {
        return OperatorUtils.getUnop(op);
    }

    public static String getBinop(char op) throws CompilerException {
        return OperatorUtils.getBinop(op);
    }

    public static BinopType getBinopType(char op) throws CompilerException {
        return OperatorUtils.getBinopType(op);
    }

    public static String repeatString(
        Type firstInputType,
        Type secondInputType
    ) {
        return StringRuntime.repeatString(firstInputType, secondInputType);
    }

    public static String getStringLength() {
        return StringRuntime.getStringLength();
    }

    public static String appendStringHeap() {
        return StringRuntime.appendStringHeap();
    }

    public static String concatString() {
        return StringRuntime.concatString();
    }

    public static String compareString(char op) throws CompilerException {
        return StringRuntime.compareString(op);
    }

    public static String reverseString() {
        // delegate to centralized StringRuntime implementation
        return StringRuntime.reverseString();
    }
}
