package assignment2;


import edu.utexas.cs.sam.io.SamTokenizer;
import edu.utexas.cs.sam.io.Tokenizer;
import edu.utexas.cs.sam.io.Tokenizer.TokenType;
import java.io.BufferedWriter;
import java.io.FileWriter;
import java.io.IOException;
import java.util.ArrayDeque;
import java.util.Deque;
import java.util.HashMap;
import java.util.Map;
import assignment2.ast.CodegenExprVisitor;

public class LiveOak2Compiler extends LiveOak0Compiler {
    // Ensure '=' is treated as a comparison operator in expressions
    public static BinopType getBinopType(char op) throws CompilerException {
        switch (op) {
            case '+':
            case '-':
            case '*':
            case '/':
            case '%':
                return BinopType.ARITHMETIC;
            case '&':
            case '|':
                return BinopType.BITWISE;
            case '>':
            case '<':
            case '=':
                return BinopType.COMPARISON;
            default:
                throw new TypeErrorException("Unknown operator: " + op, -1);
        }
    }

    //             globalNode
    //             /         \
    //       mainMethod      anotherMethod
    //      /         \
    //   local1       local2

    public static Node globalNode = new Node();

    public static void reset() {
        CompilerUtils.clearTokens();
        globalNode = new Node();
        MainMethod.resetInstance();
    }

    static String compiler(String fileName) throws Exception {
        reset(); // Clear the list before starting

        //returns SaM code for program in file
        try {
            SamTokenizer firstPass = new SamTokenizer(
                fileName,
                SamTokenizer.TokenizerOptions.PROCESS_STRINGS
            );
            populateSymbolTable(firstPass);

            SamTokenizer secondPass = new SamTokenizer(
                fileName,
                SamTokenizer.TokenizerOptions.PROCESS_STRINGS
            );
            String program = getProgram(secondPass);

            return program;
        } catch (CompilerException e) {
            String errorMessage = String.format(
                "Failed to compile %s.\nError Message: %s\n",
                fileName,
                e.getMessage()
            );
            System.err.println(errorMessage);
            // CompilerUtils.printTokens();
            throw new Error(errorMessage, e);
        } catch (Exception e) {
            String errorMessage = String.format(
                "Failed to compile %s.\nError Message: %s\n",
                fileName,
                e.getMessage()
            );
            System.err.println(errorMessage);
            // CompilerUtils.printTokens();
            throw new Error(errorMessage, e);
        }
    }

    /*** FIRST PASS: POPULATE SYMBOL TABLE
     ***/
    static void populateSymbolTable(SamTokenizer f) throws CompilerException {
        // First pass: populate symbolTable
        while (f.peekAtKind() != TokenType.EOF) {
            populateMethod(f);
        }
        CompilerUtils.clearTokens();
    }

    static void populateMethod(SamTokenizer f) throws CompilerException {
        // MethodDecl -> Type ...
        Type returnType = getType(f);

        // MethodDecl -> Type MethodName ...
        String methodName = getIdentifier(f);

        // Check if the method is already defined
        if (globalNode.existSymbol(methodName)) {
            throw new CompilerException(
                "Method '" + methodName + "' is already defined",
                f.lineNo()
            );
        }

        // MethodDecl -> Type MethodName (...
        CompilerUtils.expectChar(f, '(', f.lineNo());

        // Init method
        MethodNode method = null;
        if (methodName.equals("main")) {
            // MethodDecl -> Type main() ...
            CompilerUtils.expectChar(f, ')', f.lineNo());
            method = MainMethod.getInstance();
            // Set the main method's return type to the declared return type
            method.setType(returnType);
        } else {
            // create Method object
            method = new MethodNode(methodName, returnType);

            // Save params in symbol table and method object
            populateParams(f, method);

            // MethodDecl -> Type MethodName ( Formals? ) ...
            CompilerUtils.expectChar(f, ')', f.lineNo());
        }
        // Save Method in global scope
        globalNode.addChild(method);

        // MethodDecl -> Type MethodName ( Formals? ) { ...
        CompilerUtils.expectChar(f, '{', f.lineNo());

        // MethodDecl -> Type MethodName ( Formals? ) { Body ...
        populateLocals(f, method);

        // MethodDecl -> Type MethodName ( Formals? ) { Body }
        CompilerUtils.expectChar(f, '}', f.lineNo());
    }

    static void populateParams(SamTokenizer f, MethodNode method)
        throws CompilerException {
        while (f.peekAtKind() == TokenType.WORD) {
            // Formals -> Type ...
            Type formalType = getType(f);

            // Formals -> Type Identifier
            String formalName = getIdentifier(f);

            // Check if the formal has already defined
            if (method.existSymbol(formalName)) {
                throw new CompilerException(
                    "populateParams: Param '" +
                    formalName +
                    "' has already defined",
                    f.lineNo()
                );
            }

            // set Formal as child of MethodNode
            VariableNode paramNode = new VariableNode(
                formalName,
                formalType,
                true
            );
            method.addChild(paramNode);

            if (!CompilerUtils.check(f, ',')) {
                break;
            }
        }
    }

    static void populateLocals(SamTokenizer f, MethodNode method)
        throws CompilerException {
        // while start with "int | bool | String"
        while (f.peekAtKind() == TokenType.WORD) {
            // VarDecl -> Type ...
            Type varType = getType(f);

            // while varName = a | b | c | ...
            while (f.peekAtKind() == TokenType.WORD) {
                // VarDecl -> Type Identifier1, Identifier2
                String varName = getIdentifier(f);

                // Check if the variable is already defined in the current scope
                if (method.existSymbol(varName)) {
                    throw new CompilerException(
                        "populateLocals: Variable '" +
                        varName +
                        "' has already defined in this scope",
                        f.lineNo()
                    );
                }

                // save local variable as child of methodNode
                VariableNode variable = new VariableNode(
                    varName,
                    varType,
                    false
                );
                method.addChild(variable);

                if (CompilerUtils.check(f, ',')) {
                    continue;
                } else if (CompilerUtils.check(f, ';')) {
                    break;
                } else {
                    throw new SyntaxErrorException(
                        "populateLocals: Expected ',' or `;` after each variable declaration",
                        f.lineNo()
                    );
                }
            }
        }

        // MethodDecl -> Type MethodName ( Formals? ) { VarDecl [skipBlock] }
        skipBlock(f);
    }

    static void skipBlock(SamTokenizer f) throws CompilerException {
        // Skip the entire block in first pass
        Deque<Character> stack = new ArrayDeque<>();

        CompilerUtils.expectChar(f, '{', f.lineNo());
        stack.push('{');

        while (stack.size() > 0 && f.peekAtKind() != TokenType.EOF) {
            if (CompilerUtils.check(f, '{')) {
                stack.push('{');
            } else if (CompilerUtils.check(f, '}')) {
                stack.pop();
            } else {
                CompilerUtils.skipToken(f);
            }
        }
        if (stack.size() > 0) {
            throw new SyntaxErrorException(
                "skipBlock missed a closing parentheses",
                f.lineNo()
            );
        }
    }

    /*** SECOND PASS: CODEGEN
     ***/

    static String getProgram(SamTokenizer f) throws CompilerException {
        SamBuilder sb = new SamBuilder();
        sb.append("PUSHIMM 0\n");
        sb.append("LINK\n");
        sb.append("JSR main\n");
        sb.append("UNLINK\n");
        sb.append("STOP\n");

        // LiveOak-2
        while (f.peekAtKind() != TokenType.EOF) {
            sb.append(getMethodDecl(f));
        }

        // Emit shared string runtime functions at the bottom
        sb.append(StringRuntime.emitAllStringFunctions());
        return sb.toString();
    }

    static String getMethodDecl(SamTokenizer f) throws CompilerException {
        SamBuilder sb = new SamBuilder();

        // MethodDecl -> Type ...
        Type returnType = getType(f);

        // MethodDecl -> Type MethodName ...
        String methodName = getIdentifier(f);

        // Pull method from global scope (required)
        MethodNode method = CompilerUtils.requireSymbol(globalNode, methodName, MethodNode.class, f);

        // Valid method, start generating...
        sb.append("\n");
        sb.append(methodName + ":\n");

        // MethodDecl -> Type MethodName (...
        CompilerUtils.expectChar(f, '(', f.lineNo());

        // MethodDecl -> Type MethodName ( Formals? ) ...
        while (f.peekAtKind() == TokenType.WORD) {
            // Formals -> Type ...
            Type formalType = getType(f);

            // Formals -> Type Identifier
            String formalName = getIdentifier(f);

            if (!CompilerUtils.check(f, ',')) {
                break;
            }
        }
        CompilerUtils.expectChar(f, ')', f.lineNo());

        // MethodDecl -> Type MethodName ( Formals? ) { ...
        CompilerUtils.expectChar(f, '{', f.lineNo());

        // MethodDecl -> Type MethodName ( Formals? ) { Body ...
        sb.append(getBody(f, method));

        // MethodDecl -> Type MethodName ( Formals? ) { Body }
        CompilerUtils.expectChar(f, '}', f.lineNo());

        // Check return method at the end
        if (
            method.peekStatement() != Statement.RETURN ||
            !method.hasStatement(Statement.RETURN)
        ) {
            throw new SyntaxErrorException(
                "get method missing return statement",
                f.lineNo()
            );
        }

        return sb.toString();
    }

    /*** Recursive operations. Override all
     ***/
    static String getBody(SamTokenizer f, MethodNode method)
        throws CompilerException {
        SamBuilder sb = new SamBuilder();

        // while start with "int | bool | String"
        while (f.peekAtKind() == TokenType.WORD) {
            // VarDecl will store variable in Hashmap: identifier -> { type: TokenType, relative_address: int }
            sb.append(getVarDecl(f, method));
        }

        // check EOF
        if (f.peekAtKind() == TokenType.EOF) {
            return sb.toString();
        }

        Label cleanupLabel = new Label(LabelType.RETURN);
        method.pushLabel(cleanupLabel);

        // Then, get Block
        sb.append(getBlock(f, method));

        // Cleanup procedure
        sb.append(cleanupLabel.getName() + ":\n");
        sb.append("STOREOFF " + method.returnAddress() + "\n");
        sb.append("ADDSP -" + method.numLocalVariables() + "\n");
        sb.append("RST\n");
        method.popLabel();

        return sb.toString();
    }

    static String getVarDecl(SamTokenizer f, MethodNode method)
        throws CompilerException {
        String sam = "";

        // VarDecl -> Type ...
        Type varType = getType(f);

        // while varName = a | b | c | ...
        while (f.peekAtKind() == TokenType.WORD) {
            // VarDecl -> Type Identifier1, Identifier2
            String varName = getIdentifier(f);

            // write sam code
            sam += "PUSHIMM 0\n";

            if (CompilerUtils.check(f, ',')) {
                continue;
            } else if (CompilerUtils.check(f, ';')) {
                break;
            } else {
                throw new SyntaxErrorException(
                    "Expected ',' or `;` after each variable declaration",
                    f.lineNo()
                );
            }
        }

        return sam;
    }

    static String getBlock(SamTokenizer f, MethodNode method)
        throws CompilerException {
        SamBuilder sb = new SamBuilder();

        CompilerUtils.expectChar(f, '{', f.lineNo());

        while (!CompilerUtils.check(f, '}')) {
            sb.append(getStmt(f, method));
        }

        return sb.toString();
    }

    static String getStmt(SamTokenizer f, MethodNode method)
        throws CompilerException {
        SamBuilder sb = new SamBuilder();

        // Stmt -> ;
        if (CompilerUtils.check(f, ';')) {
            return sb.toString(); // Null statement
        }

        if (f.peekAtKind() != TokenType.WORD) {
            throw new SyntaxErrorException(
                "getStmt expects TokenType.WORD at beginning of statement",
                f.lineNo()
            );
        }

        // Stmt -> break;
        if (f.test("break")) {
            method.pushStatement(Statement.BREAK);
            sb.append(getBreakStmt(f, method));
        }
        // Stmt -> return Expr;
        else if (f.test("return")) {
            // TODO: ONLY 1 return STMT at the end, all other return STMT "jump" to that end
            method.pushStatement(Statement.RETURN);
            sb.append(getReturnStmt(f, method));
            // Stmt -> if (Expr) Block else Block;
        } else if (f.test("if")) {
            method.pushStatement(Statement.CONDITIONAL);
            sb.append(getIfStmt(f, method));
            // Stmt -> while (Expr) Block;
        } else if (f.test("while")) {
            method.pushStatement(Statement.LOOP);
            sb.append(getWhileStmt(f, method));
            // Stmt -> Var = Expr;
        } else {
            method.pushStatement(Statement.ASSIGN);
            sb.append(getVarStmt(f, method));
        }

        return sb.toString();
    }

    static String getBreakStmt(SamTokenizer f, MethodNode method)
        throws CompilerException {
        CompilerUtils.expectWord(f, "break", f.lineNo());

        // Create AST node and generate code using visitor
        assignment2.ast.BreakStmt breakStmt = new assignment2.ast.BreakStmt();
        return generateStmt(breakStmt, method);
    }

    static String getReturnStmt(SamTokenizer f, MethodNode method)
        throws CompilerException {
        CompilerUtils.expectWord(f, "return", f.lineNo());
        
        // Use AST-based parser for return expression
        assignment2.ast.Expr astExpr = parseExprAST(f, method);
        if (!method.getType().isCompatibleWith(astExpr.getType())) {
            throw new TypeErrorException(
                "Return statement type mismatch: " + method.getType() + " and " + astExpr.getType(),
                f.lineNo()
            );
        }

        // Create AST node and generate code using visitor
        assignment2.ast.ReturnStmt returnStmt = new assignment2.ast.ReturnStmt(astExpr);
        return generateStmt(returnStmt, method);
    }

    static String getIfStmt(SamTokenizer f, MethodNode method)
        throws CompilerException {
        CompilerUtils.expect(f, "if", f.lineNo());

        SamBuilder sb = new SamBuilder();

        // labels used
        Label stop_stmt = new Label();
        Label false_block = new Label();

        // if ( Expr ) ...
        CompilerUtils.expect(f, '(', f.lineNo());

    // AST migration: parse condition via new AST path
    assignment2.ast.Expr condAst = parseExprAST(f, method);
    sb.append(generateExpr(condAst));

        CompilerUtils.expect(f, ')', f.lineNo());

        sb.append("ISNIL\n");
        sb.append("JUMPC " + false_block.getName() + "\n");

        // Truth block:  // if ( Expr ) Block ...
        sb.append(getBlock(f, method));
        sb.append("JUMP " + stop_stmt.getName() + "\n");

        // Checks 'else'
        CompilerUtils.expectWord(f, "else", f.lineNo());

        // False block: (...) ? (...) : Expr
        sb.append(false_block.getName() + ":\n");
        sb.append(getBlock(f, method));

        // Done if statement
        sb.append(stop_stmt.getName() + ":\n");

        return sb.toString();
    }

    static String getWhileStmt(SamTokenizer f, MethodNode method)
        throws CompilerException {
        CompilerUtils.expect(f, "while", f.lineNo());

        SamBuilder sb = new SamBuilder();

        // labels used
        Label start_loop = new Label();
        Label stop_loop = new Label(LabelType.BREAK);

        // Push exit label to use for break statement
        method.pushLabel(stop_loop);

        // while ( Expr ) ...
        CompilerUtils.expect(f, '(', f.lineNo());

        sb.append(start_loop.getName() + ":\n");
    assignment2.ast.Expr whileCondAst = parseExprAST(f, method);
    sb.append(generateExpr(whileCondAst));

        CompilerUtils.expect(f, ')', f.lineNo());

        sb.append("ISNIL\n");
        sb.append("JUMPC " + stop_loop.getName() + "\n");

        // Continue loop
        sb.append(getBlock(f, method));
        sb.append("JUMP " + start_loop.getName() + "\n");

        // Stop loop
        sb.append(stop_loop.getName() + ":\n");

        // Pop label when done
        method.popLabel();

        return sb.toString();
    }

    static String getVarStmt(SamTokenizer f, MethodNode method)
        throws CompilerException {
        SamBuilder sb = new SamBuilder();
        Node variable = getVar(f, method);

        CompilerUtils.expect(f, '=', f.lineNo());

    assignment2.ast.Expr assignAst = parseExprAST(f, method);
        sb.append(generateExpr(assignAst));

        // Store item on the stack to Node
        sb.append("STOREOFF " + variable.getAddress() + "\n");

        CompilerUtils.expect(f, ';', f.lineNo());

        return sb.toString();
    }

    // Legacy accessor removed: expressions now handled directly via AST

    // ===== New AST-based expression parsing (migration in progress) =====
    private static assignment2.ast.Expr parseExprAST(SamTokenizer f, MethodNode method) throws CompilerException {
        if (CompilerUtils.check(f, '(')) {
            if (f.test('~') || f.test('!')) { // unary
                char op = CompilerUtils.getOp(f);
                assignment2.ast.Expr inner = parseExprAST(f, method);
                CompilerUtils.expectChar(f, ')', f.lineNo());
                assignment2.Type t = inner.getType();
                if (op == '~') {
                    if (t != assignment2.Type.INT && t != assignment2.Type.STRING) {
                        throw new TypeErrorException("'~' requires INT or STRING", f.lineNo());
                    }
                    return assignment2.ast.ConstantFolder.fold(new assignment2.ast.UnaryExpr(op, inner, (t == assignment2.Type.STRING) ? assignment2.Type.STRING : assignment2.Type.INT));
                } else {
                    if (t != assignment2.Type.BOOL) throw new TypeErrorException("'!' requires BOOL", f.lineNo());
                    return assignment2.ast.ConstantFolder.fold(new assignment2.ast.UnaryExpr(op, inner, assignment2.Type.BOOL));
                }
            }
            assignment2.ast.Expr first = parseExprAST(f, method);
            if (f.peekAtKind() != Tokenizer.TokenType.OPERATOR) {
                throw new SyntaxErrorException("Expected operator/?/) after expression", f.lineNo());
            }
            if (CompilerUtils.check(f, ')')) return assignment2.ast.ConstantFolder.fold(first); // grouping
            if (CompilerUtils.check(f, '?')) { // ternary
                assignment2.ast.Expr thenE = parseExprAST(f, method);
                CompilerUtils.expectChar(f, ':', f.lineNo());
                assignment2.ast.Expr elseE = parseExprAST(f, method);
                if (!thenE.getType().isCompatibleWith(elseE.getType())) {
                    throw new TypeErrorException("Ternary branch type mismatch", f.lineNo());
                }
                CompilerUtils.expectChar(f, ')', f.lineNo());
                return assignment2.ast.ConstantFolder.fold(new assignment2.ast.TernaryExpr(first, thenE, elseE, thenE.getType()));
            }
            char op = CompilerUtils.getOp(f);
            assignment2.ast.Expr second = parseExprAST(f, method);
            CompilerUtils.expectChar(f, ')', f.lineNo());
            return assignment2.ast.ConstantFolder.fold(buildBinaryAST(first, op, second, f));
        }
        return assignment2.ast.ConstantFolder.fold(parseTerminalAST(f, method));
    }

    private static assignment2.ast.Expr parseTerminalAST(SamTokenizer f, MethodNode method) throws CompilerException {
        Tokenizer.TokenType tk = f.peekAtKind();
        switch (tk) {
            case INTEGER:
                return new assignment2.ast.IntLitExpr(CompilerUtils.getInt(f));
            case STRING:
                return new assignment2.ast.StrLitExpr(CompilerUtils.getString(f));
            case WORD: {
                String w = CompilerUtils.getWord(f);
                if (w.equals("true")) return new assignment2.ast.BoolLitExpr(true);
                if (w.equals("false")) return new assignment2.ast.BoolLitExpr(false);
                // Could be method or variable
                Node sym = method.lookupSymbol(w);
                if (sym == null) throw new SyntaxErrorException("Undeclared symbol: " + w, f.lineNo());
                if (sym instanceof MethodNode) {
                    MethodNode callee = (MethodNode) sym;
                    CompilerUtils.expectChar(f, '(', f.lineNo());
                    java.util.ArrayList<assignment2.ast.Expr> args = new java.util.ArrayList<>();
                    int paramCount = callee.numParameters();
                    int argCount = 0;
                    if (!CompilerUtils.check(f, ')')) { // has args
                        do {
                            assignment2.ast.Expr arg = parseExprAST(f, method);
                            if (argCount >= paramCount) {
                                throw new SyntaxErrorException("Too many arguments for method '" + callee.getName() + "'", f.lineNo());
                            }
                            VariableNode formal = callee.parameters.get(argCount);
                            if (!arg.getType().isCompatibleWith(formal.getType())) {
                                throw new TypeErrorException("Argument type mismatch for parameter '" + formal.getName() + "'", f.lineNo());
                            }
                            args.add(arg);
                            argCount++;
                        } while (CompilerUtils.check(f, ','));
                        CompilerUtils.expectChar(f, ')', f.lineNo());
                    }
                    if (argCount != paramCount) {
                        throw new SyntaxErrorException("Incorrect number of arguments for method '" + callee.getName() + "'", f.lineNo());
                    }
                    return new assignment2.ast.CallExpr(callee, args);
                }
                // variable
                VariableNode var = (VariableNode) sym;
                return new assignment2.ast.VarExpr(w, var.getAddress(), var.getType());
            }
            default:
                throw new SyntaxErrorException("Unexpected token in expression", f.lineNo());
        }
    }

    private static assignment2.ast.Expr buildBinaryAST(assignment2.ast.Expr left, char op, assignment2.ast.Expr right, SamTokenizer f) throws CompilerException {
        assignment2.Type lt = left.getType();
        assignment2.Type rt = right.getType();
        if (op == '*' && ((lt == assignment2.Type.STRING && rt == assignment2.Type.INT) || (lt == assignment2.Type.INT && rt == assignment2.Type.STRING))) return new assignment2.ast.BinaryExpr(left, op, right, assignment2.Type.STRING);
        if (op == '+' && lt == assignment2.Type.STRING && rt == assignment2.Type.STRING) return new assignment2.ast.BinaryExpr(left, op, right, assignment2.Type.STRING);
        if ((op == '<' || op == '>' || op == '=') && lt == assignment2.Type.STRING && rt == assignment2.Type.STRING) return new assignment2.ast.BinaryExpr(left, op, right, assignment2.Type.BOOL);
        if (op == '=' && lt.isCompatibleWith(rt)) return new assignment2.ast.BinaryExpr(left, op, right, assignment2.Type.BOOL);
        if (op == '&' || op == '|') {
            if (lt != assignment2.Type.BOOL || rt != assignment2.Type.BOOL) throw new TypeErrorException("Logical op requires BOOL operands", f.lineNo());
            return new assignment2.ast.BinaryExpr(left, op, right, assignment2.Type.BOOL);
        }
        if (op == '+' || op == '-' || op == '*' || op == '/' || op == '%') {
            if (lt != assignment2.Type.INT || rt != assignment2.Type.INT) throw new TypeErrorException("Arithmetic op requires INT operands", f.lineNo());
            return new assignment2.ast.BinaryExpr(left, op, right, assignment2.Type.INT);
        }
        if (op == '<' || op == '>' || op == '=') {
            if (!lt.isCompatibleWith(rt)) throw new TypeErrorException("Comparison requires matching types", f.lineNo());
            return new assignment2.ast.BinaryExpr(left, op, right, assignment2.Type.BOOL);
        }
        throw new TypeErrorException("Unsupported operator: " + op, f.lineNo());
    }

    // Direct codegen helper replacing legacy Expression wrapper
    private static String generateExpr(assignment2.ast.Expr astExpr) throws CompilerException {
        CodegenExprVisitor v = new CodegenExprVisitor();
        try {
            return astExpr.accept(v);
        } catch (CompilerException ce) {
            throw ce;
        } catch (Exception e) {
            throw new CompilerException("AST expression codegen failed: " + e.getMessage(), -1);
        }
    }

    // AST statement code generation helper
    private static String generateStmt(assignment2.ast.Stmt astStmt, MethodNode method) throws CompilerException {
        // Get current break and return labels from the method's label stack
        Label breakLabel = method.mostRecent(LabelType.BREAK);
        Label returnLabel = method.mostRecent(LabelType.RETURN);
        
        assignment2.ast.CodegenStmtVisitor v = new assignment2.ast.CodegenStmtVisitor(method, breakLabel, returnLabel);
        try {
            return astStmt.accept(v);
        } catch (CompilerException ce) {
            throw ce;
        } catch (Exception e) {
            throw new CompilerException("AST statement codegen failed: " + e.getMessage(), -1);
        }
    }

    // Removed legacy getUnopExpr, getBinopExpr, getTernaryExpr: all expression forms now handled by unified AST pipeline.

    static assignment2.ast.Expr getMethodCallExpr(
        SamTokenizer f,
        MethodNode scopeMethod,
        MethodNode callingMethod
    ) throws CompilerException {
        CompilerUtils.expectChar(f, '(', f.lineNo());
        java.util.ArrayList<assignment2.ast.Expr> args = new java.util.ArrayList<>();
        int paramCount = callingMethod.numParameters();
        int argCount = 0;
        if (!CompilerUtils.check(f, ')')) {
            do {
                assignment2.ast.Expr arg = parseExprAST(f, scopeMethod);
                if (argCount >= paramCount) {
                    throw new SyntaxErrorException("Too many arguments for method '" + callingMethod.getName() + "'", f.lineNo());
                }
                VariableNode formal = callingMethod.parameters.get(argCount);
                if (!arg.getType().isCompatibleWith(formal.getType())) {
                    throw new TypeErrorException("Argument type mismatch for parameter '" + formal.getName() + "'", f.lineNo());
                }
                args.add(arg);
                argCount++;
            } while (CompilerUtils.check(f, ','));
            CompilerUtils.expectChar(f, ')', f.lineNo());
        }
        if (argCount != paramCount) {
            throw new SyntaxErrorException("Incorrect number of arguments for method '" + callingMethod.getName() + "'", f.lineNo());
        }
        return new assignment2.ast.CallExpr(callingMethod, args);
    }

    // getActuals removed: handled directly in getMethodCallExpr / parseTerminalAST

    // getTerminal is now a recursive operation
    static assignment2.ast.Expr getTerminal(SamTokenizer f, MethodNode method)
        throws CompilerException {
        TokenType type = f.peekAtKind();
        switch (type) {
            // Expr -> Literal -> Num
            case INTEGER:
                int value = CompilerUtils.getInt(f);
                return new assignment2.ast.IntLitExpr(value);
            // Expr -> Literal -> String
            case STRING:
                String strValue = CompilerUtils.getString(f);
                return new assignment2.ast.StrLitExpr(strValue);
            // Expr -> MethodName | Var | Literal
            case WORD:
                String name = CompilerUtils.getWord(f);

                // Expr -> Literal -> "true" | "false"
                if (name.equals("true")) return new assignment2.ast.BoolLitExpr(true);
                if (name.equals("false")) return new assignment2.ast.BoolLitExpr(false);

                // Expr -> MethodName | Var
                Node node = CompilerUtils.requireVar(method, name, f);

                // Expr -> MethodName ( Actuals )
                if (node instanceof MethodNode) {
                    return getMethodCallExpr(f, method, (MethodNode) node);
                }
                // Expr -> Var
                else if (node instanceof VariableNode) {
                    VariableNode vn = (VariableNode) node;
                    return new assignment2.ast.VarExpr(vn.getName(), vn.getAddress(), vn.getType());
                } else {
                    throw new CompilerException(
                        "getTerminal trying to access invalid symbol: Node " +
                        node,
                        f.lineNo()
                    );
                }
            default:
                throw new TypeErrorException(
                    "getTerminal received invalid type " + type,
                    f.lineNo()
                );
        }
    }

    /*** Non-recursive operations. Override "getVar", inherit the rest from LiveOak0Compiler
     ***/
    static Node getVar(SamTokenizer f, MethodNode method)
        throws CompilerException {
        // Not a var, raise
        if (f.peekAtKind() != TokenType.WORD) {
            throw new SyntaxErrorException(
                "getVar should starts with a WORD",
                f.lineNo()
            );
        }

        String varName = CompilerUtils.getWord(f);
        return CompilerUtils.requireVar(method, varName, f);
    }
    /*** HELPERS. Inherit all the helpers from LiveOak0Compiler
     ***/
}
