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
        if (!CompilerUtils.check(f, '(')) {
            throw new SyntaxErrorException(
                "populateMethod expects '(' at start of get formals",
                f.lineNo()
            );
        }

        // Init method
        MethodNode method = null;
        if (methodName.equals("main")) {
            // MethodDecl -> Type main() ...
            if (!CompilerUtils.check(f, ')')) {
                throw new SyntaxErrorException(
                    "main method should not receive formals",
                    f.lineNo()
                );
            }
            method = MainMethod.getInstance();
            // Set the main method's return type to the declared return type
            method.setType(returnType);
        } else {
            // create Method object
            method = new MethodNode(methodName, returnType);

            // Save params in symbol table and method object
            populateParams(f, method);

            // MethodDecl -> Type MethodName ( Formals? ) ...
            if (!CompilerUtils.check(f, ')')) {
                throw new SyntaxErrorException(
                    "get method expects ')' at end of get formals",
                    f.lineNo()
                );
            }
        }
        // Save Method in global scope
        globalNode.addChild(method);

        // MethodDecl -> Type MethodName ( Formals? ) { ...
        if (!CompilerUtils.check(f, '{')) {
            throw new SyntaxErrorException(
                "populateMethod expects '{' at start of body",
                f.lineNo()
            );
        }

        // MethodDecl -> Type MethodName ( Formals? ) { Body ...
        populateLocals(f, method);

        // MethodDecl -> Type MethodName ( Formals? ) { Body }
        if (!CompilerUtils.check(f, '}')) {
            throw new SyntaxErrorException(
                "populateMethod expects '}' at end of body",
                f.lineNo()
            );
        }
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

        if (!CompilerUtils.check(f, '{')) {
            throw new SyntaxErrorException(
                "skipBlock expects '{' at start of block",
                f.lineNo()
            );
        }
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
        return sb.toString();
    }

    static String getMethodDecl(SamTokenizer f) throws CompilerException {
        SamBuilder sb = new SamBuilder();

        // MethodDecl -> Type ...
        Type returnType = getType(f);

        // MethodDecl -> Type MethodName ...
        String methodName = getIdentifier(f);

        // Pull method from global scope
        MethodNode method = globalNode.lookupSymbol(
            methodName,
            MethodNode.class
        );
        if (method == null) {
            throw new CompilerException(
                "get method cannot find method " +
                methodName +
                " in symbol table",
                f.lineNo()
            );
        }

        // Valid method, start generating...
        sb.append("\n");
        sb.append(methodName + ":\n");

        // MethodDecl -> Type MethodName (...
        if (!CompilerUtils.check(f, '(')) {
            throw new SyntaxErrorException(
                "get method expects '(' at start of get formals",
                f.lineNo()
            );
        }

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
        if (!CompilerUtils.check(f, ')')) {
            throw new SyntaxErrorException(
                "get method expects ')' at end of get formals",
                f.lineNo()
            );
        }

        // MethodDecl -> Type MethodName ( Formals? ) { ...
        if (!CompilerUtils.check(f, '{')) {
            throw new SyntaxErrorException(
                "get method expects '{' at start of body",
                f.lineNo()
            );
        }

        // MethodDecl -> Type MethodName ( Formals? ) { Body ...
        sb.append(getBody(f, method));

        // MethodDecl -> Type MethodName ( Formals? ) { Body }
        if (!CompilerUtils.check(f, '}')) {
            throw new SyntaxErrorException(
                "get method expects '}' at end of body",
                f.lineNo()
            );
        }

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

        if (!CompilerUtils.check(f, '{')) {
            throw new SyntaxErrorException(
                "getBlock expects '{' at start of block",
                f.lineNo()
            );
        }

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
        if (!CompilerUtils.check(f, "break")) {
            throw new SyntaxErrorException(
                "break statement expects 'break'",
                f.lineNo()
            );
        }

        Label breakLabel = method.mostRecent(LabelType.BREAK);
        if (breakLabel == null) {
            throw new SyntaxErrorException(
                "break statement outside of a loop",
                f.lineNo()
            );
        }

        SamBuilder sb = new SamBuilder();
        sb.append("JUMP " + breakLabel.getName() + "\n");
        return sb.toString();
    }

    static String getReturnStmt(SamTokenizer f, MethodNode method)
        throws CompilerException {
        if (!CompilerUtils.check(f, "return")) {
            throw new SyntaxErrorException(
                "getReturnStmt expects 'return' at beginining",
                f.lineNo()
            );
        }
        SamBuilder sb = new SamBuilder();

        Expression expr = getExpr(f, method);

        // write sam code
        sb.append(expr.getSamCode());

        // Type check: returned expression must be compatible with method return type
        if (!method.getType().isCompatibleWith(expr.getType())) {
            throw new TypeErrorException(
                "Return statement type mismatch: " + method.getType() + " and " + expr.getType(),
                f.lineNo()
            );
        }

        // Jump to clean up
        Label returnLabel = method.mostRecent(LabelType.RETURN);
        if (returnLabel == null) {
            throw new CompilerException(
                "getReturnStmt missing exit label",
                f.lineNo()
            );
        }
        sb.append("JUMP " + returnLabel.getName() + "\n");

        return sb.toString();
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

        sb.append(getExpr(f, method).getSamCode());

        CompilerUtils.expect(f, ')', f.lineNo());

        sb.append("ISNIL\n");
        sb.append("JUMPC " + false_block.getName() + "\n");

        // Truth block:  // if ( Expr ) Block ...
        sb.append(getBlock(f, method));
        sb.append("JUMP " + stop_stmt.getName() + "\n");

        // Checks 'else'
        if (!CompilerUtils.getWord(f).equals("else")) {
            throw new SyntaxErrorException(
                "if statement expects 'else' between expressions",
                f.lineNo()
            );
        }

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
        sb.append(getExpr(f, method).getSamCode());

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

        Expression expr = getExpr(f, method);

        // write sam code
        sb.append(expr.getSamCode());

        // Store item on the stack to Node
        sb.append("STOREOFF " + variable.getAddress() + "\n");

        CompilerUtils.expect(f, ';', f.lineNo());

        return sb.toString();
    }

    static Expression getExpr(SamTokenizer f, MethodNode method)
        throws CompilerException {
        if (CompilerUtils.check(f, '(')) {
            Expression expr = null;

            // Expr -> ( Unop Expr )
            if (f.test('~') || f.test('!')) {
                expr = getUnopExpr(f, method);
            } else {
                // Expr -> ( Expr (...) )
                expr = getExpr(f, method);

                // Raise if Expr -> ( Expr NOT('?' | ')' | Binop) )
                if (f.peekAtKind() != TokenType.OPERATOR) {
                    throw new SyntaxErrorException(
                        "Expr -> Expr (...) expects '?' | ')' | Binop",
                        f.lineNo()
                    );
                }

                // Expr -> ( Expr ) , ends early
                if (CompilerUtils.check(f, ')')) {
                    return expr;
                }

                // Exprt -> (Expr ? Expr : Expr)
                if (CompilerUtils.check(f, '?')) {
                    Expression ternaryExpr = getTernaryExpr(f, method);
                    // The overall expression type becomes the ternary expression's type
                    expr = new Expression(expr.getSamCode() + ternaryExpr.getSamCode(), ternaryExpr.getType());
                }
                // Exprt -> (Expr Binop Expr)
                else {
                    Expression binopExpr = getBinopExpr(f, expr, method);
                    expr = new Expression(expr.getSamCode() + binopExpr.getSamCode(), binopExpr.getType());
                }
            }

            // Check closing ')'
            CompilerUtils.expect(f, ')', f.lineNo());
            return expr;
        } else {
            return getTerminal(f, method);
        }
    }

    static Expression getUnopExpr(SamTokenizer f, MethodNode method)
        throws CompilerException {
        // Not an operator, raise
        if (f.peekAtKind() != TokenType.OPERATOR) {
            throw new TypeErrorException(
                "getUnopExpr expects an OPERATOR",
                f.lineNo()
            );
        }
        char op = CompilerUtils.getOp(f);

        // getExpr() would return "exactly" one value on the stack
        Expression expr = getExpr(f, method);

        /*** Special case
         ***/
        if (op == '~' && expr.getType() == Type.STRING) {
            expr = new Expression(expr.getSamCode() + reverseString(), Type.STRING);
        } /*** Basic cases
         ***/else {
            // Type check
            if (
                op == '~' && expr.getType() != Type.INT && expr.getType() != Type.STRING
            ) {
                throw new TypeErrorException(
                    "Bitwise NOT operation requires INT | STRING operand, but got " +
                    expr.getType(),
                    f.lineNo()
                );
            }

            // apply unop on expression
            expr = new Expression(expr.getSamCode() + getUnop(op), expr.getType());
        }

        return expr;
    }

    static Expression getBinopExpr(
        SamTokenizer f,
        Expression prevExpr,
        MethodNode method
    ) throws CompilerException {
        // parse operator
        if (f.peekAtKind() != TokenType.OPERATOR) {
            throw new TypeErrorException("getBinopExpr expects an OPERATOR", f.lineNo());
        }
        char op = CompilerUtils.getOp(f);
        Expression expr = getExpr(f, method);
        // String repeat
        if (
            op == '*' &&
            ((prevExpr.getType() == Type.STRING && expr.getType() == Type.INT) ||
                (prevExpr.getType() == Type.INT && expr.getType() == Type.STRING))
        ) {
            return new Expression(expr.getSamCode() + repeatString(prevExpr.getType(), expr.getType()), Type.STRING);
        }
        // String concatenation
        if (
            op == '+' &&
            prevExpr.getType() == Type.STRING &&
            expr.getType() == Type.STRING
        ) {
            return new Expression(expr.getSamCode() + concatString(), Type.STRING);
        }
        // String comparison
        if (
            getBinopType(op) == BinopType.COMPARISON &&
            prevExpr.getType() == Type.STRING &&
            expr.getType() == Type.STRING
        ) {
            return new Expression(expr.getSamCode() + compareString(op), Type.BOOL);
        }
        // Assignment in expression context: treat as comparison, return BOOL
        if (op == '=') {
            if (!prevExpr.getType().isCompatibleWith(expr.getType())) {
                throw new TypeErrorException(
                    "Assignment in expression type mismatch: " +
                    prevExpr.getType() +
                    " and " +
                    expr.getType(),
                    f.lineNo()
                );
            }
            return new Expression(expr.getSamCode() + getBinop(op), Type.BOOL);
        }
        // Logical and comparison binops: require same type, return BOOL
        BinopType binopType = getBinopType(op);
        if (binopType == BinopType.BITWISE || binopType == BinopType.COMPARISON) {
            if (!expr.getType().isCompatibleWith(prevExpr.getType())) {
                throw new TypeErrorException(
                    "Binop expr type mismatch: " +
                    prevExpr.getType() +
                    " and " +
                    expr.getType(),
                    f.lineNo()
                );
            }
            if (binopType == BinopType.BITWISE && (prevExpr.getType() != Type.BOOL || expr.getType() != Type.BOOL)) {
                throw new TypeErrorException(
                    "Logical operation '" +
                    op +
                    "' requires BOOL operands, but got " +
                    prevExpr.getType() +
                    " and " +
                    expr.getType(),
                    f.lineNo()
                );
            }
            return new Expression(expr.getSamCode() + getBinop(op), Type.BOOL);
        }
        // Arithmetic binops: require INT, return INT
        if (binopType == BinopType.ARITHMETIC) {
            if (prevExpr.getType() != Type.INT || expr.getType() != Type.INT) {
                throw new TypeErrorException(
                    "Arithmetic operation '" +
                    op +
                    "' requires INT operands, but got " +
                    prevExpr.getType() +
                    " and " +
                    expr.getType(),
                    f.lineNo()
                );
            }
            return new Expression(expr.getSamCode() + getBinop(op), Type.INT);
        }
        // Fallback (should not reach here)
    throw new TypeErrorException("Unknown binop or type error.", f.lineNo());
    }

    static Expression getTernaryExpr(SamTokenizer f, MethodNode method)
        throws CompilerException {
        // Generate sam code
        Label stop_ternary = new Label();
        Label false_expr = new Label();
        SamBuilder sb = new SamBuilder();
        sb.append("ISNIL\n");
        sb.append("JUMPC " + false_expr.getName() + "\n");

        // Truth expression:  (...) ? Expr : (..)
        Expression truthExpr = getExpr(f, method);
        sb.append(truthExpr.getSamCode());
        sb.append("JUMP " + stop_ternary.getName() + "\n");

        // Checks ':'
        if (!CompilerUtils.check(f, ':')) {
            throw new SyntaxErrorException(
                "Ternary expects ':' between expressions",
                f.lineNo()
            );
        }

        // False expression: (...) ? (...) : Expr
        sb.append(false_expr.getName() + ":\n");
        Expression falseExpr = getExpr(f, method);
        sb.append(falseExpr.getSamCode());

        // Type check return
        if (!truthExpr.getType().isCompatibleWith(falseExpr.getType())) {
            throw new TypeErrorException(
                "Ternary expr type mismatch: " +
                truthExpr.getType() +
                " and " +
                falseExpr.getType(),
                f.lineNo()
            );
        }
        // Stop Frame
        sb.append(stop_ternary.getName() + ":\n");

        return new Expression(sb.toString(), truthExpr.getType());
    }

    static Expression getMethodCallExpr(
        SamTokenizer f,
        MethodNode scopeMethod,
        MethodNode callingMethod
    ) throws CompilerException {
        SamBuilder sb = new SamBuilder();
        sb.append("PUSHIMM 0\n"); // return value

        if (!CompilerUtils.check(f, '(')) {
            throw new SyntaxErrorException(
                "getMethodCallExpr expects '(' at the start of actuals",
                f.lineNo()
            );
        }

        sb.append(getActuals(f, scopeMethod, callingMethod));
        sb.append("LINK\n");
        sb.append("JSR " + callingMethod.getName() + "\n");
        sb.append("UNLINK\n");
        sb.append("ADDSP -" + callingMethod.numParameters() + "\n");

        if (!CompilerUtils.check(f, ')')) {
            throw new SyntaxErrorException(
                "getMethodCallExpr expects ')' at the end of actuals",
                f.lineNo()
            );
        }

        return new Expression(sb.toString(), callingMethod.getType());
    }

    static String getActuals(
        SamTokenizer f,
        MethodNode scopeMethod,
        MethodNode callingMethod
    ) throws CompilerException {
        SamBuilder sb = new SamBuilder();
        int paramCount = callingMethod.numParameters();
        int argCount = 0;

        do {
            // check done processing all the actuals
            if (f.test(')')) {
                break;
            }
            // too many actuals provided
            if (argCount > paramCount) {
                throw new SyntaxErrorException(
                    "Too many arguments provided for method '" +
                    callingMethod.getName() +
                    "'. Expected " +
                    paramCount +
                    " but got more.",
                    f.lineNo()
                );
            }

            Expression expr = getExpr(f, scopeMethod);
            VariableNode currParam = callingMethod.parameters.get(argCount);

            // Type check
            if (!expr.getType().isCompatibleWith(currParam.getType())) {
                throw new TypeErrorException(
                    "Argument type mismatch for parameter '" +
                    currParam.getName() +
                    "': expected " +
                    currParam.getType() +
                    " but got " +
                    expr.getType(),
                    f.lineNo()
                );
            }

            // write sam code
            sb.append(expr.getSamCode());

            // value field removed from Expression and VariableNode; nothing to assign here

            argCount++;
        } while (CompilerUtils.check(f, ','));

        // too few actuals provided
        if (argCount < paramCount) {
            throw new SyntaxErrorException(
                "Not enough arguments provided for method '" +
                callingMethod.getName() +
                "'. Expected " +
                paramCount +
                " but got " +
                argCount,
                f.lineNo()
            );
        }

        return sb.toString();
    }

    // getTerminal is now a recursive operation
    static Expression getTerminal(SamTokenizer f, MethodNode method)
        throws CompilerException {
        TokenType type = f.peekAtKind();
        switch (type) {
            // Expr -> Literal -> Num
            case INTEGER:
                int value = CompilerUtils.getInt(f);
                return new Expression(
                    "PUSHIMM " + value + "\n",
                    Type.INT
                );
            // Expr -> Literal -> String
            case STRING:
                String strValue = CompilerUtils.getString(f);
                return new Expression(
                    "PUSHIMMSTR \"" + strValue + "\"\n",
                    Type.STRING
                );
            // Expr -> MethodName | Var | Literal
            case WORD:
                String name = CompilerUtils.getWord(f);

                // Expr -> Literal -> "true" | "false"
                if (name.equals("true")) {
                    return new Expression("PUSHIMM 1\n", Type.BOOL);
                }
                if (name.equals("false")) {
                    return new Expression("PUSHIMM 0\n", Type.BOOL);
                }

                // Expr -> MethodName | Var
                Node node = method.lookupSymbol(name);

                if (node == null) {
                    throw new CompilerException(
                        "getTerminal trying to access symbol that has not been declared: Node " +
                        node,
                        f.lineNo()
                    );
                }

                // Expr -> MethodName ( Actuals )
                if (node instanceof MethodNode) {
                    return getMethodCallExpr(f, method, (MethodNode) node);
                }
                // Expr -> Var
                else if (node instanceof VariableNode) {
                    return new Expression(
                        "PUSHOFF " + node.getAddress() + "\n",
                        node.getType()
                    );
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
        Node variable = method.lookupSymbol(varName);
        if (variable == null) {
            throw new SyntaxErrorException(
                "getVar trying to access variable that has not been declared: Variable" +
                varName,
                f.lineNo()
            );
        }
        return variable;
    }
    /*** HELPERS. Inherit all the helpers from LiveOak0Compiler
     ***/
}
