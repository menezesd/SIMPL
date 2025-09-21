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
        SamBuilder sb = new SamBuilder();
        sb.append("PUSHIMM 0\n");
        sb.append("LINK\n");
        sb.append("JSR main\n");
        sb.append("UNLINK\n");
        sb.append("STOP\n");

        // LiveOak-0
        sb.label("main");
        sb.append(getBody(f));

        // Return whatever on top of the stack
        sb.append("DUP\n");
        sb.append("STOREOFF -1\n");
        sb.append("ADDSP -" + mainMethod.numLocalVariables() + "\n");
        sb.append("RST\n");

        return sb.toString();
    }

    /*** Recursive operations
     ***/
    static String getBody(SamTokenizer f) throws CompilerException {
        SamBuilder sb = new SamBuilder();

        // while start with "int | bool | String"
        while (f.peekAtKind() == TokenType.WORD) {
            sb.append(getVarDecl(f));
        }

        // check EOF
        if (f.peekAtKind() == TokenType.EOF) {
            return sb.toString();
        }

        // Then, get Block
        sb.append(getBlock(f));

        return sb.toString();
    }

    static String getVarDecl(SamTokenizer f) throws CompilerException {
        SamBuilder sb = new SamBuilder();

        // VarDecl -> Type ...
        Type varType = getType(f);

        // while varName = a | b | c | ...
        while (f.peekAtKind() == TokenType.WORD) {
            // VarDecl -> Type Identifier1, Identifier2
            String varName = getIdentifier(f);

            // Check if the variable is already defined in the current scope
            if (mainMethod.existSymbol(varName)) {
                throw new CompilerException(
                    "Variable '" + varName + "' is already defined in this scope",
                    f.lineNo()
                );
            }

            // put variable in symbol table
            VariableNode variable = new VariableNode(varName, varType, false);
            mainMethod.addChild(variable);

            // write sam code
            sb.append("PUSHIMM 0\n");

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

        sb.append("\n");
        return sb.toString();
    }

    static String getBlock(SamTokenizer f) throws CompilerException {
        SamBuilder sb = new SamBuilder();

        if (!CompilerUtils.check(f, '{')) {
            throw new SyntaxErrorException(
                "getBlock expects '{' at start of block",
                f.lineNo()
            );
        }

        // while not "}"
        while (!CompilerUtils.check(f, '}')) {
            sb.append(getStmt(f));
        }

        return sb.toString();
    }

    static String getStmt(SamTokenizer f) throws CompilerException {
        SamBuilder sb = new SamBuilder();

        if (CompilerUtils.check(f, ';')) {
            return ""; // Null statement
        }

        if (f.peekAtKind() != TokenType.WORD) {
            throw new SyntaxErrorException(
                "getStmt expects TokenType.WORD at beginning of statement",
                f.lineNo()
            );
        }

        if (f.test("if")) {
            sb.append(getIfStmt(f));
        } else if (f.test("while")) {
            sb.append(getWhileStmt(f));
        } else {
            sb.append(getVarStmt(f));
        }

        return sb.toString();
    }

    static String getIfStmt(SamTokenizer f) throws CompilerException {
        CompilerUtils.expect(f, "if", f.lineNo());

        SamBuilder sb = new SamBuilder();

        // labels used
        Label stop_stmt = new Label();
        Label false_block = new Label();

        // if ( Expr ) ...
        CompilerUtils.expect(f, '(', f.lineNo());

        sb.append(getExpr(f).getSamCode());

        CompilerUtils.expect(f, ')', f.lineNo());

        sb.append("ISNIL\n");
        sb.append("JUMPC " + false_block.getName() + "\n");

        // Truth block:  // if ( Expr ) Block ...
        sb.append(getBlock(f));
        sb.append("JUMP " + stop_stmt.getName() + "\n");

        // Checks 'else'
        if (!CompilerUtils.getWord(f).equals("else")) {
            throw new SyntaxErrorException(
                "if statement expects 'else' between expressions",
                f.lineNo()
            );
        }

        // False block: (...) ? (...) : Expr
        sb.append(false_block + ":\n");
        sb.append(getBlock(f));

        // Done if statement
        sb.append(stop_stmt + ":\n");

        return sb.toString();
    }

    static String getWhileStmt(SamTokenizer f) throws CompilerException {
        CompilerUtils.expect(f, "while", f.lineNo());

        SamBuilder sb = new SamBuilder();

        // labels used
        Label start_loop = new Label();
        Label stop_loop = new Label();

        // while ( Expr ) ...
        CompilerUtils.expect(f, '(', f.lineNo());

        sb.append(start_loop.getName() + ":\n");
        sb.append(getExpr(f).getSamCode());

        CompilerUtils.expect(f, ')', f.lineNo());

        sb.append("ISNIL\n");
        sb.append("JUMPC " + stop_loop.getName() + "\n");

        // Continue loop
        sb.append(getBlock(f));
        sb.append("JUMP " + start_loop.getName() + "\n");

        // Stop loop
        sb.append(stop_loop.getName() + ":\n");

        return sb.toString();
    }

    static String getVarStmt(SamTokenizer f) throws CompilerException {
        SamBuilder sb = new SamBuilder();
        Node variable = getVar(f);

        CompilerUtils.expect(f, '=', f.lineNo());

        // getExpr() would return "exactly" one value on the stack
        sb.append(getExpr(f).getSamCode());

        // Store item on the stack to Node
        sb.append("STOREOFF " + variable.getAddress() + "\n");

        CompilerUtils.expect(f, ';', f.lineNo());

        return sb.toString();
    }

    static Expression getExpr(SamTokenizer f) throws CompilerException {
        // TODO: Before getTerminal and getUnopExpr, make sure the FBR on TOS
        // OR: maybe simplify this shit and remove all the JSR

        if (CompilerUtils.check(f, '(')) {
            // Expr -> ( Unop Expr )
            try {
                return getUnopExpr(f);
            } catch (TypeErrorException e) {
                // Expr -> ( Expr (...) )
                Expression expr = getExpr(f);

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
                    expr = new Expression(expr.getSamCode() + getTernaryExpr(f).getSamCode(), expr.getType());
                }
                // Exprt -> (Expr Binop Expr)
                else {
                    expr = new Expression(expr.getSamCode() + getBinopExpr(f, expr).getSamCode(), expr.getType());
                }

                // Check closing ')'
                CompilerUtils.expect(f, ')', f.lineNo());
                return expr;
            }
        }
        // Expr -> Var | Literal
        else {
            return getTerminal(f);
        }
    }

    static Expression getUnopExpr(SamTokenizer f) throws CompilerException {
        // unop sam code
        String unop_sam = getUnop(CompilerUtils.getOp(f));

        // getExpr() would return "exactly" one value on the stack
        Expression expr = getExpr(f);

        // apply unop on expression
        expr = new Expression(expr.getSamCode() + unop_sam, expr.getType());

        return expr;
    }

    static Expression getBinopExpr(SamTokenizer f, Expression prevExpr)
        throws CompilerException {
        // binop sam code
        String binop_sam = getBinop(CompilerUtils.getOp(f));

        // // labels used
        // String binop_label = new Label();

        // // Start Frame
        // sam += binop_label + ":\n";
        // sam += "LINK\n";

        // sam += "PUSHOFF -2\n";
        Expression expr = getExpr(f);

        // Type check
        if (!expr.getType().isCompatibleWith(prevExpr.getType())) {
            throw new TypeErrorException(
                "Binop expr type mismatch: " +
                prevExpr.getType() +
                " and " +
                expr.getType(),
                f.lineNo()
            );
        }

        expr = new Expression(expr.getSamCode() + binop_sam, expr.getType());

        // // Stop Frame
        // sam += "STOREOFF -2\n"; // store result on TOS
        // sam += "UNLINK\n";
        // sam += "RST\n";

        // // Save the method in symbol table
        // int address = CompilerUtils.getNextAddress(symbolTable);
        // Node sam_func = new Node(binop_label, Type.SAM, sam, address);
        // symbolTable.put(binop_label, sam_func);

        return expr;
    }

    static Expression getTernaryExpr(SamTokenizer f) throws CompilerException {
        // Generate sam code
        Expression expr = new Expression();

        // // labels used
        // String start_ternary = new Label();
        Label stop_ternary = new Label();
        Label false_expr = new Label();

        // // Start Frame
        // sam += start_ternary + ":\n";
        // sam += "LINK\n";

        // // Expr ? (...) : (...)
        expr = new Expression(expr.getSamCode() + "ISNIL\n", expr.getType());
        expr = new Expression(expr.getSamCode() + "JUMPC " + false_expr.getName() + "\n", expr.getType());

        // Truth expression:  (...) ? Expr : (..)
        expr = new Expression(expr.getSamCode() + getExpr(f).getSamCode(), expr.getType());
        expr = new Expression(expr.getSamCode() + "JUMP " + stop_ternary.getName() + "\n", expr.getType());

        // Checks ':'
        if (!CompilerUtils.check(f, ':')) {
            throw new SyntaxErrorException(
                "Ternary expects ':' between expressions",
                f.lineNo()
            );
        }

        // False expression: (...) ? (...) : Expr
        expr = new Expression(expr.getSamCode() + false_expr.getName() + ":\n", expr.getType());
        expr = new Expression(expr.getSamCode() + getExpr(f).getSamCode(), expr.getType());

        // Stop Frame
        expr = new Expression(expr.getSamCode() + stop_ternary.getName() + ":\n", expr.getType());

        // // Save the method in symbol table
        // int address = CompilerUtils.getNextAddress(symbolTable);
        // Node sam_func = new Node(start_ternary, Type.SAM, sam, address);
        // symbolTable.put(start_ternary, sam_func);

        return expr;
    }

    /*** Non-recursive operations
     ***/
    static Node getVar(SamTokenizer f) throws CompilerException {
        // Not a var, raise
        if (f.peekAtKind() != TokenType.WORD) {
            throw new SyntaxErrorException(
                "getVar should starts with a WORD",
                f.lineNo()
            );
        }

        String varName = CompilerUtils.getWord(f);

        // Lookup and require variable in current main method scope
        return CompilerUtils.requireVar(mainMethod, varName, f);
    }

    static Type getType(SamTokenizer f) throws CompilerException {
        // typeString = "int" | "bool" | "String"
        String typeString = CompilerUtils.getWord(f);
        Type type = Type.fromString(typeString);

        // typeString != INT | BOOL | STRING
        if (type == null) {
            throw new TypeErrorException(
                "Invalid type: " + typeString,
                f.lineNo()
            );
        }

        return type;
    }

    static Expression getTerminal(SamTokenizer f) throws CompilerException {
        TokenType type = f.peekAtKind();
        switch (type) {
            // Literal -> Num
            case INTEGER:
                int value = CompilerUtils.getInt(f);
                return new Expression("PUSHIMM " + value + "\n", Type.INT);
            // Literal -> String
            case STRING:
                String strValue = CompilerUtils.getString(f);
                return new Expression(
                    "PUSHIMMSTR \"" + strValue + "\"\n",
                    Type.STRING
                );
            case WORD:
                String boolOrVar = CompilerUtils.getWord(f);

                // Literal -> "true" | "false"
                if (boolOrVar.equals("true")) {
                    return new Expression("PUSHIMM 1\n", Type.BOOL);
                }
                if (boolOrVar.equals("false")) {
                    return new Expression("PUSHIMM 0\n", Type.BOOL);
                }

                // Var -> Identifier
                Node variable = CompilerUtils.requireVar(mainMethod, boolOrVar, f);

                return new Expression(
                    "PUSHOFF " + variable.getAddress() + "\n",
                    variable.getType()
                );
            default:
                throw new TypeErrorException(
                    "getTerminal received invalid type " + type,
                    f.lineNo()
                );
        }
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
        "^[a-zA-Z]([a-zA-Z0-9'_'])*$"
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
        if (getBinopType(op) != BinopType.COMPARISON) {
            throw new SyntaxErrorException(
                "compareString receive invalid operation: " + op,
                -1
            );
        }

        // expects parameters (2 strings) already on the stack
        Label enterFuncLabel = new Label();
        Label exitFuncLabel = new Label();
        Label startLoopLabel = new Label();
        Label stopLoopLabel = new Label();

        String sam = "";

        // call method
        sam += "LINK\n";
        sam += "JSR " + enterFuncLabel.getName() + "\n";
        sam += "UNLINK\n";
        sam += "ADDSP -1\n"; // free second param, only first param remain with new value
        sam += "JUMP " + exitFuncLabel.getName() + "\n";

        // method definition
        sam += enterFuncLabel.getName() + ":\n";
        sam += "PUSHIMM 0\n"; // local 2: counter
        sam += "PUSHIMM 0\n"; // local 3: result

        // loop...
        sam += startLoopLabel.getName() + ":\n";
        // reach end of string 1?
        sam += "PUSHOFF -2\n";
        sam += "PUSHOFF 2\n";
        sam += "ADD\n";
        sam += "PUSHIND\n";
        sam += "ISNIL\n";

        // reach end of string 2?
        sam += "PUSHOFF -1\n";
        sam += "PUSHOFF 2\n";
        sam += "ADD\n";
        sam += "PUSHIND\n";
        sam += "ISNIL\n";

        // reach end of both string, is equal
        sam += "AND\n";
        sam += "JUMPC " + stopLoopLabel.getName() + "\n";

        // not end, comparing char by char
        // get char of string 1
        sam += "PUSHOFF -2\n";
        sam += "PUSHOFF 2\n";
        sam += "ADD\n";
        sam += "PUSHIND\n";

        // get char of string 2
        sam += "PUSHOFF -1\n";
        sam += "PUSHOFF 2\n";
        sam += "ADD\n";
        sam += "PUSHIND\n";

        // compare and store result
        sam += "CMP\n";
        sam += "STOREOFF 3\n";

        // check if done
        sam += "PUSHOFF 3\n";
        sam += "JUMPC " + stopLoopLabel.getName() + "\n";

        // not done, continue to next char
        sam += "PUSHOFF 2\n";
        sam += "PUSHIMM 1\n";
        sam += "ADD\n";
        sam += "STOREOFF 2\n";
        sam += "JUMP " + startLoopLabel.getName() + "\n";

        // Stop loop
        sam += stopLoopLabel.getName() + ":\n";
        sam += "PUSHOFF 3\n";
        sam += "STOREOFF -2\n";
        sam += "ADDSP -2\n";
        sam += "RST\n";

        // Exit method
        sam += exitFuncLabel.getName() + ":\n";

        if (op == '<') {
            sam += "PUSHIMM 1\n";
        } else if (op == '>') {
            sam += "PUSHIMM -1\n";
        } else {
            sam += "PUSHIMM 0\n";
        }
        sam += "EQUAL\n";

        return sam;
    }

    public static String reverseString() {
        // expects parameter (1 string) already on the stack
        Label enterFuncLabel = new Label();
        Label exitFuncLabel = new Label();
        Label startLoopLabel = new Label();
        Label stopLoopLabel = new Label();

        String sam = "";

        // call method
        sam += "LINK\n";
        sam += "JSR " + enterFuncLabel.getName() + "\n";
        sam += "UNLINK\n";
        sam += "JUMP " + exitFuncLabel.getName() + "\n";

        // method definition
        sam += enterFuncLabel.getName() + ":\n";
        sam += "PUSHIMM 0\n"; // local 2: counter
        sam += "PUSHIMM 0\n"; // local 3: increment address
        sam += "PUSHIMM 0\n"; // local 4: result

        // get string length and store in local 2
        sam += "PUSHOFF -1\n";
        sam += getStringLength();
        sam += "STOREOFF 2\n";

        // allocate space for resulting string
        sam += "PUSHOFF 2\n";
        sam += "PUSHIMM 1\n";
        sam += "ADD\n";
        sam += "MALLOC\n";
        sam += "STOREOFF 3\n";

        // return this address
        sam += "PUSHOFF 3\n";
        sam += "STOREOFF 4\n";

        // set EOS char first
        sam += "PUSHOFF 3\n";
        sam += "PUSHOFF 2\n";
        sam += "ADD\n";
        sam += "PUSHIMMCH '\\0'" + "\n";
        sam += "STOREIND\n";

        // loop (backward)...
        sam += startLoopLabel.getName() + ":\n";

        // end loop if counter == 0
        sam += "PUSHOFF 2\n";
        sam += "ISNIL\n";
        sam += "JUMPC " + stopLoopLabel.getName() + "\n";

        // get current address
        sam += "PUSHOFF 3\n";

        // get current char
        sam += "PUSHOFF -1\n";
        sam += "PUSHOFF 2\n";
        sam += "ADD\n";
        sam += "PUSHIMM 1\n"; // subtract 1 because indexing
        sam += "SUB\n";
        sam += "PUSHIND\n";

        // store char in address
        sam += "STOREIND\n";

        // increment address
        sam += "PUSHOFF 3\n";
        sam += "PUSHIMM 1\n";
        sam += "ADD\n";
        sam += "STOREOFF 3\n";

        // decrement counter
        sam += "PUSHOFF 2\n";
        sam += "PUSHIMM 1\n";
        sam += "SUB\n";
        sam += "STOREOFF 2\n";

        // Continue loop
        sam += "JUMP " + startLoopLabel.getName() + "\n";

        // Stop loop
        sam += stopLoopLabel.getName() + ":\n";
        sam += "PUSHOFF 4\n";
        sam += "STOREOFF -1\n";
        sam += "ADDSP -3\n";
        sam += "RST\n";

        // Exit method
        sam += exitFuncLabel.getName() + ":\n";

        return sam;
    }
}
