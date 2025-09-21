package assignment2.ast;

import assignment2.Label;
import assignment2.OperatorUtils;
import assignment2.StringRuntime;
import assignment2.Type;
import assignment2.TypeErrorException;
import assignment2.CompilerException;
import assignment2.SamBuilder;

/**
 * Expression code generation visitor.
 * Produces SAM code that leaves exactly one value (the expression result) on TOS.
 */
public class CodegenExprVisitor implements ExtendedExprVisitor<String> {

    @Override
    public String visitVar(VarExpr e) {
        return "PUSHOFF " + e.getAddress() + "\n"; // variable already stored at offset
    }

    @Override
    public String visitInt(IntLitExpr e) {
        return "PUSHIMM " + e.getValue() + "\n";
    }

    @Override
    public String visitStr(StrLitExpr e) {
        // NOTE: Not escaping embedded quotes beyond basic assumption (mirrors existing code path)
        return "PUSHIMMSTR \"" + e.getValue() + "\"\n";
    }

    @Override
    public String visitBool(BoolLitExpr e) {
        return e.getValue() ? "PUSHIMM 1\n" : "PUSHIMM 0\n";
    }

    @Override
    public String visitUnary(UnaryExpr e) throws CompilerException {
        String sam;
        try {
            sam = e.getExpr().accept(this);
        } catch (CompilerException ce) {
            throw ce;
        } catch (Exception ex) {
            throw new CompilerException("Failed to generate unary expression: " + ex.getMessage(), -1);
        }
        char op = e.getOp();
        // Special case: string reversal with '~'
        if (op == '~' && e.getExpr().getType() == Type.STRING) {
            sam += StringRuntime.reverseString();
            return sam;
        }
        sam += OperatorUtils.getUnop(op);
        return sam;
    }

    @Override
    public String visitBinary(BinaryExpr e) throws Exception {
        Expr left = e.getLeft();
        Expr right = e.getRight();
        Type lt = left.getType();
        Type rt = right.getType();
        char op = e.getOp();
        // Short-circuit logical AND / OR (only when both operands are BOOL)
        if ((op == '&' || op == '|') && lt == Type.BOOL && rt == Type.BOOL) {
            SamBuilder sb = new SamBuilder();
            if (op == '&') {
                Label falseLbl = new Label();
                Label endLbl = new Label();
                // Evaluate left; if false jump to falseLbl
                sb.append(left.accept(this));
                sb.append("ISNIL\n");
                sb.append("JUMPC ").append(falseLbl.getName()).append("\n");
                // Left was true, evaluate right; if false jump to falseLbl
                sb.append(right.accept(this));
                sb.append("ISNIL\n");
                sb.append("JUMPC ").append(falseLbl.getName()).append("\n");
                // Both true
                sb.append("PUSHIMM 1\n");
                sb.append("JUMP ").append(endLbl.getName()).append("\n");
                sb.label(falseLbl.getName());
                sb.append("PUSHIMM 0\n");
                sb.label(endLbl.getName());
                return sb.toString();
            } else { // op == '|'
                Label needRight = new Label();
                Label falseLbl = new Label();
                Label endLbl = new Label();
                // Evaluate left; if false (==0) jump to needRight, else short-circuit true
                sb.append(left.accept(this));
                sb.append("ISNIL\n");
                sb.append("JUMPC ").append(needRight.getName()).append("\n");
                // Left was true -> result true
                sb.append("PUSHIMM 1\n");
                sb.append("JUMP ").append(endLbl.getName()).append("\n");
                // Need to evaluate right
                sb.label(needRight.getName());
                sb.append(right.accept(this));
                sb.append("ISNIL\n");
                sb.append("JUMPC ").append(falseLbl.getName()).append("\n");
                sb.append("PUSHIMM 1\n");
                sb.append("JUMP ").append(endLbl.getName()).append("\n");
                sb.label(falseLbl.getName());
                sb.append("PUSHIMM 0\n");
                sb.label(endLbl.getName());
                return sb.toString();
            }
        }

        String sam = left.accept(this) + right.accept(this);

        // String operations
        if (lt == Type.STRING || rt == Type.STRING) {
            // Repeat: String * Int or Int * String -> result String
            if (op == '*' && ((lt == Type.STRING && rt == Type.INT) || (lt == Type.INT && rt == Type.STRING))) {
                sam += StringRuntime.repeatString(lt, rt);
                return sam;
            }
            if (op == '+') {
                if (lt == Type.STRING && rt == Type.STRING) {
                    sam += StringRuntime.concatString();
                    return sam;
                }
                throw new TypeErrorException("'+' only defined for String+String or numeric addition", -1);
            }
            if (op == '<' || op == '>' || op == '=') {
                if (lt == Type.STRING && rt == Type.STRING) {
                    sam += StringRuntime.compareString(op);
                    return sam;
                }
                throw new TypeErrorException("String comparison requires both operands String", -1);
            }
            throw new TypeErrorException("Unsupported operator '" + op + "' for String operands", -1);
        }

        // Non-string: delegate to OperatorUtils for binop code
        sam += OperatorUtils.getBinop(op);
        return sam;
    }

    @Override
    public String visitTernary(TernaryExpr e) throws Exception {
        String cond = e.getCondition().accept(this);
        String thenCode = e.getThenExpr().accept(this);
        String elseCode = e.getElseExpr().accept(this);
        Label falseLabel = new Label();
        Label endLabel = new Label();
        SamBuilder sb = new SamBuilder();
        sb.append(cond);
        sb.append("ISNIL\n");
        sb.append("JUMPC " + falseLabel.getName() + "\n");
        sb.append(thenCode);
        sb.append("JUMP " + endLabel.getName() + "\n");
        sb.label(falseLabel.getName());
        sb.append(elseCode);
        sb.label(endLabel.getName());
        return sb.toString();
    }

    @Override
    public String visitCall(CallExpr c) throws Exception {
        // Allocate return slot
        SamBuilder sb = new SamBuilder();
        sb.append("PUSHIMM 0\n");
        // Push actual arguments in order
        for (Expr arg : c.getArgs()) {
            sb.append(arg.accept(this));
        }
        // LINK/JSR/UNLINK pattern with arg cleanup similar to existing LO2 code
        sb.append("LINK\n");
        sb.append("JSR ").append(c.getMethod().getName()).append("\n");
        sb.append("UNLINK\n");
        sb.append("ADDSP -" + c.getMethod().numParameters() + "\n");
        return sb.toString();
    }
}
