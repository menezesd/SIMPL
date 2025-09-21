package assignment2.ast;

import java.util.List;
import assignment2.MethodNode;
import assignment2.Type;

/** Represents a method call expression. */
public class CallExpr extends Expr {
    private final MethodNode method;
    private final List<Expr> args;

    public CallExpr(MethodNode method, List<Expr> args) {
        super(method.getType());
        this.method = method;
        this.args = args;
    }

    public MethodNode getMethod() { return method; }
    public List<Expr> getArgs() { return args; }

    @Override
    public <R> R accept(ExprVisitor<R> visitor) throws Exception {
        if (visitor instanceof ExtendedExprVisitor) {
            return ((ExtendedExprVisitor<R>) visitor).visitCall(this);
        }
        throw new UnsupportedOperationException("Visitor does not support CallExpr");
    }
}
