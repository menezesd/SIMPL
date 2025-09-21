package assignment2.ast;

import assignment2.Type;

public class UnaryExpr extends Expr {
    private final char op;
    private final Expr expr;
    public UnaryExpr(char op, Expr expr, Type type) { super(type); this.op = op; this.expr = expr; }
    public char getOp() { return op; }
    public Expr getExpr() { return expr; }
    @Override public <R> R accept(ExprVisitor<R> v) throws Exception { return v.visitUnary(this); }
}
