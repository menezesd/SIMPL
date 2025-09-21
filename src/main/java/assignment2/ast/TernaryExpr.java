package assignment2.ast;

import assignment2.Type;

public class TernaryExpr extends Expr {
    private final Expr condition;
    private final Expr thenExpr;
    private final Expr elseExpr;
    public TernaryExpr(Expr condition, Expr thenExpr, Expr elseExpr, Type type) { super(type); this.condition = condition; this.thenExpr = thenExpr; this.elseExpr = elseExpr; }
    public Expr getCondition() { return condition; }
    public Expr getThenExpr() { return thenExpr; }
    public Expr getElseExpr() { return elseExpr; }
    @Override public <R> R accept(ExprVisitor<R> v) throws Exception { return v.visitTernary(this); }
}
