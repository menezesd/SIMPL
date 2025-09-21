package assignment2.ast;

import assignment2.Type;

public class BinaryExpr extends Expr {
    private final char op;
    private final Expr left;
    private final Expr right;
    public BinaryExpr(Expr left, char op, Expr right, Type type) { super(type); this.left = left; this.op = op; this.right = right; }
    public char getOp() { return op; }
    public Expr getLeft() { return left; }
    public Expr getRight() { return right; }
    @Override public <R> R accept(ExprVisitor<R> v) throws Exception { return v.visitBinary(this); }
}
