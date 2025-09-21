package assignment2.ast;

import assignment2.Type;

public class BoolLitExpr extends Expr {
    private final boolean value;
    public BoolLitExpr(boolean value) { super(Type.BOOL); this.value = value; }
    public boolean getValue() { return value; }
    @Override public <R> R accept(ExprVisitor<R> v) throws Exception { return v.visitBool(this); }
}
