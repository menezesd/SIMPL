package assignment2.ast;

import assignment2.Type;

public class IntLitExpr extends Expr {
    private final int value;
    public IntLitExpr(int value) { super(Type.INT); this.value = value; }
    public int getValue() { return value; }
    @Override public <R> R accept(ExprVisitor<R> v) throws Exception { return v.visitInt(this); }
}
