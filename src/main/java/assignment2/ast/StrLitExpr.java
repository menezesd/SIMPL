package assignment2.ast;

import assignment2.Type;

public class StrLitExpr extends Expr {
    private final String value;
    public StrLitExpr(String value) { super(Type.STRING); this.value = value; }
    public String getValue() { return value; }
    @Override public <R> R accept(ExprVisitor<R> v) throws Exception { return v.visitStr(this); }
}
