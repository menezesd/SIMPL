package assignment2.ast;

import assignment2.Type;

public abstract class Expr {
    private final Type type;

    protected Expr(Type type) { this.type = type; }
    public Type getType() { return type; }
    public abstract <R> R accept(ExprVisitor<R> visitor) throws Exception;
}
