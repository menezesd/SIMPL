package assignment2.ast;

import assignment2.Type;

public class VarExpr extends Expr {
    private final String name;
    private final int address; // stack offset

    public VarExpr(String name, int address, Type type) {
        super(type);
        this.name = name;
        this.address = address;
    }

    public String getName() { return name; }
    public int getAddress() { return address; }

    @Override
    public <R> R accept(ExprVisitor<R> visitor) throws Exception { return visitor.visitVar(this); }
}
