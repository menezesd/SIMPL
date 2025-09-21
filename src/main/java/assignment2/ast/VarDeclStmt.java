package assignment2.ast;

import assignment2.Type;

public class VarDeclStmt extends Stmt {
    private final String name; private final Type type; private final Expr init; // init may be null for default 0
    public VarDeclStmt(String name, Type type, Expr init){ this.name=name; this.type=type; this.init=init; }
    public String getName(){ return name; }
    public Type getVarType(){ return type; }
    public Expr getInit(){ return init; }
    @Override public <R> R accept(StmtVisitor<R> v) throws Exception { return v.visitVarDecl(this); }
}
