package assignment2.ast;

public class AssignStmt extends Stmt {
    private final String varName; private final Expr value;
    public AssignStmt(String varName, Expr value){ this.varName=varName; this.value=value; }
    public String getVarName(){ return varName; }
    public Expr getValue(){ return value; }
    @Override public <R> R accept(StmtVisitor<R> v) throws Exception { return v.visitAssign(this); }
}
