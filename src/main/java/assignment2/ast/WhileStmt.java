package assignment2.ast;

public class WhileStmt extends Stmt {
    private final Expr condition; private final Stmt body;
    public WhileStmt(Expr condition, Stmt body){ this.condition=condition; this.body=body; }
    public Expr getCondition(){ return condition; }
    public Stmt getBody(){ return body; }
    @Override public <R> R accept(StmtVisitor<R> v) throws Exception { return v.visitWhile(this); }
}
