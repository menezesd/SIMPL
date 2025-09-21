package assignment2.ast;

public class IfStmt extends Stmt {
    private final Expr condition; private final Stmt thenBranch; private final Stmt elseBranch;
    public IfStmt(Expr condition, Stmt thenBranch, Stmt elseBranch){ this.condition=condition; this.thenBranch=thenBranch; this.elseBranch=elseBranch; }
    public Expr getCondition(){ return condition; }
    public Stmt getThenBranch(){ return thenBranch; }
    public Stmt getElseBranch(){ return elseBranch; }
    @Override public <R> R accept(StmtVisitor<R> v) throws Exception { return v.visitIf(this); }
}
