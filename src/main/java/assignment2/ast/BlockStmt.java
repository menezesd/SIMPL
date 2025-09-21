package assignment2.ast;

import java.util.List;

public class BlockStmt extends Stmt {
    private final List<Stmt> statements;
    public BlockStmt(List<Stmt> statements){ this.statements = statements; }
    public List<Stmt> getStatements(){ return statements; }
    @Override public <R> R accept(StmtVisitor<R> v) throws Exception { return v.visitBlock(this); }
}
