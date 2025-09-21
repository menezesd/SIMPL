package assignment2.ast;

/** AST node for a break statement */
public class BreakStmt extends Stmt {
	@Override
	public <R> R accept(StmtVisitor<R> v) throws Exception {
		return v.visitBreak(this);
	}
}
