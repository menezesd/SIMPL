package assignment2.ast;

/** AST node for a return statement */
public class ReturnStmt extends Stmt {
	private final Expr value;

	public ReturnStmt(Expr value) {
		this.value = value;
	}

	/**
	 * @return the return value expression, or null if none
	 */
	public Expr getValue() {
		return value;
	}

	@Override
	public <R> R accept(StmtVisitor<R> v) throws Exception {
		return v.visitReturn(this);
	}
}
