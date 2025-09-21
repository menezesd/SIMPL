package assignment2.ast;

/** Base class for statements */
public abstract class Stmt {
    public abstract <R> R accept(StmtVisitor<R> v) throws Exception;
}
