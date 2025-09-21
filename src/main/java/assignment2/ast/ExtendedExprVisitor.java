package assignment2.ast;

public interface ExtendedExprVisitor<R> extends ExprVisitor<R> {
    R visitCall(CallExpr c) throws Exception;
}
