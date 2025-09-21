package assignment2.ast;

public interface ExprVisitor<R> {
    R visitVar(VarExpr e) throws Exception;
    R visitInt(IntLitExpr e) throws Exception;
    R visitStr(StrLitExpr e) throws Exception;
    R visitBool(BoolLitExpr e) throws Exception;
    R visitUnary(UnaryExpr e) throws Exception;
    R visitBinary(BinaryExpr e) throws Exception;
    R visitTernary(TernaryExpr e) throws Exception;
}
