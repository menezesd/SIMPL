package assignment2.ast;

public interface StmtVisitor<R> {
    R visitBlock(BlockStmt s) throws Exception;
    R visitVarDecl(VarDeclStmt s) throws Exception;
    R visitAssign(AssignStmt s) throws Exception;
    R visitIf(IfStmt s) throws Exception;
    R visitWhile(WhileStmt s) throws Exception;
}
