package assignment2.ast;

import assignment2.*;

public class CodegenStmtVisitor implements StmtVisitor<String> {
    private final CodegenExprVisitor exprGen = new CodegenExprVisitor();
    private final MethodNode scope; // for variable offsets
    private final Label returnLabel;
    private final Label breakLabel;

    // Main constructor for function/method codegen
    public CodegenStmtVisitor(MethodNode scope) {
        this(scope, null, new Label(LabelType.RETURN));
    }

    // Internal constructor for nested codegen (loops, etc.)
    public CodegenStmtVisitor(MethodNode scope, Label breakLabel, Label returnLabel) {
        this.scope = scope;
        this.breakLabel = breakLabel;
        this.returnLabel = returnLabel;
    }

    @Override
    public String visitBlock(BlockStmt s) throws Exception {
        StringBuilder sb = new StringBuilder();
        for (Stmt st : s.getStatements()) sb.append(st.accept(this));
        return sb.toString();
    }

    @Override
    public String visitVarDecl(VarDeclStmt s) throws Exception {
        // Variable already registered externally with its offset; just emit init
        if (s.getInit() == null) return "PUSHIMM 0\n"; // default
        return s.getInit().accept(exprGen);
    }

    @Override
    public String visitAssign(AssignStmt s) throws Exception {
        VariableNode var = (VariableNode) CompilerUtils.requireVar(scope, s.getVarName(), null);
        String code = s.getValue().accept(exprGen);
        code += "STOREOFF " + var.getAddress() + "\n";
        return code;
    }

    @Override
    public String visitIf(IfStmt s) throws Exception {
        Label elseLbl = new Label(); Label endLbl = new Label();
        String cond = s.getCondition().accept(exprGen);
        StringBuilder sb = new StringBuilder();
        sb.append(cond).append("ISNIL\n").append("JUMPC ").append(elseLbl.getName()).append("\n")
          .append(s.getThenBranch().accept(this))
          .append("JUMP ").append(endLbl.getName()).append("\n")
          .append(elseLbl.getName()).append(":\n")
          .append(s.getElseBranch().accept(this))
          .append(endLbl.getName()).append(":\n");
        return sb.toString();
    }

    @Override
    public String visitWhile(WhileStmt s) throws Exception {
        Label start = new Label();
        Label stop = new Label(LabelType.BREAK);
        StringBuilder sb = new StringBuilder();
        sb.append(start.getName()).append(":\n");
        sb.append(s.getCondition().accept(exprGen));
        sb.append("ISNIL\nJUMPC ").append(stop.getName()).append("\n");
        // Use a new visitor with updated break label
        CodegenStmtVisitor bodyVisitor = new CodegenStmtVisitor(scope, stop, returnLabel);
        sb.append(s.getBody().accept(bodyVisitor));
        sb.append("JUMP ").append(start.getName()).append("\n");
        sb.append(stop.getName()).append(":\n");
        return sb.toString();
    }

    @Override
    public String visitBreak(BreakStmt s) throws Exception {
        if (breakLabel == null) throw new CompilerException("'break' used outside of loop", -1);
        return "JUMP " + breakLabel.getName() + "\n";
    }

    @Override
    public String visitReturn(ReturnStmt s) throws Exception {
        StringBuilder sb = new StringBuilder();
        if (s.getValue() != null) {
            sb.append(s.getValue().accept(exprGen));
        }
        sb.append("JUMP ").append(returnLabel.getName()).append("\n");
        return sb.toString();
    }
}
