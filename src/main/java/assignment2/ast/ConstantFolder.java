package assignment2.ast;

import assignment2.Type;

/**
 * Simple, local constant folding pass over the expression AST.
 * Only folds when every operand of an operation is a literal and the
 * operation is side‑effect free and safe (e.g. no div-by-zero).
 *
 * It is deliberately conservative: anything even slightly questionable is
 * left in non-folded form and handled by normal codegen.
 */
public final class ConstantFolder {
    private ConstantFolder() {}

    public static Expr fold(Expr root) {
        try {
            return root.accept(new Folder());
        } catch (Exception ex) {
            // Should never happen; on any internal failure just return original tree.
            return root;
        }
    }

    private static final class Folder implements ExprVisitor<Expr> {
        private static final int MAX_REPEAT = 1024; // guard against explosion

        @Override public Expr visitVar(VarExpr e) { return e; }
        @Override public Expr visitInt(IntLitExpr e) { return e; }
        @Override public Expr visitStr(StrLitExpr e) { return e; }
        @Override public Expr visitBool(BoolLitExpr e) { return e; }

        @Override public Expr visitUnary(UnaryExpr e) throws Exception {
            Expr inner = e.getExpr().accept(this);
            if (inner instanceof IntLitExpr && e.getOp() == '-') {
                // We do not currently create a distinct unary minus node in the parser;
                // if that ever appears, this shows how to fold it. (Placeholder)
                return inner; // leave untouched; unary minus not part of current grammar
            }
            if (inner instanceof BoolLitExpr && e.getOp() == '!') {
                return new BoolLitExpr(!((BoolLitExpr) inner).getValue());
            }
            if (inner instanceof StrLitExpr && e.getOp() == '~') {
                String s = ((StrLitExpr) inner).getValue();
                return new StrLitExpr(new StringBuilder(s).reverse().toString());
            }
            if (inner instanceof IntLitExpr && e.getOp() == '~') {
                // In the language, '~' on INT appears to be a no-op / unsupported for folding;
                // treat as non-foldable (could be bitwise NOT if later required).
                return new UnaryExpr(e.getOp(), inner, e.getType());
            }
            return new UnaryExpr(e.getOp(), inner, e.getType());
        }

        @Override public Expr visitBinary(BinaryExpr e) throws Exception {
            Expr l = e.getLeft().accept(this);
            Expr r = e.getRight().accept(this);

            if (l instanceof IntLitExpr && r instanceof IntLitExpr) {
                int a = ((IntLitExpr) l).getValue();
                int b = ((IntLitExpr) r).getValue();
                switch (e.getOp()) {
                    case '+': return new IntLitExpr(a + b);
                    case '-': return new IntLitExpr(a - b);
                    case '*': return new IntLitExpr(a * b);
                    case '/': if (b != 0) return new IntLitExpr(a / b); else break;
                    case '%': if (b != 0) return new IntLitExpr(a % b); else break;
                    case '<': return new BoolLitExpr(a < b);
                    case '>': return new BoolLitExpr(a > b);
                    case '=': return new BoolLitExpr(a == b);
                }
            }

            if (l instanceof BoolLitExpr && r instanceof BoolLitExpr) {
                boolean a = ((BoolLitExpr) l).getValue();
                boolean b = ((BoolLitExpr) r).getValue();
                switch (e.getOp()) {
                    case '&': return new BoolLitExpr(a && b);
                    case '|': return new BoolLitExpr(a || b);
                    case '=': return new BoolLitExpr(a == b);
                }
            }

            if (e.getOp() == '|' && l instanceof BoolLitExpr) {
                // short-circuit: true | x => true ; false | x => x
                boolean a = ((BoolLitExpr) l).getValue();
                if (a) return new BoolLitExpr(true);
            }
            if (e.getOp() == '&' && l instanceof BoolLitExpr) {
                // short-circuit: false & x => false ; true & x => x
                boolean a = ((BoolLitExpr) l).getValue();
                if (!a) return new BoolLitExpr(false);
            }

            if (l instanceof StrLitExpr && r instanceof StrLitExpr) {
                String a = ((StrLitExpr) l).getValue();
                String b = ((StrLitExpr) r).getValue();
                switch (e.getOp()) {
                    case '+': return new StrLitExpr(a + b);
                    case '<': return new BoolLitExpr(a.compareTo(b) < 0);
                    case '>': return new BoolLitExpr(a.compareTo(b) > 0);
                    case '=': return new BoolLitExpr(a.equals(b));
                }
            }

            // string * int or int * string (repeat)
            if (e.getOp() == '*') {
                if (l instanceof StrLitExpr && r instanceof IntLitExpr) {
                    String s = ((StrLitExpr) l).getValue();
                    int times = ((IntLitExpr) r).getValue();
                    if (times >= 0 && times <= MAX_REPEAT) {
                        return new StrLitExpr(repeat(s, times));
                    }
                } else if (l instanceof IntLitExpr && r instanceof StrLitExpr) {
                    int times = ((IntLitExpr) l).getValue();
                    String s = ((StrLitExpr) r).getValue();
                    if (times >= 0 && times <= MAX_REPEAT) {
                        return new StrLitExpr(repeat(s, times));
                    }
                }
            }

            return new BinaryExpr(l, e.getOp(), r, e.getType());
        }

        @Override public Expr visitTernary(TernaryExpr e) throws Exception {
            Expr c = e.getCondition().accept(this);
            Expr t = e.getThenExpr().accept(this);
            Expr f = e.getElseExpr().accept(this);
            if (c instanceof BoolLitExpr) {
                return ((BoolLitExpr) c).getValue() ? t : f;
            }
            return new TernaryExpr(c, t, f, e.getType());
        }

        private static String repeat(String s, int n) {
            if (n == 0) return "";
            StringBuilder sb = new StringBuilder(s.length() * n);
            for (int i = 0; i < n; i++) sb.append(s);
            return sb.toString();
        }
    }
}
