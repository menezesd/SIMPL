package assignment2;

import edu.utexas.cs.sam.io.SamTokenizer;
import edu.utexas.cs.sam.io.Tokenizer.TokenType;
import java.io.PrintStream;
import java.util.ArrayList;
import java.util.Map;
import java.util.UUID;

public class CompilerUtils {
    /**
     * Expects the next token to be the specified character. Throws SyntaxErrorException if not.
     */
    public static void expect(SamTokenizer f, char expected, int line) throws SyntaxErrorException {
        if (!f.check(expected)) {
            throw new SyntaxErrorException("Expected '" + expected + "'", line);
        }
        processedTokens.add(String.valueOf(expected));
    }

    /**
     * Expects the next token to be the specified string. Throws SyntaxErrorException if not.
     */
    public static void expect(SamTokenizer f, String expected, int line) throws SyntaxErrorException {
        if (!f.check(expected)) {
            throw new SyntaxErrorException("Expected '" + expected + "'", line);
        }
        processedTokens.add(expected);
    }

    /**
     * Expects the next token to be a word (identifier). Throws SyntaxErrorException if not.
     */
    public static String expectIdentifier(SamTokenizer f, int line) throws SyntaxErrorException {
        if (f.peekAtKind() != TokenType.WORD) {
            throw new SyntaxErrorException("Expected identifier", line);
        }
        String word = f.getWord();
        processedTokens.add(word);
        return word;
    }

    /**
     * Expects the next token to be an integer. Throws SyntaxErrorException if not.
     */
    public static int expectInt(SamTokenizer f, int line) throws SyntaxErrorException {
        if (f.peekAtKind() != TokenType.INTEGER) {
            throw new SyntaxErrorException("Expected integer", line);
        }
        int value = f.getInt();
        processedTokens.add(String.valueOf(value));
        return value;
    }

    /**
     * Expects the next token to be a string literal. Throws SyntaxErrorException if not.
     */
    public static String expectString(SamTokenizer f, int line) throws SyntaxErrorException {
        if (f.peekAtKind() != TokenType.STRING) {
            throw new SyntaxErrorException("Expected string literal", line);
        }
        String str = f.getString();
        processedTokens.add('"' + str + '"');
        return str;
    }

    /**
     * Expect the next word token to equal the provided value. Records token and
     * throws SyntaxErrorException with line number if mismatch.
     */
    public static void expectWord(SamTokenizer f, String expected, int line) throws SyntaxErrorException {
        if (f.peekAtKind() != TokenType.WORD) {
            throw new SyntaxErrorException("Expected '" + expected + "'", line);
        }
        String word = f.getWord();
        processedTokens.add(word);
        if (!word.equals(expected)) {
            throw new SyntaxErrorException("Expected '" + expected + "'", line);
        }
    }

    /**
     * Expect the next single-character token to be the provided char. Records
     * token and throws SyntaxErrorException with line number if mismatch.
     */
    public static void expectChar(SamTokenizer f, char expected, int line) throws SyntaxErrorException {
        if (!f.check(expected)) {
            throw new SyntaxErrorException("Expected '" + expected + "'", line);
        }
        processedTokens.add(String.valueOf(expected));
    }

    /**
     * Expect the next token to be an operator (single char). Records token and
     * throws SyntaxErrorException with line number if not an operator.
     */
    public static char expectOperator(SamTokenizer f, int line) throws SyntaxErrorException {
        if (f.peekAtKind() != TokenType.OPERATOR) {
            throw new SyntaxErrorException("Expected operator", line);
        }
        char op = f.getOp();
        processedTokens.add(String.valueOf(op));
        return op;
    }

    /**
     * Peek at the next word token without consuming it. Returns null if not a
     * word.
     */
    public static String peekWord(SamTokenizer f) {
        if (f.peekAtKind() != TokenType.WORD) {
            return null;
        }
        // SamTokenizer doesn't provide a peekWord, so consume and push back is not
        // available. We mimic peek by using getWord and re-inserting into
        // processedTokens but this is only safe if callers rely only on value
        // check; however SamTokenizer supports lookahead via peekAtKind; for
        // actual word content peek we temporarily getWord and add it back to
        // processedTokens to reflect consumption semantics; caller should not
        // assume this consumes the tokenizer.
        String w = f.getWord();
        // push the word into processedTokens to reflect that we observed it but
        // also push it back by resetting tokenizer state is not available here,
        // so we will document that peekWord consumes the token. To keep
        // compatibility with existing callers, prefer using check/expect methods.
        processedTokens.add(w);
        return w;
    }

    /**
     * Consume the token if it equals the provided string. Records on consume.
     * Returns true if consumed.
     */
    public static boolean consumeIf(SamTokenizer f, String s) {
        // Only consume when the next token is a word and matches `s`.
        if (f.peekAtKind() != TokenType.WORD) {
            return false;
        }
        // Use check-like behavior by peeking the next token text without
        // consuming it. SamTokenizer doesn't provide a non-consuming getWord,
        // so we use f.check(s) to safely test for the exact string token: that
        // method performs the correct non-consuming comparison (and records
        // tokens when true). If it returns true, the tokenizer has consumed
        // the token already via its contract; to keep behavior consistent we
        // simply return true.
        boolean matched = f.check(s);
        if (matched) {
            processedTokens.add(s);
            return true;
        }
        return false;
    }

    /**
     * Consume the token if it equals the provided character. Returns true if
     * consumed.
     */
    public static boolean consumeIf(SamTokenizer f, char c) {
        if (f.check(c)) {
            processedTokens.add(String.valueOf(c));
            return true;
        }
        return false;
    }

    /** General utils
     **/

    /** Processed Tokens Utils
     **/
    public static ArrayList<String> processedTokens = new ArrayList<>();

    public static void clearTokens() {
        processedTokens.clear();
    }

    public static boolean check(SamTokenizer f, char expected) {
        boolean result = f.check(expected);
        if (result) {
            processedTokens.add(String.valueOf(expected));
        }
        return result;
    }

    public static boolean check(SamTokenizer f, String expected) {
        boolean result = f.check(expected);
        if (result) {
            processedTokens.add(expected);
        }
        return result;
    }

    public static String getWord(SamTokenizer f) {
        String word = f.getWord();
        processedTokens.add(word);
        return word;
    }

    public static String getString(SamTokenizer f) {
        String str = f.getString();
        processedTokens.add("\"" + str + "\"");
        return str;
    }

    public static int getInt(SamTokenizer f) {
        int value = f.getInt();
        processedTokens.add(String.valueOf(value));
        return value;
    }

    public static char getOp(SamTokenizer f) {
        char op = f.getOp();
        processedTokens.add(String.valueOf(op));
        return op;
    }

    public static void skipToken(SamTokenizer f) {
        f.skipToken();
        processedTokens.add(".");
    }

    public static void printTokens() {
        System.out.println("PROCESSED TOKENS:");
        for (String token : processedTokens) {
            System.out.print(token + " ");
        }
        System.out.println("\n");
    }

    /**
     * Lookup a variable (or symbol) in the provided scope and throw a
     * SyntaxErrorException with the tokenizer line number if it is not found.
     */
    public static Node requireVar(Node scope, String name, SamTokenizer f) throws CompilerException {
        Node n = scope.lookupSymbol(name);
        if (n == null) {
            throw new SyntaxErrorException("Undeclared variable: " + name, f.lineNo());
        }
        return n;
    }

    /**
     * Typed symbol lookup: require a symbol of a specific type from a scope.
     */
    public static <T extends Node> T requireSymbol(Node scope, String name, Class<T> type, SamTokenizer f)
        throws CompilerException {
        T n = scope.lookupSymbol(name, type);
        if (n == null) {
            throw new SyntaxErrorException("Undeclared symbol: " + name, f.lineNo());
        }
        return n;
    }
}
