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
}
