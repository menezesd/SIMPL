package assignment2;

import assignment2.BinopType;
import assignment2.CompilerException;
import assignment2.OperatorUtils;
import assignment2.SyntaxErrorException;

public final class StringRuntime {
    private StringRuntime() {}

    public static String repeatString(Type firstInputType, Type secondInputType) {
        Label enterFuncLabel = new Label();
        Label exitFuncLabel = new Label();
        Label startLoopLabel = new Label();
        Label stopLoopLabel = new Label();
        Label returnLabel = new Label();
        Label invalidParamLabel = new Label();

        String sam = "";

        // prepare params, String always on top
        if (firstInputType == Type.STRING) {
            sam += "SWAP\n";
        }

        // call method
        sam += "LINK\n";
        sam += "JSR " + enterFuncLabel.getName() + "\n";
        sam += "UNLINK\n";
        sam += "ADDSP -1\n"; // free second param, only first param remain with new value
        sam += "JUMP " + exitFuncLabel.getName() + "\n";

        // method definition
        sam += enterFuncLabel.getName() + ":\n";
        sam += "PUSHIMM 0\n"; // local 1: loop counter
        sam += "PUSHIMM 0\n"; // local 2: increment address
        sam += "PUSHIMM 0\n"; // local 3: return address

        // validate param, if n < 0 -> return
        sam += "PUSHOFF -2\n";
        sam += "ISNEG\n";
        sam += "JUMPC " + invalidParamLabel.getName() + "\n";

        // allocate memory for new string -> Address
        sam += "PUSHOFF -1\n";
        sam += getStringLength();
        sam += "PUSHOFF -2\n";
        sam += "TIMES\n";
        sam += "PUSHIMM 1\n";
        sam += "ADD\n";
        sam += "MALLOC\n";
        sam += "STOREOFF 3\n";

        // return this address
        sam += "PUSHOFF 3\n";
        sam += "STOREOFF 4\n";

        // loop...
        sam += startLoopLabel.getName() + ":\n";
        // check if done
        sam += "PUSHOFF 2\n";
        sam += "PUSHOFF -2\n";
        sam += "EQUAL\n";
        sam += "JUMPC " + stopLoopLabel.getName() + "\n";

        // append str to memory
        sam += "PUSHIMM 0\n"; // will return next address
        sam += "PUSHOFF 3\n"; // param1: starting memory address
        sam += "PUSHOFF -1\n"; // param2: string
        sam += appendStringHeap();
        sam += "STOREOFF 3\n";

        // increase counter
        sam += "PUSHOFF 2\n";
        sam += "PUSHIMM 1\n";
        sam += "ADD\n";
        sam += "STOREOFF 2\n";

        // Continue loop
        sam += "JUMP " + startLoopLabel.getName() + "\n";

        // Stop loop
        sam += stopLoopLabel.getName() + ":\n";
        sam += "PUSHOFF 4\n";
        sam += "STOREOFF -2\n";
        sam += "JUMP " + returnLabel.getName() + "\n";

        // Invalid param, return empty string
        sam += invalidParamLabel.getName() + ":\n";
        sam += "PUSHIMMSTR \"\"";
        sam += "STOREOFF -2\n";
        sam += "JUMP " + returnLabel.getName() + "\n";

        // Return func
        sam += returnLabel.getName() + ":\n";
        sam += "ADDSP -3\n";
        sam += "RST\n";

        // Exit method
        sam += exitFuncLabel.getName() + ":\n";

        return sam;
    }

    public static String getStringLength() {
        Label startCountLabel = new Label();
        Label stopCountLabel = new Label();
        String sam = "";

        sam += "DUP\n";

        // START
        sam += startCountLabel.getName() + ":\n";
        sam += "DUP\n";
        sam += "PUSHIND\n";

        // check end of string
        sam += "ISNIL\n";
        sam += "JUMPC " + stopCountLabel.getName() + "\n";

        // increament count and continue loop
        sam += "PUSHIMM 1\n";
        sam += "ADD\n";
        sam += "JUMP " + startCountLabel.getName() + "\n";

        // STOP
        sam += stopCountLabel.getName() + ":\n";
        sam += "SWAP\n";
        sam += "SUB\n";

        return sam;
    }

    public static String appendStringHeap() {
        Label enterFuncLabel = new Label();
        Label exitFuncLabel = new Label();
        Label startLoopLabel = new Label();
        Label stopLoopLabel = new Label();

        String sam = "";

        // call method
        sam += "LINK\n";
        sam += "JSR " + enterFuncLabel.getName() + "\n";
        sam += "UNLINK\n";
        sam += "ADDSP -2\n";
        sam += "JUMP " + exitFuncLabel.getName() + "\n";

        sam += enterFuncLabel.getName() + ":\n";
        sam += "PUSHOFF -2\n";
        sam += "PUSHOFF -1\n";

        sam += startLoopLabel.getName() + ":\n";
        // put char in TOS
        // end loop if nil
        sam += "PUSHOFF 3\n";
        sam += "PUSHIND\n";
        sam += "ISNIL\n";
        sam += "JUMPC " + stopLoopLabel.getName() + "\n";

        // Save to allocated memory
        sam += "PUSHOFF 2\n";
        sam += "PUSHOFF 3\n";
        sam += "PUSHIND\n";
        sam += "STOREIND\n";

        // increase address current string
        sam += "PUSHOFF 3\n";
        sam += "PUSHIMM 1\n";
        sam += "ADD\n";
        sam += "STOREOFF 3\n";

        // increase final address string
        sam += "PUSHOFF 2\n";
        sam += "PUSHIMM 1\n";
        sam += "ADD\n";
        sam += "STOREOFF 2\n";

        sam += "JUMP " + startLoopLabel.getName() + "\n";

        sam += stopLoopLabel.getName() + ":\n";
        sam += "PUSHOFF 2\n";
        sam += "PUSHIMMCH '\\0'" + "\n";
        sam += "STOREIND\n";
        sam += "PUSHOFF 2\n";
        sam += "STOREOFF -3\n";
        sam += "ADDSP -2\n";
        sam += "RST\n";

        // Exit method
        sam += exitFuncLabel.getName() + ":\n";

        return sam;
    }

    public static String concatString() {
        Label enterFuncLabel = new Label();
        Label exitFuncLabel = new Label();

        String sam = "";

        // call method
        sam += "LINK\n";
        sam += "JSR " + enterFuncLabel.getName() + "\n";
        sam += "UNLINK\n";
        sam += "ADDSP -1\n"; // free second param, only first param remain with new value
        sam += "JUMP " + exitFuncLabel.getName() + "\n";

        // method definition
        sam += enterFuncLabel.getName() + ":\n";
        sam += "PUSHIMM 0\n"; // local 2: increment address
        sam += "PUSHIMM 0\n"; // local 3: return address

        // allocate space for resulting string
        sam += "PUSHOFF -1\n";
        sam += getStringLength();
        sam += "PUSHOFF -2\n";
        sam += getStringLength();
        sam += "ADD\n";
        sam += "PUSHIMM 1\n";
        sam += "ADD\n";
        sam += "MALLOC\n";
        sam += "STOREOFF 2\n";

        // return this address
        sam += "PUSHOFF 2\n";
        sam += "STOREOFF 3\n";

        // append first string to memory
        sam += "PUSHIMM 0\n"; // will return next address
        sam += "PUSHOFF 2\n"; // param1: starting memory address
        sam += "PUSHOFF -2\n"; // param2: string
        sam += appendStringHeap();
        sam += "STOREOFF 2\n";

        // append second string to memory
        sam += "PUSHIMM 0\n";
        sam += "PUSHOFF 2\n";
        sam += "PUSHOFF -1\n";
        sam += appendStringHeap();
        sam += "STOREOFF 2\n";

        // store in the first string pos
        sam += "PUSHOFF 3\n";
        sam += "STOREOFF -2\n";

        // clean local vars
        sam += "ADDSP -2\n";
        // return
        sam += "RST\n";

        // Exit method
        sam += exitFuncLabel.getName() + ":\n";

        return sam;
    }

    public static String compareString(char op) throws CompilerException {
        if (OperatorUtils.getBinopType(op) != BinopType.COMPARISON) {
            throw new SyntaxErrorException(
                "compareString receive invalid operation: " + op,
                -1
            );
        }

        // expects parameters (2 strings) already on the stack
        Label enterFuncLabel = new Label();
        Label exitFuncLabel = new Label();
        Label startLoopLabel = new Label();
        Label stopLoopLabel = new Label();

        String sam = "";

        // call method
        sam += "LINK\n";
        sam += "JSR " + enterFuncLabel.getName() + "\n";
        sam += "UNLINK\n";
        sam += "ADDSP -1\n"; // free second param, only first param remain with new value
        sam += "JUMP " + exitFuncLabel.getName() + "\n";

        // method definition
        sam += enterFuncLabel.getName() + ":\n";
        sam += "PUSHIMM 0\n"; // local 2: counter
        sam += "PUSHIMM 0\n"; // local 3: result

        // loop...
        sam += startLoopLabel.getName() + ":\n";
        // reach end of string 1?
        sam += "PUSHOFF -2\n";
        sam += "PUSHOFF 2\n";
        sam += "ADD\n";
        sam += "PUSHIND\n";
        sam += "ISNIL\n";

        // reach end of string 2?
        sam += "PUSHOFF -1\n";
        sam += "PUSHOFF 2\n";
        sam += "ADD\n";
        sam += "PUSHIND\n";
        sam += "ISNIL\n";

        // reach end of both string, is equal
        sam += "AND\n";
        sam += "JUMPC " + stopLoopLabel.getName() + "\n";

        // not end, comparing char by char
        // get char of string 1
        sam += "PUSHOFF -2\n";
        sam += "PUSHOFF 2\n";
        sam += "ADD\n";
        sam += "PUSHIND\n";

        // get char of string 2
        sam += "PUSHOFF -1\n";
        sam += "PUSHOFF 2\n";
        sam += "ADD\n";
        sam += "PUSHIND\n";

        // compare and store result
        sam += "CMP\n";
        sam += "STOREOFF 3\n";

        // check if done
        sam += "PUSHOFF 3\n";
        sam += "JUMPC " + stopLoopLabel.getName() + "\n";

        // not done, continue to next char
        sam += "PUSHOFF 2\n";
        sam += "PUSHIMM 1\n";
        sam += "ADD\n";
        sam += "STOREOFF 2\n";
        sam += "JUMP " + startLoopLabel.getName() + "\n";

        // Stop loop
        sam += stopLoopLabel.getName() + ":\n";
        sam += "PUSHOFF 3\n";
        sam += "STOREOFF -2\n";
        sam += "ADDSP -2\n";
        sam += "RST\n";

        // Exit method
        sam += exitFuncLabel.getName() + ":\n";

        if (op == '<') {
            sam += "PUSHIMM 1\n";
        } else if (op == '>') {
            sam += "PUSHIMM -1\n";
        } else {
            sam += "PUSHIMM 0\n";
        }
        sam += "EQUAL\n";

        return sam;
    }
}
