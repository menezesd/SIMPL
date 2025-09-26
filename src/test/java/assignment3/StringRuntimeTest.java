package assignment3;

import org.junit.jupiter.api.Test;

import static org.junit.jupiter.api.Assertions.*;

public class StringRuntimeTest {

    @Test
    void concat_two_strings() throws Throwable {
        SamBuilder sb = new SamBuilder();
        // Build: s1, s2, concat -> result at TOS -> store at [0] -> STOP
        sb.pushImmStr("\"foo\"");
        sb.pushImmStr("\"bar\"");
        sb.append(StringRuntime.concatString());
        sb.storeOff(0);
        sb.stop();
        sb.append(StringRuntime.emitAllStringFunctions());
        String program = sb.toString();
        SamTestRunner.checkReturnedStringValue(program, "foobar");
    }

    @Test
    void compare_strings_lt_gt_eq() throws Throwable {
        // '<' case: "a" < "b" => true (1)
        SamBuilder sb1 = new SamBuilder();
        sb1.pushImmStr("\"a\"");
        sb1.pushImmStr("\"b\"");
        sb1.append(StringRuntime.compareString('<'));
        sb1.storeOff(0);
        sb1.stop();
        sb1.append(StringRuntime.emitAllStringFunctions());
        SamTestRunner.checkReturnValue(sb1.toString(), 1);

        // '>' case: "b" > "a" => true (1)
        SamBuilder sb2 = new SamBuilder();
        sb2.pushImmStr("\"b\"");
        sb2.pushImmStr("\"a\"");
        sb2.append(StringRuntime.compareString('>'));
        sb2.storeOff(0);
        sb2.stop();
        sb2.append(StringRuntime.emitAllStringFunctions());
        SamTestRunner.checkReturnValue(sb2.toString(), 1);

        // '=' case: "x" == "x" => true (1)
        SamBuilder sb3 = new SamBuilder();
        sb3.pushImmStr("\"x\"");
        sb3.pushImmStr("\"x\"");
        sb3.append(StringRuntime.compareString('='));
        sb3.storeOff(0);
        sb3.stop();
        sb3.append(StringRuntime.emitAllStringFunctions());
        SamTestRunner.checkReturnValue(sb3.toString(), 1);

        // '=' case: "x" == "y" => false (0)
        SamBuilder sb4 = new SamBuilder();
        sb4.pushImmStr("\"x\"");
        sb4.pushImmStr("\"y\"");
        sb4.append(StringRuntime.compareString('='));
        sb4.storeOff(0);
        sb4.stop();
        sb4.append(StringRuntime.emitAllStringFunctions());
        SamTestRunner.checkReturnValue(sb4.toString(), 0);
    }

    @Test
    void repeat_string_both_orders() throws Throwable {
        // "ab" * 3 => "ababab"
        SamBuilder sb1 = new SamBuilder();
        sb1.pushImmStr("\"ab\"");
        sb1.pushImmInt(3);
    sb1.append(StringRuntime.repeatStringSI());
        sb1.storeOff(0);
        sb1.stop();
        sb1.append(StringRuntime.emitAllStringFunctions());
        SamTestRunner.checkReturnedStringValue(sb1.toString(), "ababab");

        // 2 * "xy" => "xyxy"
        SamBuilder sb2 = new SamBuilder();
        sb2.pushImmInt(2);
        sb2.pushImmStr("\"xy\"");
    sb2.append(StringRuntime.repeatStringIS());
        sb2.storeOff(0);
        sb2.stop();
        sb2.append(StringRuntime.emitAllStringFunctions());
        SamTestRunner.checkReturnedStringValue(sb2.toString(), "xyxy");
    }
}
