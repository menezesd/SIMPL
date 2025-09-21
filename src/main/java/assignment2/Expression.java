package assignment2;

import java.util.Objects;

public class Expression {
    private final String samCode;
    private final Type type;

    public Expression(String samCode, Type type) {
        this.samCode = samCode;
        this.type = type;
    }

    public Expression() {
        this("", Type.INT);
    }

    public String getSamCode() {
        return samCode;
    }

    public Type getType() {
        return type;
    }

    @Override
    public boolean equals(Object o) {
        if (this == o) return true;
        if (!(o instanceof Expression)) return false;
        Expression that = (Expression) o;
        return Objects.equals(samCode, that.samCode) && type == that.type;
    }

    @Override
    public int hashCode() {
        return Objects.hash(samCode, type);
    }
}
