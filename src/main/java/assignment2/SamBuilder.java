package assignment2;

public final class SamBuilder {
    private final StringBuilder sb = new StringBuilder();

    public SamBuilder append(String s) {
        if (s != null) sb.append(s);
        return this;
    }

    public SamBuilder line(String s) {
        if (s != null) sb.append(s);
        sb.append('\n');
        return this;
    }

    public SamBuilder label(String name) {
        if (name != null) sb.append(name).append(":\n");
        return this;
    }

    @Override
    public String toString() {
        return sb.toString();
    }
}
