package assignment2;

import java.util.Optional;

public enum Type {
    INT("int"),
    BOOL("bool"),
    STRING("String");

    private final String typeName;

    Type(String typeName) {
        this.typeName = typeName;
    }

    @Override
    public String toString() {
        return typeName;
    }

    public static Optional<Type> fromString(String typeString) {
        if (typeString == null) {
            return Optional.empty();
        }
        for (Type type : Type.values()) {
            if (type.typeName.equals(typeString)) {
                return Optional.of(type);
            }
        }
        return Optional.empty();
    }

    /**
     * Parse a type string and throw a TypeErrorException with the provided
     * line number if the type is invalid. This moves the responsibility for
     * throwing to the parsing helper so callers don't need to perform a null
     * check.
     */
    public static Type parse(String typeString, int line) throws TypeErrorException {
        return fromString(typeString).orElseThrow(() -> new TypeErrorException("Invalid type: " + typeString, line));
    }

    public boolean isCompatibleWith(Type other) {
        return this == other;
    }
}
