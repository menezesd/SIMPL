package assignment2;

import java.util.UUID;
import java.util.Objects;

public class Label {
    private final String name;
    private final LabelType type;

    public Label(String name, LabelType type) {
        this.name = name;
        this.type = type;
    }

    public Label() {
        this(generateUUID(), LabelType.DEFAULT);
    }

    public Label(String name) {
        this(name, LabelType.DEFAULT);
    }

    public Label(LabelType type) {
        this(generateUUID(), type);
    }

    public String getName() {
        return name;
    }

    public LabelType getType() {
        return type;
    }

    private static String generateUUID() {
        String uuid = UUID.randomUUID().toString().substring(0, 8);
        if (!Character.isLetter(uuid.charAt(0))) {
            char randomLetter = (char) ('a' + Math.random() * ('z' - 'a' + 1));
            uuid = randomLetter + uuid.substring(1);
        }
        return uuid;
    }

    @Override
    public boolean equals(Object o) {
        if (this == o) return true;
        if (!(o instanceof Label)) return false;
        Label label = (Label) o;
    return Objects.equals(name, label.name) && type == label.getType();
    }

    @Override
    public int hashCode() {
    return Objects.hash(name, type);
    }
}
