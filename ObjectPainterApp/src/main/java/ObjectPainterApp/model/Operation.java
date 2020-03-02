package ObjectPainterApp.model;

import java.util.ArrayList;
import java.util.List;

public enum Operation {

    UNDO("undo"),
    REDO("redo"),
    DELETE ("delete"),
    SELECTION("selection");

    public final String label;

    Operation(String label) {
        this.label = label;
    }

    public static List<String> labels() {
        List<String> resultList = new ArrayList<>();
        for (Operation o : values()) {
            resultList.add(o.label);
        }
        return resultList;
    }

    public static Operation labelOf(String label) {
        return Operation.valueOf(label.toUpperCase());
    }

}
