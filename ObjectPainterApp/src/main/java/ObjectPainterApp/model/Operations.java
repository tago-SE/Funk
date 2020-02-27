package ObjectPainterApp.model;

import java.util.ArrayList;
import java.util.List;

public enum Operations {

    UNDO("undo"), REDO("redo"), DELETE ("delete"), SELECTION("selection");

    public final String label;

    Operations(String label) {
        this.label = label;
    }

    public static List<String> labels() {
        List<String> resultList = new ArrayList<>();
        for (Operations o : values()) {
            resultList.add(o.label);
        }
        return resultList;
    }

    public static Operations labelOf(String label) {
        return Operations.valueOf(label.toUpperCase());
    }

}
