package ObjectPainterApp.model;

import java.util.ArrayList;
import java.util.List;

@Deprecated
public enum OperationLabel {

    UNDO("undo"),
    REDO("redo"),
    DELETE ("delete"),
    SELECTION("selection");

    public final String label;

    OperationLabel(String label) {
        this.label = label;
    }

    public static List<String> labels() {
        List<String> resultList = new ArrayList<>();
        for (OperationLabel o : values()) {
            resultList.add(o.label);
        }
        return resultList;
    }

}
