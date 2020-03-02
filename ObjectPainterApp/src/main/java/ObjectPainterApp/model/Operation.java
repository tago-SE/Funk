package ObjectPainterApp.model;

public class Operation implements IOperation {

    private final Object data;
    private final OperationType name;

    public Operation(OperationType type) {
        this.name = type;
        this.data = null;
    }

    public Operation(OperationType type, Object data) {
        this.name = type;
        this.data = data;
    }

    @Override
    public OperationType getName() {
        return name;
    }

    @Override
    public boolean hasData() {
        return data != null;
    }

    @Override
    public Object getData() {
        return data;
    }

}
