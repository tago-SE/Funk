package ObjectPainterApp.model;

public interface IOperation {

    OperationType getName();
    boolean hasData();
    Object getData();

}
