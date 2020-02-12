package ObjectPainterApp.model.shapes;

public interface IShapeComponent {

    // Operations
    void setColor(String color);
    void setFilled(boolean fill);
    void setLineWidth(int lineWidth);
    void setSelected(boolean selected);

}
