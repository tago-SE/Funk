package ObjectPainterApp.model.shapes;

import javafx.scene.canvas.GraphicsContext;

import java.util.ArrayList;
import java.util.List;

public class ShapeComposite extends Shape implements IShapeComposite {

    private List<Shape> children = new ArrayList<>();

    @Override
    public void addChildren(Shape shape) {
        children.add(shape);
    }

    @Override
    public void removeChildren(Shape shape) {
        children.remove(shape);
    }

    @Override
    public void setFilled(boolean fill) {
        children.forEach(shape -> shape.setFilled(fill));
    }

    @Override
    public void setColor(String color) {
        children.forEach(shape -> shape.setColor(color));
    }

    @Override
    public void setLineWidth(int lineWidth) {
        children.forEach(shape -> shape.setLineWidth(lineWidth));
    }

    @Override
    protected void drawShape(GraphicsContext gc) {
        children.forEach(child -> child.drawShape(gc));
    }

}
