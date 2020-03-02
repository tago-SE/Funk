package ObjectPainterApp.model.shapes;

import ObjectPainterApp.model.shapes.factory.ShapeType;
import javafx.scene.canvas.GraphicsContext;

import java.util.ArrayList;
import java.util.Collection;
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
    public Collection<Shape> getChildren() {
        return children;
    }

    @Override
    public void clear() {
        children.clear();
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
    public double getStartX() {
        return getLeftX();
    }

    @Override
    public double getEndX() {
       return getRightX();
    }

    @Override
    public double getStartY() {
        return getTopY();
    }

    @Override
    public double getEndY() {
        return getBotY();
    }

    @Override
    public double getLeftX() {
        double x = Integer.MAX_VALUE;
        for (Shape child : children) {
            x = Math.min(x, child.getLeftX());
            x = Math.min(x, child.getRightX());
        }
        return x;
    }

    @Override
    public double getRightX() {
        double x = Integer.MIN_VALUE;
        for (Shape child : children) {
            x = Math.max(x, child.getLeftX());
            x = Math.max(x, child.getRightX());
        }
        return x;
    }

    @Override
    public double getTopY() {
        double y = Integer.MAX_VALUE;
        for (Shape child : children) {
            y = Math.min(y, child.getTopY());
            y = Math.min(y, child.getBotY());
        }
        return y;
    }

    @Override
    public double getBotY() {
        double y = Integer.MIN_VALUE;
        for (Shape child : children) {
            y = Math.max(y, child.getTopY());
            y = Math.max(y, child.getBotY());
        }
        return y;
    }

    public void draw(GraphicsContext gc) {
        children.forEach(child -> child.draw(gc));
    }

    @Override
    protected void drawShape(GraphicsContext gc) {
        throw new IllegalStateException("Should never be called.");
    }

    @Override
    public ShapeType getType() {
        throw new IllegalArgumentException("Composite does not have a type yet");
    }

    @Override
    public IShapeMemento getMemento() {
        throw new IllegalArgumentException("Composite does not have a memento");
    }

}
