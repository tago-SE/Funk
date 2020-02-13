package ObjectPainterApp.model.shapes;

import javafx.scene.canvas.GraphicsContext;

import java.util.ArrayList;
import java.util.Collection;
import java.util.Iterator;
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
        double x = Integer.MAX_VALUE;
        for (Shape child : children) {
            startX = child.startX;
            if (startX < x)
                x = startX;
            endX = child.endX;
            if (endX < x)
                x = endX;
        }
        return x;
    }

    @Override
    public double getStartY() {
        double y = Integer.MAX_VALUE;
        for (Shape child : children) {
            startY = child.startY;
            if (startY < y)
                y = startY;
            endY = child.endY;
            if (endY < y)
                y = endY;
        }
        return y;
    }

    @Override
    public double getEndX() {
        double x = Integer.MIN_VALUE;
        for (Shape child : children) {
            endX = child.endX;
            if (endX > x)
                x = endX;
            startX = child.startX;
            if (startX > x)
                x = startX;
        }
        return x;
    }

    @Override
    public double getEndY() {
        double y = Integer.MIN_VALUE;
        for (Shape child : children) {
            endY = child.endY;
            if (endY > y)
                y = endY;
            startY = child.startY;
            if (startY > y)
                y = startY;
        }
        return y;
    }

    public void draw(GraphicsContext gc) {
        children.forEach(child -> child.draw(gc));
    }

    @Override
    protected void drawShape(GraphicsContext gc) {
        throw new IllegalStateException("This should never be called.");
    }

    @Override
    public IShapeMemento getMemento() {
        return null;  // Not implemented
    }

    @Override
    public void setMemento(IShapeMemento memento) {
        // Not implemented
    }

    @Override
    public boolean equals(Object o) {
        return false;
    }

}
