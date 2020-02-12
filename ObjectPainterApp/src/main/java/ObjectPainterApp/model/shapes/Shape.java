package ObjectPainterApp.model.shapes;


import javafx.scene.canvas.GraphicsContext;
import javafx.scene.paint.Color;

import java.util.ArrayList;
import java.util.List;

/**
 * An abstract class for representing drawable shapes. The creation patterns used are Build-Pattern and Prototype-Pattern.
 *
 * The choice of using Builder was that Shapes are fairly similar in the attributes they share, but that the number of
 * arguments in the constructor can grow in size. By using the Build-pattern constructor pollution is avoided, and a
 * interface for how to add new properties is introduced.
 *
 * The choice behind using the Prototype pattern was to be able to dynamically create new Shapes without knowing which
 * shapes exists in a predefined list. Shape prototypes are saved at run-time inside the Shape-Cache and used to create
 * clones, which can then be rebuilt with new properties. This could then be used to validate the shape-type argument
 * before creation. Another benefit with using the prototype-pattern is that if one wants to create a copy of the
 * original, but only change one or two attributes, it can be done using clone(), rather than having to use the factory
 * method.
 *
 * Note that I've decided to break MVC here adding JavaFX implementation into the model. Adding a View class equivalent
 * was seen as a bit too time consuming for the purposes of this programming lab.
 *
 *
 * Ref:
 * 1. https://github.com/iluwatar/java-design-patterns/tree/master/builder
 * 2. https://github.com/iluwatar/java-design-patterns/tree/master/prototype
 * 3. https://www.tutorialspoint.com/design_pattern/prototype_pattern.htm
 */
public abstract class Shape implements Cloneable {

    protected boolean selected;
    protected String id;
    protected String color;
    protected boolean filled;
    protected int lineWidth;
    protected double startX, startY, endX, endY;

    public Shape() {

    }

    public Shape(ShapeBuilder builder) {
        rebuild(builder);
    }

    public Shape rebuild(ShapeBuilder builder) {
        this.color = builder.getColor();
        this.filled = builder.isFillShape();
        this.lineWidth = builder.getLineWidth();
        this.startX = builder.getStartX();
        this.startY = builder.getStartY();
        this.endX = builder.getEndX();
        this.endY = builder.getEndY();
        this.selected = builder.isSelected();
        return this;
    }

    // Prevent override
    final public String getName() {
        return this.getClass().getSimpleName().replace(Shape.class.getSimpleName(), "");
    }

    // Prevent override
    final public Shape setId(String id) {
        this.id = id;
        return this;
    }

    public boolean intersects(Shape s) {
        return !(this.startX > s.endX || this.endX < s.startX || this.startY > s.endY || this.endY < s.startY);
    }

    final public String getId() {
        return this.id;
    }

    public String getColor() {
        return color;
    }

    public void setColor(String color) {
        this.color = color;
    }

    public boolean isFilled() {
        return filled;
    }

    public void setFilled(boolean fill) {
        this.filled = fill;
    }

    public int getLineWidth() {
        return lineWidth;
    }

    public void setLineWidth(int lineWidth) {
        this.lineWidth = lineWidth;

    }

    public double getStartX() {
        return startX;
    }

    public void setStartX(double startX) {
        this.startX = startX;
    }

    public double getStartY() {
        return startY;
    }

    public void setStartY(double startY) {
        this.startY = startY;
    }

    public double getEndX() {
        return endX;
    }

    public void setEndX(double endX) {
        this.endX = endX;
    }

    public double getEndY() {
        return endY;
    }

    public void setEndY(double endY) {
        this.endY = endY;
    }

    public boolean isSelected() {
        return selected;
    }

    public void setSelected(boolean selected) {
        this.selected = selected;
    }

    @Override
    public Shape clone() {
        try {
            return (Shape) super.clone();
        } catch (CloneNotSupportedException e) {
            e.printStackTrace();
            return null;
        }
    }

    // Prevent override
    final public void draw(GraphicsContext gc) {
        gc.setFill(Color.web(color));
        gc.setStroke(Color.web(color));
        gc.setLineWidth(lineWidth);
        if (selected)
            gc.setLineDashes(5);
        else
            gc.setLineDashes(0);
        drawShape(gc);
    }

    // Template method for rendering the shape
    abstract protected  void drawShape(GraphicsContext gc);


    @Override
    public String toString() {
        return getClass().getSimpleName() + '\'' + "{" +
                "id=\'" + id + "\'" +
                ", name=\'" + getName() + "\'" +
                ", color='" + color + '\'' +
                ", filled=" + filled +
                ", lineWidth=" + lineWidth +
                ", startX=" + startX +
                ", startY=" + startY +
                ", endX=" + endX +
                ", endY=" + endY +
                ", selected=" + selected +
                '}';
    }
}
