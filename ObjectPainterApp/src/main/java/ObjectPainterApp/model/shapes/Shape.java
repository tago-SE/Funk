package ObjectPainterApp.model.shapes;


import javafx.scene.canvas.GraphicsContext;
import javafx.scene.paint.Color;

import java.io.Serializable;

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
public abstract class Shape implements Cloneable, IMemento, Serializable {

    private String id;
    private String color;
    boolean filled;
    private int lineWidth;
    double startX, startY, endX, endY;
    private int lineDashes;

    private static class ShapeMemento implements IMemento {
        String color;
        boolean filled;
        int lineWidth;
        double startX, startY, endX, endY;
        int lineDashes;
    }

    public Shape() {

    }

    public Shape(ShapeBuilder builder) {
        rebuild(builder);
    }

    public IMemento getMemento() {
        ShapeMemento state = new ShapeMemento();
        state.color = this.getColor();
        state.filled = this.isFilled();
        state.lineWidth = this.getLineWidth();
        state.startX = this.getStartX();
        state.startY = this.getStartY();
        state.endX = this.getEndX();
        state.endY = this.getEndY();
        state.lineDashes = this.getLineDashes();
        return state;
    }

    public void setMemento(IMemento memento) {
        ShapeMemento state = (ShapeMemento) memento;
        setColor(state.color);
        setLineWidth(state.lineWidth);
        setFilled(state.filled);
        setStartX(state.startX);
        setStartY(state.startY);
        setEndX(state.endX);
        setEndY(state.endY);
        setLineDashes(state.lineDashes);
    }

    public Shape rebuild(ShapeBuilder builder) {
        this.color = builder.getColor();
        this.filled = builder.isFillShape();
        this.lineDashes = builder.getLineDashes();
        this.lineWidth = builder.getLineWidth();
        this.startX = builder.getStartX();
        this.startY = builder.getStartY();
        this.endX = builder.getEndX();
        this.endY = builder.getEndY();
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

    public double getLeftX() {
        return Math.min(startX, endX);
    }

    public double getRightX() {
        return Math.max(startX, endX);
    }

    public double getTopY() {
        return Math.min(startY, endY);
    }

    public double getBotY() {
        return Math.max(startY, endY);
    }

    public int getLineDashes() {
        return lineDashes;
    }

    public void setLineDashes(int lineDashes) {
        this.lineDashes = lineDashes;
    }

    public double getCenterX() {
        // Don't use any of the above public methods internally as they may be changed in sub-types
        return Math.abs(startX - endX)/2 + Math.min(startX, endX);
    }

    public double getCenterY() {
        // Don't use any of the above public methods internally as they may be changed in sub-types
        return Math.abs(startY - endY)/2 +  Math.min(startY, endY);
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

// ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
//  This section contains rendering for JavaFX, should probably be refactored if other rendering frameworks should be
//  supported...
// ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

    public void draw(GraphicsContext gc) {
        gc.setFill(Color.web(color));
        gc.setStroke(Color.web(color));
        gc.setLineWidth(lineWidth);
        gc.setLineDashes(lineDashes);
        drawShape(gc);
    }

    // Template method for rendering the shape
    abstract protected void drawShape(GraphicsContext gc);

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
                '}';
    }

}
