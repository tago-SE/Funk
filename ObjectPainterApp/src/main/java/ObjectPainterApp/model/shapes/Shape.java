package ObjectPainterApp.model.shapes;


import ObjectPainterApp.model.shapes.factory.ShapeType;
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
public abstract class Shape implements Cloneable, IShapeMemento, Serializable {

    protected ShapeState state = new ShapeState();

    public Shape() {

    }

    /**
     * Returns the type of the shape
     * @return
     */
    abstract public ShapeType getType();

    public IShapeMemento getMemento() {
        return state.clone();
    }

    public void setMemento(IShapeMemento memento) {
        ShapeState s = (ShapeState) memento;
        state = new ShapeState()
                .setColor(s.getColor())
                .setFilled(s.isFilled())
                .setLineDashes(s.getLineDashes())
                .setLineWidth(s.getLineWidth())
                .setStartX(s.getStartX())
                .setStartY(s.getStartY())
                .setEndX(s.getEndX())
                .setEndY(s.getEndY());
    }

    // Prevent override
    final public String getName() {
        return this.getClass().getSimpleName().replace(Shape.class.getSimpleName(), "");
    }

    public boolean intersects(Shape s) {
        return !(this.getLeftX() > s.getRightX() ||
                this.getRightX() < s.getLeftX() ||
                this.getTopY() > s.getBotY() ||
                this.getBotY() < s.getTopY());
    }

    public String getColor() {
        return state.color;
    }

    public void setColor(String color) {
        state.color = color;
    }

    public boolean isFilled() {
        return state.filled;
    }

    public void setFilled(boolean fill) {
        state.filled = fill;
    }

    public int getLineWidth() {
        return state.lineWidth;
    }

    public void setLineWidth(int lineWidth) {
        state.lineWidth = lineWidth;
    }

    public double getStartX() {
        return state.startX;
    }

    public void setStartX(double startX) {
        state.startX = startX;
    }

    public double getStartY() {
        return state.startY;
    }

    public void setStartY(double startY) {
        state.startY = startY;
    }

    public double getEndX() {
        return state.endX;
    }

    public void setEndX(double endX) {
        state.endX = endX;
    }

    public double getEndY() {
        return state.endY;
    }

    public void setEndY(double endY) {
        state.endY = endY;
    }

    public int getLineDashes() {
        return state.lineDashes;
    }

    public void setLineDashes(int lineDashes) {
        state.lineDashes = lineDashes;
    }


    public double getLeftX() {
        return Math.min(state.startX, state.endX);
    }

    public double getRightX() {
        return Math.max(state.startX, state.endX);
    }

    public double getTopY() {
        return Math.min(state.startY, state.endY);
    }

    public double getBotY() {
        return Math.max(state.startY, state.endY);
    }

    public double getCenterX() {
        // Don't use any of the above public methods internally as they may be changed in sub-types
        return Math.abs(state.startX - state.endX)/2 + Math.min(state.startX, state.endX);
    }

    public double getCenterY() {
        // Don't use any of the above public methods internally as they may be changed in sub-types
        return Math.abs(state.startY - state.endY)/2 +  Math.min(state.startY, state.endY);
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
        String color = state.getColor();
        gc.setFill(Color.web(color));
        gc.setStroke(Color.web(color));
        gc.setLineWidth(state.getLineWidth());
        gc.setLineDashes(state.getLineDashes());
        drawShape(gc);
    }

    // Template method for rendering the shape
    abstract protected void drawShape(GraphicsContext gc);

    @Override
    public String toString() {
        return "Shape{" +
                "state=" + state +
                '}';
    }
}
