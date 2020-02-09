package ObjectPainterApp.view.shapes;
import ObjectPainterApp.model.shapes.Shape;
import javafx.scene.canvas.GraphicsContext;
import javafx.scene.paint.Color;

/**
 * Template method
 */
public abstract class ShapeDrawer implements Cloneable {

    protected Color color;
    protected int lineWidth;
    protected boolean filled;
    protected double startX, startY, endX, endY;

    public ShapeDrawer() { }

    public ShapeDrawer rebuild(Shape shape) {
        if (shape.getColor() == null || shape.getColor().equals(""))
            color = Color.BLACK;
        else
            color = Color.web(shape.getColor());
        lineWidth = shape.getLineWidth();
        startX = shape.getStartX();
        startY = shape.getStartY();
        endX = shape.getEndX();
        endY = shape.getEndY();
        filled = shape.isFilled();
        return this;
    }

    // Prevent override
    final public String getName() {
        return this.getClass().getSimpleName().replace(ShapeDrawer.class.getSimpleName(), "");
    }

    // Prevent override
    final public void draw(GraphicsContext gc) {
        gc.setFill(color);
        gc.setStroke(color);
        gc.setLineWidth(lineWidth);
        drawShape(gc);
    }

    // Template method for rendering the shape
    abstract protected  void drawShape(GraphicsContext gc);

    @Override
    public ShapeDrawer clone() {
        try {
            return (ShapeDrawer) super.clone();
        } catch (CloneNotSupportedException e) {
            e.printStackTrace();
            return null;
        }
    }

    @Override
    public String toString() {
        return getClass().getSimpleName() + '\'' + "{" +
                "name=\'" + getName() + "\'" +
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
