package ObjectPainterApp.model.shapes;

import ObjectPainterApp.model.shapes.factory.ShapeType;
import javafx.scene.canvas.GraphicsContext;

public class Rectangle extends Shape {

    public Rectangle() {
        super();
    }

    @Override
    public ShapeType getType() {
        return ShapeType.RECTANGLE;
    }

    @Override
    protected void drawShape(GraphicsContext gc) {
        double w = Math.abs(super.getStartX() - super.getEndX());
        double h = Math.abs(super.getStartY() - super.getEndY());
        if (super.isFilled())
            gc.fillRect(super.getCenterX() - w/2, super.getCenterY() - h/2, w, h);
        else
            gc.strokeRect(super.getCenterX() - w/2, super.getCenterY() - h/2, w, h);
    }

}
