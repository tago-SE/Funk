package ObjectPainterApp.model.shapes;

import ObjectPainterApp.model.shapes.factory.ShapeType;
import javafx.scene.canvas.GraphicsContext;

public class Oval extends Shape {

    public Oval() {
        super();  // required
    }

    @Override
    public ShapeType getType() {
        return ShapeType.OVAL;
    }

    @Override
    protected void drawShape(GraphicsContext gc) {
        double w = Math.abs(super.getStartX() - super.getEndX());
        double h = Math.abs(super.getStartY() - super.getEndY());
        if (super.isFilled())
            gc.fillOval(super.getCenterX() - w/2, super.getCenterY() - h/2, w, h);
        else
            gc.strokeOval(super.getCenterX() - w/2, super.getCenterY() - h/2, w, h);
    }

}
