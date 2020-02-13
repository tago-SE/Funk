package ObjectPainterApp.model.shapes;

import javafx.scene.canvas.GraphicsContext;

public class SquareShape extends Shape {

    public static final String NAME = SquareShape.class.getSimpleName().replace(Shape.class.getSimpleName(), "");

    public SquareShape() {
        super(); // required
    }

    @Override
    protected void drawShape(GraphicsContext gc) {
        double w = Math.abs(super.startX - super.endX);
        double h = Math.abs(super.startY - super.endY);
        if (super.filled)
            gc.fillRect(super.startX, super.startY, w, h);
        else
            gc.strokeRect(super.startX, super.startY, w, h);
    }

}