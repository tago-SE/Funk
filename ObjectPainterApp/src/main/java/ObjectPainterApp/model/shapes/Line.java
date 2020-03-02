package ObjectPainterApp.model.shapes;

import ObjectPainterApp.model.shapes.factory.ShapeType;
import javafx.scene.canvas.GraphicsContext;

public class Line extends Shape {

    @Override
    public ShapeType getType() {
        return ShapeType.LINE;
    }

    @Override
    protected void drawShape(GraphicsContext gc) {
        gc.strokeLine(getStartX(), getStartY(), getEndX(), getEndY());
    }

}
