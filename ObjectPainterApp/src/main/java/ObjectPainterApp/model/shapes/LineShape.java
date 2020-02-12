package ObjectPainterApp.model.shapes;

import javafx.scene.canvas.GraphicsContext;

public class LineShape extends Shape {

    public LineShape() {
        super();  // required
    }

    @Override
    protected void drawShape(GraphicsContext gc) {
        gc.strokeLine(super.startX, super.startY, super.endX, super.endY);
    }

}
