package ObjectPainterApp.view.shapes;

import javafx.scene.canvas.GraphicsContext;

public class LineShapeDrawer extends ShapeDrawer {

    public LineShapeDrawer() {
        super(); // required
    }

    @Override
    protected void drawShape(GraphicsContext gc) {
        gc.strokeLine(super.startX, super.startY, super.endX, super.endY);
    }



}
