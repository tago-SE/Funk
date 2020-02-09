package ObjectPainterApp.view.shapes;

import javafx.scene.canvas.GraphicsContext;

public class OvalShapeDrawer extends ShapeDrawer {

    public OvalShapeDrawer() {
        super(); // required
    }

    @Override
    protected void drawShape(GraphicsContext gc) {
        double w = Math.abs(super.startX - super.endX);
        double h = Math.abs(super.startY - super.endY);
        if (super.filled)
            gc.fillOval(super.startX, super.startY, w, h);
        else
            gc.strokeOval(super.startX, super.startY, w, h);
    }

}
