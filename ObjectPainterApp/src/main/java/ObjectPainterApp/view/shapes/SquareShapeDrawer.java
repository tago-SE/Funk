package ObjectPainterApp.view.shapes;

import javafx.scene.canvas.GraphicsContext;

public class SquareShapeDrawer extends ShapeDrawer {

    public SquareShapeDrawer(){
        super(); // required
    }

    @Override
    protected void drawShape(GraphicsContext gc) {
        System.out.println(super.lineWidth);
        double w = Math.abs(super.startX - super.endX);
        double h = Math.abs(super.startY - super.endY);
        if (super.filled)
            gc.fillRect(super.startX, super.startY, w, h);
        else
            gc.strokeRect(super.startX, super.startY, w, h);
    }
}
