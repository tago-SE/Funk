package ObjectPainterApp.model.shapes.factory;

import ObjectPainterApp.model.shapes.Rectangle;
import ObjectPainterApp.model.shapes.Shape;

/**
 * Factory class for creating drag selection markers of a given size
 */
public class DragSelectionFactory implements IDragSelectionFactory {

    private static DragSelectionFactory instance;

    private final Shape prototype;

    private DragSelectionFactory() {
        // Box configuration
        prototype = new Rectangle();
        prototype.setColor("0x000000ff");
        prototype.setLineWidth(1);
        prototype.setLineDashes(5);
        prototype.setFilled(false);
    }

    public static DragSelectionFactory getInstance() {
        if (instance == null) {
            return instance = new DragSelectionFactory();
        }
        return instance;
    }

    @Override
    public Shape getDragSelectionBox(Shape selectedShape) {
        double x1 = selectedShape.getLeftX() - 10;
        double y1 = selectedShape.getTopY() - 10;
        double x2 = selectedShape.getRightX() + 10;
        double y2 = selectedShape.getBotY() + 10;
        return getDragSelectionBox(x1, y1, x2, y2);
    }

    @Override
    public Shape getDragSelectionBox(double x1, double y1, double x2, double y2) {
        Shape box = prototype.clone();
        box.setStartX(x1);
        box.setStartY(y1);
        box.setEndX(x2);
        box.setEndY(y2);
        return box;
    }

}
