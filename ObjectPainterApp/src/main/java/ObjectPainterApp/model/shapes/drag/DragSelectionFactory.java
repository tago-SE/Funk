package ObjectPainterApp.model.shapes.drag;

import ObjectPainterApp.model.shapes.Shape;
import ObjectPainterApp.model.shapes.ShapeBuilder;

/**
 * Factory class for creating drag selection markers of a given size
 */
public class DragSelectionFactory implements IShapeFactory {

    private static DragSelectionFactory instance;
    private Shape selectionBoxPrototype;

    private DragSelectionFactory() {
        // We build a selection box prototype with the following default properties
        selectionBoxPrototype = new ShapeBuilder()
                .setShapeName("Square")
                .setColor("0x000000ff")
                .setLineWidth(1)
                .setFillShape(false)
                .setLineDashes(5)
                .build();
    }

    public static DragSelectionFactory getInstance() {
        if (instance == null) {
            return instance = new DragSelectionFactory();
        }
        return instance;
    }

    @Override
    public Shape getDragSelectionBox(double x1, double y1, double x2, double y2) {
        Shape prototype = selectionBoxPrototype.clone();
        prototype.setStartX(x1);
        prototype.setStartY(y1);
        prototype.setEndX(x2);
        prototype.setEndY(y2);
        return prototype;
    }

}
