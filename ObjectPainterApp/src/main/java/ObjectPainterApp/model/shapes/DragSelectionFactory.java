package ObjectPainterApp.model.shapes;

public class DragSelectionFactory implements IDragSelectionFactory {

    private static DragSelectionFactory instance;
    private Shape selectionBoxPrototype;

    private DragSelectionFactory() {
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
