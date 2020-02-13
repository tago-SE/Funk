package ObjectPainterApp.model.shapes;

public interface IShapeFactory {

    /**
     * Restores a previously created default box type selection marker bound by the specified coordinates.
     * @param x1 Top left x
     * @param y1 Top left y
     * @param x2 Bottom right x
     * @param y2 Bottom right y
     * @return Created Shape
     */
    Shape getDragSelectionBox(double x1, double y1, double x2, double y2);

}
