package ObjectPainterApp.model.shapes.factory;

import ObjectPainterApp.model.shapes.Shape;

public interface IDragSelectionFactory {

    /**
     * Returns a selection box based on the selected shape
     *
     * @param selectedShape the current selected shape composite
     *
     * @return new Selection Box
     */
    Shape getDragSelectionBox(Shape selectedShape);

    /**
     *  Returns a selection box based on the provided parameters
     *
     * @param x1 top left
     * @param y1 top left
     * @param x2 bot right
     * @param y2 bot right
     *
     * @return new Selection Box
     */
    Shape getDragSelectionBox(double x1, double y1, double x2, double y2);

}
