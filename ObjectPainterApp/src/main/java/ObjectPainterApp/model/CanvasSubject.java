package ObjectPainterApp.model;

import ObjectPainterApp.model.shapes.*;
import ObjectPainterApp.model.shapes.drag.DragSelectionFactory;
import ObjectPainterApp.utils.IObserver;
import ObjectPainterApp.utils.ISubject;

import java.util.*;
import java.util.logging.Logger;

public class CanvasSubject implements ISubject {

    private static final Logger LOGGER = Logger.getLogger(AppFacade.class.getName());

    private ShapeComposite canvasShapes = new ShapeComposite();
    private ShapeComposite selectedShapes = new ShapeComposite();
    private ShapeComposite selectedMarker = new ShapeComposite();

    private List<IObserver> observerList;

    public CanvasSubject() {
        reset();
    }

    public void addShape(Shape shape) {
        if (canvasShapes.getChildren().contains(shape)) {

        } else {
            canvasShapes.addChildren(shape);
        }
    }

    public void removeShape(Shape shape) {
        canvasShapes.removeChildren(shape);
    }

    public void selectIntersectingShapes(Shape otherShape) {
        selectedMarker.clear();
        selectedShapes.clear();
        for (Shape s : canvasShapes.getChildren()) {
            if (otherShape.intersects(s)) {
                LOGGER.info("INTERSECTS: " + s);
                selectedShapes.addChildren(s);
            }
        }
        if (selectedShapes.getChildren().size() > 0) {
            double x1 = selectedShapes.getLeftX() - 10;
            double y1 = selectedShapes.getTopY() - 10;
            double x2 = selectedShapes.getRightX() + 10;
            double y2 = selectedShapes.getBotY() + 10;
            Shape marker = DragSelectionFactory.getInstance().getDragSelectionBox(x1, y1, x2, y2);
            selectedMarker.addChildren(marker);
        }
    }

    public void clearSelection() {
        if (selectedShapes.getChildren().size() == 0)
            return;
        selectedMarker.clear();
        selectedShapes.clear();
    }

    private void reset() {
        canvasShapes.clear();
        canvasShapes.addChildren(selectedMarker);
        clearSelection();
    }

    public void clear() {
        reset();
    }

    public Collection<Shape> getSelectedShapes() {
        return new ArrayList<>(selectedShapes.getChildren());
    }

    public Collection<Shape> getCurrentShapes() {
        return new ArrayList<>(canvasShapes.getChildren());
    }

    @Override
    public void addObserver(IObserver observer) {
        if (observerList == null)
            observerList = new ArrayList<>();
        observer.onChange(this); // alert of current state
        if (observerList.contains(observer))
            return;
        observerList.add(observer);
    }

    @Override
    public void removeObserver(IObserver observer) {
        if (observerList == null)
            return;
        observerList.remove(observer);
    }

    @Override
    public void notifyObservers() {
        for (IObserver o : observerList)
            o.onChange(this);
    }

}
