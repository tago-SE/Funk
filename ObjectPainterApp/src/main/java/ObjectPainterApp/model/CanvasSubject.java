package ObjectPainterApp.model;

import ObjectPainterApp.model.shapes.*;
import ObjectPainterApp.utils.IObserver;
import ObjectPainterApp.utils.ISubject;
import javafx.scene.canvas.Canvas;

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
                double x1 = s.getStartX() - 10;
                double y1 = s.getStartY() - 10;
                double x2 = s.getEndX() + 10;
                double y2 = s.getEndY() + 10;
                Shape marker = DragSelectionFactory.getInstance().getDragSelectionBox(x1, y1, x2, y2);
                selectedMarker.addChildren(marker);
                selectedShapes.addChildren(s);
            }
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
        notifyObservers();
    }

    public Collection<Shape> getSelectedShapes() {
        return new ArrayList<>(selectedShapes.getChildren());
    }

    public Collection<Shape> getCurrentShapes() {
        // We protect the integrity of the list by creating a new one.
        return new ArrayList<>(canvasShapes.getChildren());
    }

    @Override
    public void addObserver(IObserver observer) {
        if (observerList == null)
            observerList = new ArrayList<>();
        else if (observerList.contains(observer))
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
