package ObjectPainterApp.model;

import ObjectPainterApp.model.shapes.Shape;
import ObjectPainterApp.model.shapes.ShapeBuilder;
import ObjectPainterApp.model.shapes.SquareShape;
import ObjectPainterApp.utils.IObserver;
import ObjectPainterApp.utils.ISubject;
import javafx.scene.canvas.Canvas;

import java.util.*;
import java.util.logging.Logger;

public class CanvasSubject implements ISubject {

    private List<Shape> shapes = new ArrayList<>();
    private List<Shape> selectedShapes = new ArrayList<>();
    private List<IObserver> observerList;

    public CanvasSubject() {

    }

    public List<Shape> selectIntersectionShapes(Shape otherShape) {
        selectedShapes.clear();
        for (Shape s : shapes) {
            if (otherShape.intersects(s)) {
                selectedShapes.add(s);
                s.setSelected(true);
            } else {
                s.setSelected(false);
            }
        }
        notifyObservers();
        return selectedShapes;
    }

    public List<Shape> getSelectedShapes() {
        return selectedShapes;
    }

    private boolean removeShapeNoAlert(Shape shape) {
        if (shape.getId() == null || shape.getId().equals(""))
            throw new IllegalStateException("Shape has no id.");
        Iterator<Shape> itr = shapes.iterator();
        while (itr.hasNext()) {
            Shape s = itr.next();
            if (s.getId().equals(shape.getId())) {
                s.setSelected(false); // A removed shape can no longer be selected
                itr.remove();
                return true;
            }
        }
        return false;
    }

    public void removeShape(Shape shape) {
        if (removeShapeNoAlert(shape)) {
            notifyObservers();
        }
    }

    private void addOrUpdateShapeNoAlert(Shape shape) {
        if (shape.getId() == null || shape.getId().equals(""))
            throw new IllegalStateException("Shape has no id.");
        int index = 0;
        boolean foundMatch = false;
        for (Shape s : shapes) {
            if (s.getId().equals(shape.getId())) {
                foundMatch = true;
                break;
            }
            index++;
        }
        if (foundMatch) {
            shapes.set(index, shape);
        } else {
            shapes.add(shape);
        }
    }

    public void addOrUpdateShape(Shape shape) {
        addOrUpdateShapeNoAlert(shape);
        notifyObservers();
    }

    public void clear() {
        shapes.clear();
        notifyObservers();
    }

    public void addOrUpdateShapes(List<Shape> shapesToAdd) {
        shapesToAdd.forEach(this::addOrUpdateShapeNoAlert);
        notifyObservers();
    }

    public void removeShapes(List<Shape> shapesToRemove) {
        shapesToRemove.forEach(this::removeShapeNoAlert);
        notifyObservers();
    }


    public Collection<Shape> getShapes() {
        return shapes;
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
