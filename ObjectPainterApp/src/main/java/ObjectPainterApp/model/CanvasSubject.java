package ObjectPainterApp.model;

import ObjectPainterApp.model.shapes.Shape;
import ObjectPainterApp.utils.IObserver;
import ObjectPainterApp.utils.ISubject;

import java.util.*;

public class CanvasSubject implements ISubject {

    List<Shape> shapes = new LinkedList<>();

    private List<IObserver> observerList;

    public void addShape(Shape shape) {
        updateShape(shape);
    }

    public void removeShape(Shape shape) {
        if (shape.getId() == null || shape.getId().equals(""))
            throw new IllegalStateException("Shape has no id.");
        Iterator<Shape> itr = shapes.iterator();
        while (itr.hasNext()) {
            Shape s = itr.next();
            if (s.getId().equals(shape.getId())) {
                itr.remove();
                notifyObservers();
                return;
            }
        }
    }

    public void updateShape(Shape shape) {
        if (shape.getId() == null || shape.getId().equals(""))
            throw new IllegalStateException("Shape has no id.");
        Iterator<Shape> itr = shapes.iterator();
        while (itr.hasNext()) {
            Shape s = itr.next();
            if (s.getId().equals(shape.getId())) {
                s = shape;
                notifyObservers();
                return;
            }
        }
        shapes.add(shape);
        notifyObservers();
    }

    public void clear() {
        shapes.clear();
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
