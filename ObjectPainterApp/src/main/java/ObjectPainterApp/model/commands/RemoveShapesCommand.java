package ObjectPainterApp.model.commands;

import ObjectPainterApp.model.CanvasSubject;
import ObjectPainterApp.model.shapes.IShapeMemento;
import ObjectPainterApp.model.shapes.Shape;

import java.util.ArrayList;
import java.util.Collection;
import java.util.Iterator;
import java.util.List;

public class RemoveShapesCommand implements ICommand {

    private final CanvasSubject canvas;
    private final Collection<Shape> shapesToRemove;
    private final List<IShapeMemento> mementos = new ArrayList<>();

    public RemoveShapesCommand(CanvasSubject canvas) {
        this.canvas = canvas;
        this.shapesToRemove = canvas.getCurrentShapes();
    }

    @Override
    public ICommand doAction() {
        shapesToRemove.forEach(shape -> {
            canvas.removeShape(shape);
            mementos.add(shape.getMemento());
        });
        canvas.notifyObservers();
        return this;
    }

    @Override
    public ICommand undoAction() {
        int index = 0;
        for (Shape shape : shapesToRemove) {
            shape.setMemento(mementos.get(index));
            canvas.addShape(shape);
            index++;
        }
        canvas.notifyObservers();
        return this;
    }

    @Override
    public String getName() {
        return "removeShapes";
    }
}
