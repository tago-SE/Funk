package ObjectPainterApp.model.commands;

import ObjectPainterApp.model.CanvasSubject;
import ObjectPainterApp.model.shapes.IShapeMemento;
import ObjectPainterApp.model.shapes.Shape;

import java.util.ArrayList;
import java.util.Collection;
import java.util.List;

public abstract class BaseModifyShapesCommand implements ICommand {

    private final Collection<Shape> shapes;
    private final List<IShapeMemento> mementos = new ArrayList<>();
    private final CanvasSubject canvas;

    BaseModifyShapesCommand(CanvasSubject canvas) {
        this.canvas = canvas;
        this.shapes = canvas.getSelectedShapes();
    }

    @Override
    public ICommand doAction() {
        for (Shape shape : shapes) {
            mementos.add(shape.getMemento());  // save state
            onShapeModification(shape);
        }
        canvas.clearSelection();
        canvas.notifyObservers();
        return this;
    }

    abstract protected void onShapeModification(Shape shape);

    @Override
    public ICommand undoAction() {
        int index = 0;
        for (Shape shape : shapes)
            shape.setMemento(mementos.get(index++)); // restores state
        canvas.clearSelection();
        canvas.notifyObservers();
        return this;
    }

    @Override
    abstract public String getName();

    @Override
    public boolean hasUndo() {
        return true;
    }

}
