package ObjectPainterApp.model.commands;

import ObjectPainterApp.model.CanvasSubject;
import ObjectPainterApp.model.shapes.IMemento;
import ObjectPainterApp.model.shapes.Shape;

import java.util.ArrayList;
import java.util.Collection;
import java.util.List;

public abstract class BaseModifyShapesCommand implements ICommand {

    protected final Collection<Shape> shapes;
    protected final List<IMemento> mementos = new ArrayList<>();
    protected final CanvasSubject canvas;

    BaseModifyShapesCommand(CanvasSubject canvas) {
        this.canvas = canvas;
        this.shapes = canvas.getSelectedShapes();
    }

    @Override
    public abstract ICommand doAction();

    @Override
    public ICommand undoAction() {
        int index = 0;
        for (Shape shape : shapes) shape.setMemento(mementos.get(index++));
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
