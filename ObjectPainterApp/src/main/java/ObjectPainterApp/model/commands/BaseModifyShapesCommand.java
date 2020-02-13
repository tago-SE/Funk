package ObjectPainterApp.model.commands;

import ObjectPainterApp.model.CanvasSubject;
import ObjectPainterApp.model.shapes.IShapeMemento;
import ObjectPainterApp.model.shapes.Shape;

import java.util.ArrayList;
import java.util.Collection;
import java.util.List;

public abstract class BaseModifyShapesCommand implements ICommand {

    protected final Collection<Shape> shapes;
    protected final List<IShapeMemento> mementos = new ArrayList<>();
    protected final CanvasSubject canvas;

    public BaseModifyShapesCommand(CanvasSubject canvas) {
        this.canvas = canvas;
        this.shapes = canvas.getSelectedShapes();
    }

    @Override
    public abstract ICommand doAction();

    /**
     * Basic implementation to undo a changed property.
     * @return The executing undo command
     */
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

}
