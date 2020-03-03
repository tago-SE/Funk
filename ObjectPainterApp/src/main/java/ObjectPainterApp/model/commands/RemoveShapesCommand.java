package ObjectPainterApp.model.commands;

import ObjectPainterApp.model.CanvasSubject;
import ObjectPainterApp.model.shapes.Shape;

import java.util.Collection;

public class RemoveShapesCommand implements ICommand {

    private final CanvasSubject canvas;
    private final Collection<Shape> shapesToRemove;

    public RemoveShapesCommand(CanvasSubject canvas) {
        this.canvas = canvas;
        this.shapesToRemove = canvas.getSelectedShapes();
    }

    @Override
    public ICommand doAction() {
        shapesToRemove.forEach(canvas::removeShape);
        canvas.clearSelection();
        canvas.notifyObservers();
        return this;
    }

    @Override
    public ICommand undoAction() {
        shapesToRemove.forEach(canvas::addShape);
        canvas.clearSelection();
        canvas.notifyObservers();
        return this;
    }

    @Override
    public String getName() {
        return "RemoveShapes";
    }

    @Override
    public boolean hasUndo() {
        return true;
    }
}
