package ObjectPainterApp.model.commands;

import ObjectPainterApp.model.CanvasSubject;
import ObjectPainterApp.model.shapes.Shape;

import java.util.List;

public class RemoveShapesCommand implements ICommand {

    private final List<Shape> shapesToRemove;
    private final CanvasSubject canvas;

    public RemoveShapesCommand(List<Shape> shapesToRemove, CanvasSubject canvas) {
        this.shapesToRemove = shapesToRemove;
        this.canvas = canvas;
    }

    @Override
    public ICommand doAction() {
        System.out.println("Removing shapes...");
        System.out.println(shapesToRemove);
        canvas.removeShapes(shapesToRemove);
        return this;
    }

    @Override
    public ICommand undoAction() {
        canvas.addOrUpdateShapes(shapesToRemove);
        return this;
    }

    @Override
    public String getName() {
        return null;
    }
}
