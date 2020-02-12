package ObjectPainterApp.model.commands;

import ObjectPainterApp.model.CanvasSubject;
import ObjectPainterApp.model.shapes.Shape;

public class AddShapeCommand implements ICommand {

    private final Shape shape;
    private final CanvasSubject canvas;

    public AddShapeCommand(Shape shapeToCreate, CanvasSubject canvas) {
        this.shape = shapeToCreate;
        this.canvas = canvas;
    }

    @Override
    public ICommand doAction() {
        canvas.addOrUpdateShape(shape);
        return this;
    }

    @Override
    public ICommand undoAction() {
        canvas.removeShape(shape);
        return this;
    }

    @Override
    public String getName() {
        return null;
    }

}
