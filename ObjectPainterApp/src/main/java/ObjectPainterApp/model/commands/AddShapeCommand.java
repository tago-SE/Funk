package ObjectPainterApp.model.commands;

import ObjectPainterApp.model.AppFacade;
import ObjectPainterApp.model.CanvasSubject;
import ObjectPainterApp.model.shapes.IShapeMemento;
import ObjectPainterApp.model.shapes.Shape;

import java.util.logging.Logger;

public class AddShapeCommand implements ICommand {

    private final Shape shape;
    private final CanvasSubject canvas;

    public AddShapeCommand(Shape shapeToCreate, CanvasSubject canvas) {
        this.shape = shapeToCreate;
        this.canvas = canvas;
    }

    @Override
    public ICommand doAction() {
        canvas.addShape(shape);
        canvas.notifyObservers();
        return this;
    }

    @Override
    public ICommand undoAction() {
        canvas.removeShape(shape);
        canvas.notifyObservers();
        return this;
    }

    @Override
    public String getName() {
        return "AddShape";
    }

}
