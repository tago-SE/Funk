package ObjectPainterApp.model.commands;

import ObjectPainterApp.model.CanvasSubject;
import ObjectPainterApp.model.shapes.Shape;

public class PaintShapesCommand extends BaseModifyShapesCommand {

    private final String color;

    public PaintShapesCommand(String color, CanvasSubject canvas) {
        super(canvas);
        this.color = color;
    }

    @Override
    public ICommand doAction() {
        for (Shape shape : shapes) {
            mementos.add(shape.getMemento());
            shape.setColor(color);
        }
        canvas.clearSelection();
        canvas.notifyObservers();
        return this;
    }

    @Override
    public String getName() {
        return "PaintSelectedShapes";
    }


}
