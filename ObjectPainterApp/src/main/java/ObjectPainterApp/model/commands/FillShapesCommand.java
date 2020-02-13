package ObjectPainterApp.model.commands;

import ObjectPainterApp.model.CanvasSubject;
import ObjectPainterApp.model.shapes.Shape;

public class FillShapesCommand extends BaseModifyShapesCommand {

    private final boolean filled;

    public FillShapesCommand(boolean filled, CanvasSubject canvas) {
        super(canvas);
        this.filled = filled;
    }

    @Override
    public ICommand doAction() {
        for (Shape shape : shapes) {
            mementos.add(shape.getMemento());
            shape.setFilled(filled);
        }
        canvas.clearSelection();
        canvas.notifyObservers();
        return this;
    }

    @Override
    public String getName() {
        return "FillSelectedShape";
    }

}
