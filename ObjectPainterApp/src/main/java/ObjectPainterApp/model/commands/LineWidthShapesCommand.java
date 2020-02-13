package ObjectPainterApp.model.commands;

import ObjectPainterApp.model.CanvasSubject;
import ObjectPainterApp.model.shapes.Shape;

public class LineWidthShapesCommand extends BaseModifyShapesCommand {

    private final int lineWidth;

    public LineWidthShapesCommand(int lineWidth, CanvasSubject canvas) {
        super(canvas);
        this.lineWidth = lineWidth;
    }

    @Override
    public ICommand doAction() {
        for (Shape shape : shapes) {
            mementos.add(shape.getMemento());
            shape.setLineWidth(lineWidth);
        }
        canvas.clearSelection();
        canvas.notifyObservers();
        return this;
    }

    @Override
    public String getName() {
        return "LineWidthShapes";
    }

}
