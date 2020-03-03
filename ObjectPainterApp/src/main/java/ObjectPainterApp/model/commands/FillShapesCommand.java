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
    protected void onShapeModification(Shape shape) {
        shape.setFilled(filled);
    }

    @Override
    public String getName() {
        return "FillSelectedShape";
    }

}
