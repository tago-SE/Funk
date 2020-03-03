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
    protected void onShapeModification(Shape shape) {
        shape.setLineWidth(lineWidth);
    }

    @Override
    public String getName() {
        return "LineWidthShapes";
    }

}
