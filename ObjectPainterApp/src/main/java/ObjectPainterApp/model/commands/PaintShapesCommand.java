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
    protected void onShapeModification(Shape shape) {
        shape.setColor(color);
    }

    @Override
    public String getName() {
        return "PaintSelectedShapes";
    }


}
