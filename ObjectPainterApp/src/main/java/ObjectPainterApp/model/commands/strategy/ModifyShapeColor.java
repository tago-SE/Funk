package ObjectPainterApp.model.commands.strategy;

import ObjectPainterApp.model.shapes.Shape;

public class ModifyShapeColor implements IStrategy {

    private final Shape modifiedShape;
    private final String color;

    public ModifyShapeColor(Shape modified, String color) {
        this.modifiedShape = modified;
        this.color = color;
    }

    @Override
    public void execute() {
        modifiedShape.setColor(color);
    }

}
