package ObjectPainterApp.model.shapes;

import ObjectPainterApp.model.shapes.factory.ShapeType;

public class Hexagon extends Polygon {

    public Hexagon() {
        super(6);
    }

    @Override
    public ShapeType getType() {
        return ShapeType.HEXAGON;
    }
}
