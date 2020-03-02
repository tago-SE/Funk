package ObjectPainterApp.model.shapes;


import ObjectPainterApp.model.shapes.factory.ShapeType;


public class Octagon extends Polygon {

    public Octagon() {
        super(8);
    }

    @Override
    public ShapeType getType() {
        return ShapeType.OCTAGON;
    }

}

