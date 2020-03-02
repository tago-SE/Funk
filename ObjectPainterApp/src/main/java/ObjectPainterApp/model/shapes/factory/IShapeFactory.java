package ObjectPainterApp.model.shapes.factory;


import ObjectPainterApp.model.shapes.Shape;

import java.util.List;

public interface IShapeFactory {

    Shape getShapePrototype(ShapeType type);


    /**
     * Returns a list of the shapes that this factory can produce.
     * @return
     */
    List<ShapeType> types();

}
