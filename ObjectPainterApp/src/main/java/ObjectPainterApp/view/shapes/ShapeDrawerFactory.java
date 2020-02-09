package ObjectPainterApp.view.shapes;

import ObjectPainterApp.model.shapes.Shape;

public class ShapeDrawerFactory {

    private ShapeDrawerCache shapeDrawerCache = ShapeDrawerCache.getInstance();

    public ShapeDrawer createDrawer(Shape shape) {
        ShapeDrawer prototype = shapeDrawerCache.getShape(shape.getName());
        if (prototype != null) {
            return prototype.clone().rebuild(shape);
        }
        return null;
    }

}
