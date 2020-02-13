package ObjectPainterApp.model.shapes;

import java.util.Collection;

public interface IShapeComposite {

    void addChildren(Shape shape);
    void removeChildren(Shape shape);
    Collection<Shape> getChildren();
    void clear();

}
