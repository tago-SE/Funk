package ObjectPainterApp.model.shapes;

import java.util.ArrayList;
import java.util.List;

/**
 * There are various ways to implement the Composite Pattern. You can design for uniformity or type safety.
 *
 * Design for uniformity: child related operations are defined in the Component interface. Thus, clients can treat
 * Leafs and Composite objects the same way.
 *
 * Design for type safety: child related operations are defined in the Composite class. Type safety is gained as clients
 * can no longer perform child-related operations on leaf-objects.
 */
public class ShapeComposite implements IShapeComponent {

    private List<IShapeComponent> children = new ArrayList<>();

    public ShapeComposite() {

    }

    public void addChild(IShapeComponent child) {
        children.add(child);
    }

    public void removeChild(IShapeComponent child) {
        children.remove(child);
    }

    public void clear() {
        children.clear();
    }

    @Override
    public void setColor(String color) {
        children.forEach(child -> child.setColor(color));
    }

    @Override
    public void setFilled(boolean fill) {
        children.forEach(child -> child.setFilled(fill));
    }

    @Override
    public void setLineWidth(int lineWidth) {
        children.forEach(child -> child.setLineWidth(lineWidth));
    }

    @Override
    public void setSelected(boolean selected) {
        children.forEach(child -> child.setSelected(selected));
    }


}
