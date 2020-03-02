package ObjectPainterApp.model.shapes;

import ObjectPainterApp.model.shapes.factory.DrawableShapeFactory;
import ObjectPainterApp.model.shapes.factory.ShapeType;

/**
 * A factory for creating new Shape objects with various properties. The current implementation uses a mix between
 * Builder-pattern and Prototype pattern.
 *
 * The Builder Pattern was chosen to minimize constructor pollution in the shape class, additionally, adding new
 * properties should be fairly simple. Simply add methods to configure them in the ShapeBuilder class, and modify the
 * assembly of the Shape object inside the Shape class.
 *
 * The Prototype pattern was chosen to allow for finding Shape Class at run-time and being able to recreate them without
 * having to specify which Shape-Types that are available inside the build method, in case new Shape classes are added,
 * or old classes removed.
 *
 * The combination of these two patterns can be seen inside the rebuild() method, which first clones the prototype shape
 * and then rebuilds the cloned shape, in order to implement the property changes.
 *
 * Ref:
 * 1. https://github.com/iluwatar/java-design-patterns/tree/master/builder
 * 2. https://github.com/iluwatar/java-design-patterns/tree/master/prototype
 * 3. https://www.tutorialspoint.com/design_pattern/prototype_pattern.htm
 */
public class ShapeBuilder {

    // Current shape properties
    private ShapeState state = new ShapeState();

    // Selected Shape Type
    private ShapeType type;


    public ShapeBuilder() {}

    public ShapeBuilder(String shapeName, String color, int lineWidth, boolean fillShape) {
        setColor(color);
        setLineWidth(lineWidth);
        setFillShape(fillShape);
        if (shapeName != null)
            setType(ShapeType.valueOf(shapeName.toUpperCase())); // hack override
    }

    public ShapeBuilder setStartXY(double x, double y) {
        state.startX = x;
        state.startY = y;
        return this;
    }

    public ShapeBuilder setEndXY(double x, double y) {
        state.endX = x;
        state.endY = y;
        return this;
    }

    public ShapeBuilder setParam(double startX, double startY, double endX, double endY) {
        setStartXY(startX, startY);
        setEndXY(endX, endY);
        return this;
    }

    public ShapeBuilder setType(ShapeType type) {
        this.type = type;
        return this;
    }

    public ShapeBuilder setLineWidth(int lineWidth) {
        state.lineWidth = lineWidth;
        return this;
    }

    public ShapeBuilder setLineDashes(int lineDashes) {
        state.lineDashes = lineDashes;
        return this;
    }

    public ShapeBuilder setColor(String color) {
        state.color = color;
        return this;
    }

    public ShapeBuilder setFillShape(boolean fillShape) {
        state.filled = fillShape;
        return this;
    }

    public ShapeBuilder clearShapeName() {
        type = null;
        return this;
    }

    /**
     * This will allocate a Shape instance with the specified properties.
     *
     * @return Created Shape
     */
    public Shape build() {
        if (state.color == null)
            throw new IllegalStateException("Trying to build but no color was specified.");
        if (type == null)
            throw new IllegalStateException("Trying to build but no shape was specified.");
        if (state.lineWidth <= 0)
            state.lineWidth = 1;
        Shape prototype = DrawableShapeFactory.getInstance().getShapePrototype(type);
        prototype.setMemento(state);
        return prototype;
    }

    public boolean hasShape() {
        return type != null;
    }

}