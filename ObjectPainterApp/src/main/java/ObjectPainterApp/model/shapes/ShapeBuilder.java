package ObjectPainterApp.model.shapes;

import java.util.UUID;
import java.util.logging.Logger;

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

    private static final Logger LOGGER = Logger.getLogger(ShapeBuilder.class.getName());

    private static final String NO_MATCHING_SHAPE_ERR = "Cannot find matching shape: %s.";

    private String shapeName;
    private String color;
    private int lineWidth;
    private boolean fillShape;
    private double startX, startY, endX, endY;

    private ShapeCache shapeCache = ShapeCache.getInstance();

    private boolean shapeNameExists() {
        return hasShape() && shapeCache.getShapeTypes().contains(shapeName);
    }

    public ShapeBuilder(String shapeName, String color, int lineWidth, boolean fillShape) {
        setColor(color);
        setLineWidth(lineWidth);
        setFillShape(fillShape);
        setShapeName(shapeName);
    }

    public ShapeBuilder setStartXY(double x, double y) {
        this.startX = x;
        this.startY = y;
        return this;
    }

    public ShapeBuilder setEndXY(double x, double y) {
        this.endX = x;
        this.endY = y;
        return this;
    }

    public ShapeBuilder setParam(double startX, double startY, double endX, double endY) {
        setStartXY(startX, startY);
        setEndXY(endX, endY);
        return this;
    }

    public ShapeBuilder setShapeName(String shapeName) {
        this.shapeName = shapeName;
        if (!shapeNameExists()) {
            LOGGER.severe(String.format(NO_MATCHING_SHAPE_ERR, shapeName));
            this.shapeName = "";
        }
        return this;
    }

    public ShapeBuilder setLineWidth(int lineWidth) {
        this.lineWidth = lineWidth;
        return this;
    }

    public ShapeBuilder setColor(String color) {
        this.color = color;
        return this;
    }

    public ShapeBuilder setFillShape(boolean fillShape) {
        this.fillShape = fillShape;
        return this;
    }

    /**
     * This will allocate a Shape instance with the specified properties.
     *
     * @return Created Shape
     */
    public Shape build() {
        if (color == null)
            throw new IllegalStateException("Trying to build but no color was specified.");
        if (shapeName == null || shapeName.equals(""))
            throw new IllegalStateException("Trying to build but no shape was specified.");
        if (!shapeNameExists()) {
            throw new IllegalStateException("Trying to build a shape which does not exists.");
        }
        if (lineWidth <= 0) lineWidth = 1;
        Shape prototype = shapeCache.getShape(shapeName);
        return prototype.clone().rebuild(this).setId(UUID.randomUUID().toString());
    }

    public boolean hasShape() {
        return shapeName != null && !shapeName.equals("");
    }

    public String getShapeName() {
        return shapeName;
    }

    public String getColor() {
        return color;
    }

    public int getLineWidth() {
        return lineWidth;
    }

    public boolean isFillShape() {
        return fillShape;
    }

    public double getStartX() {
        return startX;
    }

    public double getStartY() {
        return startY;
    }

    public double getEndX() {
        return endX;
    }

    public double getEndY() {
        return endY;
    }



}