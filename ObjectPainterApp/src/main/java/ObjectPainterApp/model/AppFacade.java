package ObjectPainterApp.model;

import ObjectPainterApp.model.shapes.Shape;
import ObjectPainterApp.model.shapes.ShapeBuilder;
import ObjectPainterApp.model.shapes.ShapeCache;
import ObjectPainterApp.utils.IObserver;

import java.util.ArrayList;
import java.util.Collection;
import java.util.List;
import java.util.Set;
import java.util.logging.Logger;

/**
 * Singleton Application Facade Class
 */
public class AppFacade {

    private static final Logger LOGGER = Logger.getLogger(AppFacade.class.getName());

    // Log Messages
    private static final String SHAPE_DUMMY_CREATE_LOG = "Shape Dummy Created: (%f,%f)";
    private static final String SHAPE_CREATE_LOG = "Shape Created: (%f,%f) to (%f,%f)";

    private static final String SHAPE_NAME_NOT_FOUND_ERR = "Shape does not exist in model package: %s";

    private static final String SHAPES_PACKAGE_LOCATION = "ObjectPainterApp.model.shapes";

    private static AppFacade instance = null;

    private ShapeBuilder shapeBuilder = new ShapeBuilder(null, "0x000000ff", 2, false);

    private Shape lastBuiltShape = null;
    private CanvasSubject canvasSubject = new CanvasSubject();

    public void subscribeToCanvas(IObserver o) {
        canvasSubject.addObserver(o);
    }

    public void unsubscribeToCanvas(IObserver o) {
        canvasSubject.removeObserver(o);
    }

    private AppFacade() {

    }

    public static AppFacade getInstance() {
        if (instance == null)
            return instance = new AppFacade();
        return instance;
    }

    public Collection<String> getShapeTypes() {
        return ShapeCache.getInstance().getShapeTypes();
    }

    public void onShapeMenuOptionSelection(String shapeName) {
        LOGGER.info("Builder:shape: " + shapeName);
        shapeBuilder.setShapeName(shapeName);
    }

    public void onColorSelection(String color) {
        LOGGER.info("Builder:color: " + color);
        shapeBuilder.setColor(color);
    }

    public void onLineWidthSelection(int lineWidth) {
        LOGGER.info("Builder:lineWidth: " + lineWidth);
        shapeBuilder.setLineWidth(lineWidth);
    }

    public void onFillShapeSelection(boolean b) {
        LOGGER.info("Builder:fill: " + b);
        shapeBuilder.setFillShape(b);
    }

    public void onCanvasSelection(double x, double y) {
        if (shapeBuilder.hasShape()) {
            shapeBuilder.setParam(x, y, x, y);
            lastBuiltShape = shapeBuilder.build();
            canvasSubject.addShape(lastBuiltShape);
        }
    }

    public void onCanvasDrag(double x, double y) {
        if (lastBuiltShape != null) {
            lastBuiltShape.setEndX(x);
            lastBuiltShape.setEndY(y);
            canvasSubject.updateShape(lastBuiltShape);
        }
    }

    public void onCanvasDragEnded(double x, double y) {
        if (lastBuiltShape != null) {
            lastBuiltShape.setEndX(x);
            lastBuiltShape.setEndY(y);
            canvasSubject.updateShape(lastBuiltShape);
            lastBuiltShape = null;
        }

    }

}
