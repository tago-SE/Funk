package ObjectPainterApp.model;

import ObjectPainterApp.model.commands.*;
import ObjectPainterApp.model.shapes.*;
import ObjectPainterApp.utils.IObserver;

import java.util.*;
import java.util.logging.Logger;

import static ObjectPainterApp.model.Config.*;

/**
 * Singleton Application Facade Class
 */
public class AppFacade {

    private static final Logger LOGGER = Logger.getLogger(AppFacade.class.getName());

    private static AppFacade instance = null;

    private ShapeBuilder shapeBuilder =
            new ShapeBuilder(null, SHAPE_COLOR, SHAPE_LINE_WIDTH, SHAPE_FILL);

    private CommandManager commandManager = new CommandManager();

    private Shape lastBuiltShape = null;
    private boolean selectionEnabled = false;

    private CanvasSubject canvasSubject = new CanvasSubject();
    private List<String> drawableShapeTypes = new ArrayList<>();
    private List<String> drawableOperations = new ArrayList<>();


    public void subscribeToServices(IObserver o) {
        canvasSubject.addObserver(o);
        FileManagerSubject.getInstance().addObserver(o);
    }


    private AppFacade() {
        loadDrawableShapeTypes();
        loadDrawableOperations();
    }

    public static AppFacade getInstance() {
        if (instance == null) {
            return instance = new AppFacade();
        }
        return instance;
    }

    private void loadDrawableShapeTypes() {
        for (Shape shape : ShapeCache.getInstance().getShapePrototypes()) {
            if (!(shape instanceof IShapeComposite))
            drawableShapeTypes.add(shape.getName());
        }
    }

    private void clearPreviousSelection() {
        LOGGER.info("clear ");
        shapeBuilder.clearShapeName();
        selectionEnabled = false;
    }

    public int getNumberOfLoggedCommands() {
        return commandManager.size();
    }

    public Collection<Shape> getCanvasShapes() {
        return canvasSubject.getCurrentShapes();
    }

    private void loadDrawableOperations() {
        drawableOperations = Operations.labels();
    }

    public Collection<String> getDrawableShapeTypes() {
        return drawableShapeTypes;
    }

    public Collection<String> getDrawMenuOperations() {
        return drawableOperations;
    }

    public void onOperationSelection(String operation) {
        LOGGER.info("Operation: " + operation);
        Operations o = Operations.labelOf(operation);
        clearPreviousSelection();
        switch (o) {
            case SELECTION:
                selectionEnabled = true;
                break;
            case DELETE:
                commandManager.execute(new RemoveShapesCommand(canvasSubject));
                break;
            case UNDO:
                commandManager.undo();
                break;
            case REDO:
                commandManager.redo();
                break;
        }
    }

    // TODO: Merge with onOperationSelection
    public void onColorSelection(String color) {
        LOGGER.info("Builder:color: " + color);
        if (canvasSubject.getSelectedShapes().size() > 0) {
            commandManager.execute(new PaintShapesCommand(color, canvasSubject));
        }
        shapeBuilder.setColor(color);
    }

    // TODO: Merge with onOperationSelection
    public void onLineWidthSelection(int lineWidth) {
        LOGGER.info("Builder:lineWidth: " + lineWidth);
        if (canvasSubject.getSelectedShapes().size() > 0) {
            commandManager.execute(new LineWidthShapesCommand(lineWidth, canvasSubject));
        }
        shapeBuilder.setLineWidth(lineWidth);
    }

    // TODO: Merge with onOperationSelection
    public void onFillShapeSelection(boolean fill) {
        if (canvasSubject.getSelectedShapes().size() > 0) {
            commandManager.execute(new FillShapesCommand(fill, canvasSubject));
        }
        shapeBuilder.setFillShape(fill);
        clearPreviousSelection();
    }

    public void onShapeMenuOptionSelection(String shapeName) {
        LOGGER.info("Builder:shape: " + shapeName);
        clearPreviousSelection();
        shapeBuilder.setShapeName(shapeName);
    }

    private boolean isSelectionEnabled() {
        return selectionEnabled;
    }

    public void onCanvasSelection(double x, double y) {
        if (isSelectionEnabled()) {
            LOGGER.info("selection box created");
            lastBuiltShape = DragSelectionFactory.getInstance().getDragSelectionBox(x, y, x, y);
            canvasSubject.addShape(lastBuiltShape);
            canvasSubject.notifyObservers();
        }
        else if (shapeBuilder.hasShape()) {
            LOGGER.info("shape created");
            lastBuiltShape = shapeBuilder.setParam(x, y, x, y).build();
            canvasSubject.clearSelection();
            canvasSubject.addShape(lastBuiltShape);
            canvasSubject.notifyObservers();
        }
    }

    public void onCanvasDrag(double x, double y) {
        if (lastBuiltShape != null) {
            LOGGER.info("updating lastBuiltShape");
            lastBuiltShape.setEndX(x);
            lastBuiltShape.setEndY(y);
            canvasSubject.notifyObservers();
        }
    }

    public void onCanvasDragEnded(double endX, double endY) {
        if (lastBuiltShape != null) {
            LOGGER.info("updating lastBuiltShape");
            lastBuiltShape.setEndX(endX);
            lastBuiltShape.setEndY(endY);
            if (isSelectionEnabled()) {
                canvasSubject.removeShape(lastBuiltShape);
                canvasSubject.selectIntersectingShapes(lastBuiltShape);
                canvasSubject.notifyObservers();
            } else {
                commandManager.execute(new AddShapeCommand(lastBuiltShape, canvasSubject));
            }
            lastBuiltShape = null;
        }
    }

    public void clearSelection() {
        canvasSubject.clearSelection();
        canvasSubject.notifyObservers();
    }

    public void loadCanvas(Collection<Shape> shapes) {
        canvasSubject.clear();
        commandManager.clear();
        shapes.forEach(canvasSubject::addShape);
        canvasSubject.notifyObservers();
    }

    public void newCanvas() {
        canvasSubject.clear();
        commandManager.clear();
    }

}
