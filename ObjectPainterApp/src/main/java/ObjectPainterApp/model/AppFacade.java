package ObjectPainterApp.model;

import ObjectPainterApp.model.commands.*;
import ObjectPainterApp.model.shapes.*;
import ObjectPainterApp.utils.IObserver;

import java.util.*;
import java.util.logging.Logger;

import static ObjectPainterApp.model.Settings.*;

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


    public void subscribeToCanvas(IObserver o) {
        canvasSubject.addObserver(o);
    }

    public void unsubscribeToCanvas(IObserver o) {
        canvasSubject.removeObserver(o);
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

    private void loadDrawableOperations() {
        // In case the implementation becomes more complex this part could be changed
        String[] operations = {"undo", "redo", "selection", "delete"};
        drawableOperations = Arrays.asList(operations);
    }

    public Collection<String> getDrawableShapeTypes() {
        return drawableShapeTypes;
    }

    public Collection<String> getDrawMenuOperations() {
        return drawableOperations;
    }

    public void onOperationSelection(String operation) {
        LOGGER.info("Operation: " + operation);
        clearPreviousSelection();
        switch (operation) {
            case "selection":
                selectionEnabled = true;
                break;
            case "delete":
                commandManager.execute(new RemoveShapesCommand(canvasSubject));
                break;
            case "undo":
                commandManager.undo();
                break;
            case "redo":
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
}
