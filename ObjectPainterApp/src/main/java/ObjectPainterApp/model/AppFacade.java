package ObjectPainterApp.model;

import ObjectPainterApp.model.commands.AddShapeCommand;
import ObjectPainterApp.model.commands.CommandManager;
import ObjectPainterApp.model.commands.RemoveShapesCommand;
import ObjectPainterApp.model.shapes.IShapeComponent;
import ObjectPainterApp.model.shapes.Shape;
import ObjectPainterApp.model.shapes.ShapeBuilder;
import ObjectPainterApp.model.shapes.ShapeComposite;
import ObjectPainterApp.utils.IObserver;

import java.util.*;
import java.util.logging.Logger;

/**
 * Singleton Application Facade Class
 */
public class AppFacade {

    private static final Logger LOGGER = Logger.getLogger(AppFacade.class.getName());

    private static AppFacade instance = null;


    private ShapeBuilder shapeBuilder =
            new ShapeBuilder(null, "0x000000ff", 2, false);

    private ShapeBuilder selectionBuilder =
            new ShapeBuilder("Square", "0x000000ff", 1, false).setSelected(true);

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
            //if (!(shape instanceof IShapeComponent))
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
        if (operation.equals("selection")) {
            selectionEnabled = true;
        }
        else if (operation.equals("delete")) {
            commandManager.execute(new RemoveShapesCommand(canvasSubject.getSelectedShapes(), canvasSubject));
        }
        else if (operation.equals("undo")) {
            commandManager.undo();
        }
        else if (operation.equals("redo")) {
            commandManager.redo();
        }
    }

    // TODO: Merge with onOperationSelection
    public void onColorSelection(String color) {
        LOGGER.info("Builder:color: " + color);
        if (isSelectionEnabled()) {

        }
        shapeBuilder.setColor(color);
        clearPreviousSelection();
    }

    // TODO: Merge with onOperationSelection
    public void onLineWidthSelection(int lineWidth) {
        LOGGER.info("Builder:lineWidth: " + lineWidth);
        shapeBuilder.setLineWidth(lineWidth);
        clearPreviousSelection();;
    }

    // TODO: Merge with onOperationSelection
    public void onFillShapeSelection(boolean b) {
        LOGGER.info("Builder:fill: " + b);
        shapeBuilder.setFillShape(b);
        clearPreviousSelection();
    }

    public void onShapeMenuOptionSelection(String shapeName) {
        LOGGER.info("Builder:shape: " + shapeName);
        shapeBuilder.setShapeName(shapeName);
    }

    private boolean isSelectionEnabled() {
        return selectionEnabled;
    }

    public void onCanvasSelection(double x, double y) {
        if (isSelectionEnabled()) {
            LOGGER.info("selection box created");
            selectionBuilder.setParam(x, y, x, y);
            lastBuiltShape = selectionBuilder.build();
            canvasSubject.addOrUpdateShape(lastBuiltShape);
        }
        else if (shapeBuilder.hasShape()) {
            LOGGER.info("shape created");
            shapeBuilder.setParam(x, y, x, y);
            lastBuiltShape = shapeBuilder.build();
            canvasSubject.addOrUpdateShape(lastBuiltShape);
        }
    }

    public void onCanvasDrag(double x, double y) {
        if (lastBuiltShape != null) {
            LOGGER.info("updating lastBuiltShape");
            lastBuiltShape.setEndX(x);
            lastBuiltShape.setEndY(y);
            canvasSubject.addOrUpdateShape(lastBuiltShape);
        }
    }

    public void onCanvasDragEnded(double endX, double endY) {
        if (lastBuiltShape != null) {
            LOGGER.info("updating lastBuiltShape");
            lastBuiltShape.setEndX(endX);
            lastBuiltShape.setEndY(endY);
            if (isSelectionEnabled()) {
                canvasSubject.removeShape(lastBuiltShape);
                canvasSubject.selectIntersectionShapes(lastBuiltShape);
            } else {
                commandManager.execute(new AddShapeCommand(lastBuiltShape, canvasSubject));
            }
            lastBuiltShape = null;
        }
    }

}
