package ObjectPainterApp.model;

import ObjectPainterApp.model.commands.*;
import ObjectPainterApp.model.shapes.Shape;
import ObjectPainterApp.model.shapes.ShapeBuilder;
import ObjectPainterApp.model.shapes.factory.DragSelectionFactory;
import ObjectPainterApp.model.shapes.factory.DrawableShapeFactory;
import ObjectPainterApp.model.shapes.factory.ShapeType;
import ObjectPainterApp.utils.IObserver;

import java.util.ArrayList;
import java.util.Collection;
import java.util.List;
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

    private List<ShapeType> drawableShapeTypes =  DrawableShapeFactory.getInstance().types();

    private List<String> drawableOperations = new ArrayList<>();


    public void subscribeToServices(IObserver o) {
        canvasSubject.addObserver(o);
        FileManagerSubject.getInstance().addObserver(o);
    }

    private AppFacade() {
        loadDrawableOperations();
    }

    public static AppFacade getInstance() {
        if (instance == null) {
            return instance = new AppFacade();
        }
        return instance;
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
        drawableOperations = OperationLabel.labels();
    }

    public List<ShapeType> getDrawableShapeTypes() {
        return drawableShapeTypes;
    }

    public Collection<String> getDrawMenuOperations() {
        return drawableOperations;
    }

    public void onOperation(IOperation operation) {
        System.out.println(operation.getName());
        clearPreviousSelection();
        switch (operation.getName()) {
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
            case SET_COLOR: onColorSelection((String) operation.getData());
                break;
            case SET_FILL:
                onFillShapeSelection((boolean) operation.getData());
                break;
            case SET_LINE: onLineWidthSelection((int) operation.getData());
                break;
            default:
                throw new IllegalArgumentException("Operation not recognized: " + operation);
        }
        clearSelection();
    }

    private void onColorSelection(String color) {
        LOGGER.info("Builder:color: " + color);
        if (canvasSubject.getSelectedShapes().size() > 0) {
            commandManager.execute(new PaintShapesCommand(color, canvasSubject));
        }
        shapeBuilder.setColor(color);
    }

    private void onLineWidthSelection(int lineWidth) {
        LOGGER.info("Builder:lineWidth: " + lineWidth);
        if (canvasSubject.getSelectedShapes().size() > 0) {
            commandManager.execute(new LineWidthShapesCommand(lineWidth, canvasSubject));
        }
        shapeBuilder.setLineWidth(lineWidth);
    }

    private void onFillShapeSelection(boolean fill) {
        LOGGER.info("Builder:fill: " + fill);
        if (canvasSubject.getSelectedShapes().size() > 0) {
            commandManager.execute(new FillShapesCommand(fill, canvasSubject));
        }
        shapeBuilder.setFillShape(fill);
        clearPreviousSelection();
    }

    public void onShapeMenuOptionSelection(String shapeName) {
        LOGGER.info("Builder:shape: " + shapeName);
        clearPreviousSelection();
        shapeBuilder.setType(ShapeType.valueOf(shapeName));
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
            lastBuiltShape.setEndX(x);
            lastBuiltShape.setEndY(y);
            canvasSubject.notifyObservers();
        }
    }

    public void onCanvasDragEnded(double endX, double endY) {
        if (lastBuiltShape != null) {
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
        canvasSubject.notifyObservers();
    }

}
