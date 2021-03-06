package ObjectPainterApp;

import ObjectPainterApp.model.*;
import ObjectPainterApp.model.menus.MenuComponent;
import ObjectPainterApp.model.menus.MenuFactory;
import ObjectPainterApp.model.shapes.Shape;
import ObjectPainterApp.utils.IObserver;
import ObjectPainterApp.utils.ISubject;
import ObjectPainterApp.view.OperationMenuButtonFactory;
import ObjectPainterApp.view.ShapeMenuButtonFactory;
import javafx.application.Platform;
import javafx.collections.FXCollections;
import javafx.collections.ObservableList;
import javafx.event.ActionEvent;
import javafx.fxml.FXML;
import javafx.fxml.Initializable;
import javafx.scene.canvas.Canvas;
import javafx.scene.canvas.GraphicsContext;
import javafx.scene.control.*;
import javafx.scene.input.MouseEvent;
import javafx.scene.layout.HBox;
import javafx.scene.paint.Color;

import java.io.IOException;
import java.net.URL;
import java.util.*;
import java.util.logging.Logger;

import static ObjectPainterApp.model.Config.LINE_WIDTH_OPTIONS;


public class PrimaryController implements Initializable, IObserver {

    private static final Logger LOGGER = Logger.getLogger(PrimaryController.class.getName());

    @FXML public ColorPicker colorPicker;
    @FXML public CheckBox fill;
    @FXML public ComboBox lineWidthSelectionList;
    @FXML public Canvas canvas;
    @FXML public HBox objectsBox;
    @FXML public HBox menuBox;
    @FXML public MenuBar menuBar;

    private AppFacade appFacade = AppFacade.getInstance();

    private static final int BUTTON_SIZE = 25;
    private boolean canvasMouseDragStarted = false;

    @Override
    public void initialize(URL url, ResourceBundle resourceBundle) {

        appFacade.subscribeToServices(this);
        List<String> list = new ArrayList(Arrays.asList(LINE_WIDTH_OPTIONS));
        ObservableList<String> items = FXCollections.observableArrayList(list);
        lineWidthSelectionList.setItems(items);
        lineWidthSelectionList.getSelectionModel().select(0);

        Platform.runLater(() -> {
            // Setup the operational buttons
            List<ToggleButton> operationMenuButtons = new ArrayList<>();
            for (String operation : appFacade.getDrawMenuOperations()) {
                operationMenuButtons.add(OperationMenuButtonFactory.getInstance()
                        .create(this, operation, BUTTON_SIZE));
            }
            for (ToggleButton button : operationMenuButtons) {
                    menuBox.getChildren().add(button);
                    button.setOnAction(e -> {
                        OperationType opType = OperationType.valueOf(button.getId().toUpperCase());
                        appFacade.onOperation(new Operation(opType));
                    });
            }

            // Setup the shape buttons
            for (ToggleButton button : ShapeMenuButtonFactory.getInstance()
                    .createShapeMenuButtons(this, BUTTON_SIZE, appFacade.getDrawableShapeTypes())) {

                objectsBox.getChildren().add(button);
                button.setOnAction(e -> {
                    appFacade.onShapeMenuOptionSelection(button.getId());
                });
            }

            // Load Settings
            colorPicker.setValue(Color.web(Config.SHAPE_COLOR));
            setupMenuBar();
        });
    }

    // @ TODO: Does recreating the file menu cause OnAction leaks?
    private void setupMenuBar() {
        menuBar.getMenus().clear();
        MenuComponent fileMenu = MenuFactory.getInstance().getFileMenu();
        Menu menuView = new Menu(" " + fileMenu.getName());
        populateMenu(menuView, fileMenu);
        menuBar.getMenus().add(menuView);
    }

    private void populateMenu(Menu rootViewMenu, MenuComponent rootMenu) {
        for (MenuComponent child : rootMenu.getChildren()) {
            if (child.getChildren().size() > 0) {
                Menu subMenu = new Menu(child.getName());
                rootViewMenu.getItems().add(subMenu);
                populateMenu(subMenu, child);
            } else {
                MenuItem menuItem = new MenuItem(child.getName());
                rootViewMenu.getItems().add(menuItem);
                menuItem.setOnAction(actionEvent -> {
                   child.onAction();
                });

            }
        }
    }

    @FXML
    private void switchToSecondary() throws IOException {
        // Simple test to see if the canvas state remains when we switch between views
        App.setRoot("secondary");
    }


    public void onColorSelection(ActionEvent actionEvent) {
        appFacade.onOperation(new Operation(OperationType.SET_COLOR, colorPicker.getValue().toString()));
    }

    public void onFillSelection(ActionEvent actionEvent) {
        appFacade.onOperation(new Operation(OperationType.SET_FILL, fill.isSelected()));
    }

    public void setLineWidth(ActionEvent actionEvent) {
        int value = Integer.parseInt((String) lineWidthSelectionList.getValue());
        appFacade.onOperation(new Operation(OperationType.SET_LINE, value));
    }

    public void onCanvasMouseDrag(MouseEvent mouseEvent) {
        double x = mouseEvent.getX();
        double y = mouseEvent.getY();
        if (!canvasMouseDragStarted) {
            canvasMouseDragStarted = true;
        }
        appFacade.onCanvasDrag(x, y);
    }

    public void onCanvasMousePressed(MouseEvent mouseEvent) {
        double x = mouseEvent.getX();
        double y = mouseEvent.getY();
        appFacade.onCanvasSelection(x, y);
    }

    public void onCanvasMouseReleased(MouseEvent mouseEvent) {
        if (canvasMouseDragStarted) {
            appFacade.onCanvasDragEnded(mouseEvent.getX(), mouseEvent.getY());
        }
        canvasMouseDragStarted = false;
    }

    @Override
    public void onChange(ISubject subject) {
        if (subject instanceof CanvasSubject) {
            renderCanvas(((CanvasSubject) subject).getCurrentShapes());
        }
        else if (subject instanceof IFileManager) {
            setupMenuBar();
        }
    }

    private void clearCanvas(GraphicsContext gc) {
        gc.clearRect(0, 0, gc.getCanvas().getWidth(), gc.getCanvas().getHeight());
    }

    private void renderCanvas(Collection<Shape> shapes) {
        clearCanvas(canvas.getGraphicsContext2D());
        shapes.forEach(shape -> shape.draw(canvas.getGraphicsContext2D()));
    }

}
