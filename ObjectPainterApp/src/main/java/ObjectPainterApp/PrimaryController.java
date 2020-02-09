package ObjectPainterApp;

import java.io.IOException;
import java.net.URL;
import java.util.*;
import java.util.logging.Logger;

import ObjectPainterApp.model.AppFacade;
import ObjectPainterApp.model.CanvasSubject;
import ObjectPainterApp.model.shapes.Shape;
import ObjectPainterApp.utils.ISubject;
import ObjectPainterApp.utils.IObserver;
import ObjectPainterApp.view.ShapeMenuButtonFactory;
import ObjectPainterApp.view.shapes.ShapeDrawer;
import ObjectPainterApp.view.shapes.ShapeDrawerFactory;
import javafx.application.Platform;
import javafx.collections.FXCollections;
import javafx.collections.ObservableList;
import javafx.event.ActionEvent;
import javafx.fxml.FXML;
import javafx.fxml.Initializable;
import javafx.scene.canvas.Canvas;
import javafx.scene.canvas.GraphicsContext;
import javafx.scene.control.CheckBox;
import javafx.scene.control.ColorPicker;
import javafx.scene.control.ComboBox;
import javafx.scene.control.ToggleButton;
import javafx.scene.input.MouseEvent;
import javafx.scene.layout.HBox;
import javafx.scene.paint.Color;

public class PrimaryController implements Initializable, IObserver {

    private static final Logger LOGGER = Logger.getLogger(PrimaryController.class.getName());

    @FXML public ColorPicker colorPicker;
    @FXML public CheckBox fill;
    @FXML public ComboBox lineWidthSelectionList;
    @FXML public Canvas canvas;
    @FXML public HBox objectsBox;

    private AppFacade appFacade = AppFacade.getInstance();

    private boolean canvasMouseDragStarted = false;
    private ShapeDrawerFactory shapeDrawerFactory = new ShapeDrawerFactory();

    @Override
    public void initialize(URL url, ResourceBundle resourceBundle) {

        appFacade.subscribeToCanvas(this);

        String[] widths = {"1", "2", "3", "4", "5", "6", "7"};
        List<String> list = new ArrayList(Arrays.asList(widths));
        ObservableList<String> items = FXCollections.observableArrayList(list);
        lineWidthSelectionList.setItems(items);
        lineWidthSelectionList.getSelectionModel().select(0);

        Platform.runLater(() -> {
            for (ToggleButton button : ShapeMenuButtonFactory.getInstance()
                    .createShapeMenuButtons(this, appFacade.getShapeTypes())) {
                objectsBox.getChildren().add(button);
                button.setOnAction(e -> {
                    appFacade.onShapeMenuOptionSelection(button.getId());
                });
            }

        });
    }

    @FXML
    private void switchToSecondary() throws IOException {
        App.setRoot("secondary");
    }

    public void onColorSelection(ActionEvent actionEvent) {
        Color c = colorPicker.getValue();
        System.out.println(c.toString());
        System.out.println("New Color's RGB = "+c.getRed()+" "+c.getGreen()+" "+c.getBlue());
        appFacade.onColorSelection(c.toString());
    }

    public void onFillSelection(ActionEvent actionEvent) {
        System.out.println("Fill: " + fill.isSelected());
        appFacade.onFillShapeSelection(fill.isSelected());
    }

    public void setLineWidth(ActionEvent actionEvent) {
        System.out.println("Line width selection: " + lineWidthSelectionList.getValue());
        appFacade.onLineWidthSelection(Integer.parseInt((String) lineWidthSelectionList.getValue()));
    }


    public void onCanvasMouseDrag(MouseEvent mouseEvent) {
        if (!canvasMouseDragStarted) {
            System.out.println("Canvas Mouse Drag Started: " + mouseEvent.getX() + ", " + mouseEvent.getY());
            canvasMouseDragStarted = true;
        } else {
            appFacade.onCanvasDrag(mouseEvent.getX(), mouseEvent.getY());
        }
    }

    public void onCanvasMousePressed(MouseEvent mouseEvent) {
        System.out.println("Canvas Mouse Pressed: " + mouseEvent.getX() + ", " + mouseEvent.getY());
        appFacade.onCanvasSelection(mouseEvent.getX(), mouseEvent.getY());
    }

    public void onCanvasMouseReleased(MouseEvent mouseEvent) {
        if (canvasMouseDragStarted) {
            System.out.println("Canvas Drag Ended: " + mouseEvent.getX() + ", " + mouseEvent.getY());
            appFacade.onCanvasDragEnded(mouseEvent.getX(), mouseEvent.getY());
        }
        canvasMouseDragStarted = false;
    }

    private void clearCanvas(GraphicsContext gc) {
        gc.clearRect(0, 0, gc.getCanvas().getWidth(), gc.getCanvas().getHeight());
    }

    @Override
    public void onChange(ISubject subject) {
        if (subject instanceof CanvasSubject) {
            clearCanvas(canvas.getGraphicsContext2D());
            Collection<Shape> shapes = ((CanvasSubject) subject).getShapes();
            for (Shape s : shapes) {

                ShapeDrawer drawer = shapeDrawerFactory.createDrawer(s);
                System.out.println(drawer);
                drawer.draw(canvas.getGraphicsContext2D());
            }
        }
    }
}
