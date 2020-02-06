package ObjectPainterApp;

import java.io.IOException;
import java.net.URL;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;
import java.util.ResourceBundle;

import javafx.application.Platform;
import javafx.collections.FXCollections;
import javafx.collections.ObservableList;
import javafx.event.ActionEvent;
import javafx.fxml.FXML;
import javafx.fxml.Initializable;
import javafx.scene.canvas.Canvas;
import javafx.scene.control.CheckBox;
import javafx.scene.control.ColorPicker;
import javafx.scene.control.ComboBox;
import javafx.scene.control.ToggleButton;
import javafx.scene.image.Image;
import javafx.scene.image.ImageView;
import javafx.scene.input.DragEvent;
import javafx.scene.input.MouseEvent;
import javafx.scene.layout.HBox;
import javafx.scene.paint.Color;

public class PrimaryController implements Initializable {

    @FXML public ColorPicker colorPicker;
    @FXML public CheckBox fill;
    @FXML public ComboBox lineWidthSelectionList;
    @FXML public ToggleButton toggleButton;
    @FXML public Canvas canvas;
    @FXML public HBox objectsBox;

    @Override
    public void initialize(URL url, ResourceBundle resourceBundle) {

        // Init Line Width
        String[] widths = {"1", "2", "3", "4", "5"};
        List<String> list = new ArrayList(Arrays.asList(widths));
        ObservableList<String> items = FXCollections.observableArrayList(list);
        lineWidthSelectionList.setItems(items);
        lineWidthSelectionList.getSelectionModel().select(0);

        Platform.runLater(() -> {
            ImageView iv = new ImageView(getClass().getResource("images/circle.png").toExternalForm());
            iv.setFitWidth(toggleButton.getWidth() - 4);
            iv.setFitHeight(toggleButton.getHeight() - 4);
            toggleButton.setGraphic(iv);
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
    }

    public void onFillSelection(ActionEvent actionEvent) {
        System.out.println("Fill: " + fill.isSelected());
    }

    public void onCanvasMouseDrag(MouseEvent mouseEvent) {
        System.out.println("Canvas Mouse Drag: " + mouseEvent.getX() + ", " + mouseEvent.getY());
    }

    public void onCanvasMousePressed(MouseEvent mouseEvent) {
        System.out.println("Canvas Mouse Pressed: " + mouseEvent.getX() + ", " + mouseEvent.getY());
    }

    public void onCanvasMouseReleased(MouseEvent mouseEvent) {
        System.out.println("Canvas Mouse Released: " + mouseEvent.getX() + ", " + mouseEvent.getY());
    }

    public void setLineWidth(ActionEvent actionEvent) {
        System.out.println("Line width selection: " + lineWidthSelectionList.getValue());
    }
}
