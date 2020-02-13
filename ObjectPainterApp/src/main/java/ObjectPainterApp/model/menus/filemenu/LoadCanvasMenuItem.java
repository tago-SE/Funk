package ObjectPainterApp.model.menus.filemenu;

import ObjectPainterApp.model.AppFacade;
import ObjectPainterApp.model.FileManagerSubject;
import ObjectPainterApp.model.menus.MenuComponent;
import ObjectPainterApp.model.shapes.Shape;
import javafx.scene.control.Alert;
import javafx.scene.control.ButtonType;

import java.util.Collection;
import java.util.Optional;

public class LoadCanvasMenuItem extends MenuComponent {

    public LoadCanvasMenuItem(String name) {
        super(name);
    }

    @Override
    public void onAction() {
        Alert alert = new Alert(Alert.AlertType.CONFIRMATION);
        alert.setTitle("Confirmation Dialog");
        alert.setHeaderText("A new canvas will be loaded, any unsaved work on the current one will be lost.");
        alert.setContentText("Are you ok with this?");
        Optional<ButtonType> result = alert.showAndWait();
        if (result.get() == ButtonType.OK){
            Collection<Shape> shapes = FileManagerSubject.getInstance().loadShapesFromFile(getName());
            AppFacade.getInstance().loadCanvas(shapes);
        }
    }

    @Override
    public boolean hasAction() {
        return true;
    }

}
