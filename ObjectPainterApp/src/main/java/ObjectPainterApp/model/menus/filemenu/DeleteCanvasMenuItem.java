package ObjectPainterApp.model.menus.filemenu;

import ObjectPainterApp.model.AppFacade;
import ObjectPainterApp.model.FileManagerSubject;
import ObjectPainterApp.model.menus.MenuComponent;
import ObjectPainterApp.model.shapes.Shape;
import javafx.scene.control.Alert;
import javafx.scene.control.ButtonType;

import java.util.Collection;
import java.util.Optional;

public class DeleteCanvasMenuItem extends MenuComponent {

    public DeleteCanvasMenuItem(String name) {
        super(name);
    }

    @Override
    public void onAction() {
        Alert alert = new Alert(Alert.AlertType.CONFIRMATION);
        alert.setTitle("Confirmation Dialog");
        alert.setHeaderText("You are about to delete the file: \'" + getName() + "\'.");
        alert.setContentText("Are you ok with this?");
        Optional<ButtonType> result = alert.showAndWait();
        if (result.get() == ButtonType.OK){
            FileManagerSubject.getInstance().deleteFile(getName());
        }
    }

    @Override
    public boolean hasAction() {
        return true;
    }
}
