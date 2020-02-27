package ObjectPainterApp.model.menus.filemenu;

import ObjectPainterApp.model.AppFacade;
import ObjectPainterApp.model.menus.MenuComponent;
import javafx.scene.control.Alert;
import javafx.scene.control.ButtonType;

import java.util.Optional;

public class NewCanvasMenuItem extends MenuComponent {

    public NewCanvasMenuItem(String name) {
        super(name);
    }

    @Override
    public void onAction() {
        AppFacade facade = AppFacade.getInstance();
        if (facade.getNumberOfLoggedCommands() > 0) {
            Alert alert = new Alert(Alert.AlertType.CONFIRMATION);
            alert.setTitle("Confirmation Dialog");
            alert.setHeaderText("A new canvas will be created, losing all previously done work.");
            alert.setContentText("Are you ok with this?");
            Optional<ButtonType> result = alert.showAndWait();
            if (result.get() == ButtonType.OK){
                facade.newCanvas();
            }
        } else {
            facade.newCanvas();
        }
    }

    @Override
    public boolean hasAction() {
        return true;
    }


}
