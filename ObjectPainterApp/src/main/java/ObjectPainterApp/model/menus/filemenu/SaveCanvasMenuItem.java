package ObjectPainterApp.model.menus.filemenu;

import ObjectPainterApp.model.AppFacade;
import ObjectPainterApp.model.FileManagerSubject;
import ObjectPainterApp.model.IFileManager;
import ObjectPainterApp.model.menus.MenuComponent;
import javafx.scene.control.Alert;
import javafx.scene.control.ButtonType;
import javafx.scene.control.TextInputDialog;

import java.util.Optional;

public class SaveCanvasMenuItem extends MenuComponent {

    public SaveCanvasMenuItem(String name) {
        super(name);
    }

    @Override
    public void onAction() {
        final AppFacade facade = AppFacade.getInstance();
        TextInputDialog dialog = new TextInputDialog("");
        dialog.setTitle("Text Input Dialog");
        dialog.setHeaderText("Save Canvas");
        dialog.setContentText("Please enter a filename:");
        Optional<String> result = dialog.showAndWait();
        result.ifPresent(filename -> {
            if (filename.length() < 1) {
                Alert alert = new Alert(Alert.AlertType.ERROR);
                alert.setTitle("Error");
                alert.setHeaderText(null);
                alert.setContentText("No filename specified!");
                alert.showAndWait();
                return;
            }
            IFileManager fileManager =  FileManagerSubject.getInstance();
            if (fileManager.shapesFileExists(filename)) {
                Alert alert = new Alert(Alert.AlertType.CONFIRMATION);
                alert.setTitle("Confirmation Dialog");
                alert.setHeaderText(String.format("The file %s already exists and will be overwritten.", filename));
                alert.setContentText("Are you ok with this?");
                Optional<ButtonType> alertResult = alert.showAndWait();
                if (alertResult.get() == ButtonType.OK){
                    fileManager.saveShapesToFile(facade.getCanvasShapes(), filename);
                }
            } else {
                fileManager.saveShapesToFile(facade.getCanvasShapes(), filename);
            }
        });
    }

    @Override
    public boolean hasAction() {
        return true;
    }
}
