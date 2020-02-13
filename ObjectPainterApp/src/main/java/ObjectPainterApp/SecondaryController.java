package ObjectPainterApp;

import java.io.IOException;
import javafx.fxml.FXML;

public class SecondaryController implements IController {

    @FXML
    private void switchToPrimary() throws IOException {
        App.setRoot("primary");
    }

}