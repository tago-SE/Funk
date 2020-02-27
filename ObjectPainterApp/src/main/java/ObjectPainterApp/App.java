package ObjectPainterApp;

import javafx.application.Application;
import javafx.fxml.FXMLLoader;
import javafx.scene.Parent;
import javafx.scene.Scene;
import javafx.stage.Stage;
import java.io.IOException;

/*
 * How to setup JavaFx, IntelliJ using Maven:
 * https://openjfx.io/openjfx-docs/#install-javafx
 */

/**
 * JavaFX App
 */
public class App extends Application {

    private static Scene scene;
    private static Stage firstStage;

    @Override
    public void start(Stage stage) throws IOException {
        firstStage = stage;
        scene = new Scene(loadFXML("primary"));
        stage.setScene(scene);
        stage.show();
    }

    static void setRoot(String fxml) throws IOException {
        scene.setRoot(loadFXML(fxml));
    }

    private static Parent loadFXML(String fxml) throws IOException {
        FXMLLoader fxmlLoader = new FXMLLoader(App.class.getResource(fxml + ".fxml"));
        Parent root = fxmlLoader.load();
        //  Controller injections can be done here
        //  IController controller = fxmlLoader.getController();
        //
        return root;
    }

    public static void main(String[] args) {
        launch();
    }

}