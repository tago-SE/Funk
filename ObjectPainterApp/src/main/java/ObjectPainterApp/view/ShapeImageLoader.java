package ObjectPainterApp.view;

import ObjectPainterApp.model.shapes.factory.ShapeType;
import javafx.fxml.Initializable;

import java.io.File;

public class ShapeImageLoader {

    private static final String APP = "ObjectPainterApp";
    private static final String DIRECTORY = "images/shapes/";
    private static final String FILE_FORMAT = ".png";

    private static ShapeImageLoader instance = null;

    public static ShapeImageLoader getInstance() {
        if (instance == null)
            instance = new ShapeImageLoader();
        return instance;
    }

    public String getShapeImageResource(ShapeType type) {
        String path = APP + "/" + DIRECTORY + type.toString().toLowerCase() + FILE_FORMAT;
        try {
            File f = new File(getClass().getClassLoader().getResource(path).getFile());
            if (!f.exists()) {
                throw new IllegalArgumentException("File does not exists: " + f);
            }
            return f.getAbsolutePath();
        } catch (NullPointerException nue) {
            throw new IllegalArgumentException("File was not found: " + path);
        }
    }

    public String getShapeImageResource(Initializable initializable, ShapeType type) {
        String path = DIRECTORY + type.toString().toLowerCase() + FILE_FORMAT;
        return initializable.getClass().getResource(path).toExternalForm();
    }
}
