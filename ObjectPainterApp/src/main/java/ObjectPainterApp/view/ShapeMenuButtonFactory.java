package ObjectPainterApp.view;

import javafx.fxml.Initializable;
import javafx.scene.control.ToggleButton;
import javafx.scene.image.ImageView;
import org.reflections.Reflections;
import org.reflections.scanners.SubTypesScanner;

import java.util.*;
import java.util.logging.Logger;

/**
 * Creates a list of ToggleButtons based on the ShapeNameTypes provided. The created buttons will load a matching image
 * found under images/shapes. If no image resource is found the button will be created without one. The created buttons
 * will also be given a id equal to the String ShapeNameType.
 */
public class ShapeMenuButtonFactory {

    private static final Logger LOGGER = Logger.getLogger(ShapeMenuButtonFactory.class.getName());

    private static final String SHAPES_RESOURCE_DIRECTORY = "images/shapes";
    private static final String IMAGE_FORMAT = ".png";
    private static final String FAILED_TO_LOAD_RESOURCE_ERR = "Resource not found: %s.";

    private static ShapeMenuButtonFactory instance = null;

    private Hashtable<String, String> shapeToResource = new Hashtable<>();

    private ShapeMenuButtonFactory() {}

    public static ShapeMenuButtonFactory getInstance() {
        if (instance == null)
            return instance = new ShapeMenuButtonFactory();
        return instance;
    }

    /**
     * Factory for creating Shape Menu Buttons that can be added to a Widget.
     *
     * @param initializable FXML Controller instance
     * @param shapeNames Collection of strings representing the available shapes.
     * @return List containing the ToggleIconButton class
     */
    public List<ToggleButton> createShapeMenuButtons(Initializable initializable, Collection<String> shapeNames, int size) {
        LOGGER.info("createShapeMenuButtons");
        List<ToggleButton> result = new ArrayList<>();
        for (String shapeName : shapeNames) {
            String shapeIconPath = getShapeIconPath(getShapeIconName(shapeName));
            String resource = shapeToResource.get(shapeName);
            if (resource == null) {
                try {
                    resource = getShapeIconResource(initializable, shapeIconPath);
                    shapeToResource.put(shapeName, resource);
                } catch (NullPointerException ne) {
                    LOGGER.warning(String.format(FAILED_TO_LOAD_RESOURCE_ERR, shapeIconPath));
                    resource = "";
                }
            }
            result.add(new IconToggleButton(resource, shapeName, size));
        }
        return result;
    }

    private String getShapeIconName(String shapeName) {
        return shapeName.toLowerCase() + IMAGE_FORMAT;
    }

    private String getShapeIconPath(String shapeIconName) {
        return SHAPES_RESOURCE_DIRECTORY + "/" + shapeIconName;
    }

    private String getShapeIconResource(Initializable initializable, String shapeIconPath) {
        return initializable.getClass().getResource(shapeIconPath).toExternalForm();
    }

}
