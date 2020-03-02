package ObjectPainterApp.view;

import ObjectPainterApp.model.shapes.factory.ShapeType;
import javafx.fxml.Initializable;
import javafx.scene.control.ToggleButton;

import java.util.ArrayList;
import java.util.Hashtable;
import java.util.List;
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
     * @param types Type of shapes which the buttons are based on.
     * @param buttonSize The size of the button
     * @return List containing the ToggleIconButton class
     */
    public List<ToggleButton> createShapeMenuButtons(Initializable initializable, int buttonSize, List<ShapeType> types) {
        LOGGER.info("createShapeMenuButtons");
        List<ToggleButton> result = new ArrayList<>();
        for (ShapeType type : types) {
            String resource = shapeToResource.get(type);
            if (resource == null) {
                try {
                resource = ShapeImageLoader.getInstance().getShapeImageResource(initializable, type);
                    shapeToResource.put(type.toString(), resource);
                } catch (NullPointerException ne) {
                    LOGGER.warning(String.format(FAILED_TO_LOAD_RESOURCE_ERR, type));
                    resource = "";
                }
            }
            String id = type.toString();
            result.add(new IconToggleButton(resource, id, buttonSize));
        }
        return result;
    }

    /*
    private String getShapeIconName(String shapeName) {
        return shapeName.toLowerCase() + IMAGE_FORMAT;
    }

    private String getShapeIconPath(String shapeIconName) {
        return SHAPES_RESOURCE_DIRECTORY + "/" + shapeIconName;
    }

    private String getShapeIconResource(Initializable initializable, String shapeIconPath) {
       return initializable.getClass().getResource(shapeIconPath).toExternalForm();
    }
    */

}
