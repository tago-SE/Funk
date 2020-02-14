package ObjectPainterApp.view;

import javafx.fxml.Initializable;

import java.util.logging.Logger;

public class OperationMenuButtonFactory {

    private static final Logger LOGGER = Logger.getLogger(OperationMenuButtonFactory.class.getName());

    private static final String IMAGE_FORMAT = ".png";
    private static final String SHAPES_RESOURCE_DIRECTORY = "images/";
    private static final String FAILED_TO_LOAD_RESOURCE_ERR = "Resource not found: %s.";

    private static OperationMenuButtonFactory instance;

    private OperationMenuButtonFactory() {}

    public static OperationMenuButtonFactory getInstance() {
        if (instance == null)
            return instance = new OperationMenuButtonFactory();
        return instance;
    }

    public IconToggleButton create(Initializable initializable, String operation, int size) {
        String resource = getIconResource(initializable, SHAPES_RESOURCE_DIRECTORY + operation.toLowerCase() + IMAGE_FORMAT);
        return new IconToggleButton(resource, operation, size);
    }

    private String getIconResource(Initializable initializable, String iconPath) {
        try {
            return initializable.getClass().getResource(iconPath).toExternalForm();
        } catch (NullPointerException ne) {
            LOGGER.warning(String.format(FAILED_TO_LOAD_RESOURCE_ERR, iconPath));
        }
        return "";
    }

}
