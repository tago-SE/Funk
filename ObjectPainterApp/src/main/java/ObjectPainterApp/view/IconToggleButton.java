package ObjectPainterApp.view;

import javafx.scene.control.ToggleButton;
import javafx.scene.image.ImageView;

public class IconToggleButton extends ToggleButton {

    public IconToggleButton(String resource, String id, int size) {
        setMaxHeight(size);
        setMaxWidth(size);
        setMinHeight(size);
        setMinWidth(size);
        setId(id);
        if (resource != null && !resource.equals("")) {
            ImageView iv = new ImageView(resource);
            iv.setFitWidth(size);
            iv.setFitHeight(size);
            setGraphic(iv);
        }
    }
}
