package ObjectPainterApp.view;

import ObjectPainterApp.model.shapes.factory.ShapeType;
import org.testng.annotations.Test;

public class ShapeImageLoaderTest {

    private ShapeImageLoader sImgLoader = new ShapeImageLoader();

    /**
     * Verifies that all the menu icons for the shapes exist
     */
    @Test
    public void getShapesResourceDirectory() {
        sImgLoader.getShapeImageResource(ShapeType.LINE);
        sImgLoader.getShapeImageResource(ShapeType.OCTAGON);
        sImgLoader.getShapeImageResource(ShapeType.RECTANGLE);
        sImgLoader.getShapeImageResource(ShapeType.OVAL);
    }
}