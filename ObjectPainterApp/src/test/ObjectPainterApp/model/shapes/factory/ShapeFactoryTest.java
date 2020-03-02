package ObjectPainterApp.model.shapes.factory;

import org.junit.Ignore;
import org.testng.annotations.Test;

import java.util.Collection;

import static ObjectPainterApp.model.shapes.factory.ShapeType.*;
import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertTrue;

public class ShapeFactoryTest {

    private IShapeFactory shapeFactory = new DrawableShapeFactory();

    @Ignore
    public void getShapePrototype() {

    }

    @Test
    public void types() {
        ShapeType[] expected = {LINE, RECTANGLE, OVAL, OCTAGON};
        Collection<ShapeType> types = shapeFactory.types();
        assertEquals(expected.length, types.size());
        for (ShapeType t : expected) {
            assertTrue(types.contains(t));
        }
    }


}