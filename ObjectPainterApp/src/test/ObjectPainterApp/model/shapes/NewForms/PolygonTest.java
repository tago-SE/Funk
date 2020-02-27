package ObjectPainterApp.model.shapes.NewForms;

import org.junit.After;
import org.junit.Before;
import org.junit.Ignore;
import org.junit.Test;

import static org.testng.Assert.*;

@Deprecated
public class PolygonTest {

    @Before
    public void setUp() throws Exception {
    }

    @After
    public void tearDown() throws Exception {
        System.out.println("Hello!!");
        assertEquals(1, 1);
    }

    // Exception test
    @Test (expected = IllegalStateException.class)
    public void getVerticesFail() {
        throw new IllegalStateException();
    }

    // Exception test
    @Ignore ("Exception throwing not yet defined")
    @Test (expected = IllegalStateException.class)
    public void getVerticesFail2() {

    }

    @Test
    public void getVertices2() {
        System.out.println("Hello");
    }


    @Test
    public void getColor() {
    }

    @Test
    public void setColor() {
    }

    @Test
    public void isFilled() {
    }

    @Test
    public void setFilled() {
    }

    @Test
    public void getLineWidth() {
    }

    @Test
    public void setLineWidth() {
    }

    @Test
    public void getCenter() {
    }

    @Test
    public void setCenter() {
    }

    @Test
    public void getVertices() {
    }
}