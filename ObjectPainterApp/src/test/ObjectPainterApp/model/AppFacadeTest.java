package ObjectPainterApp.model;

import org.junit.Test;

import static org.junit.Assert.*;

public class AppFacadeTest {

    private AppFacade facade = AppFacade.getInstance();

    @Test
    public void getDrawableShapeTypes() {

        System.out.println(facade.getDrawableShapeTypes());

    }
}