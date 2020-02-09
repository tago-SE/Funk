package ObjectPainterApp.view.shapes;

import ObjectPainterApp.utils.SubtypeCache;

import java.util.Collection;


/**
 * Adapter for the SubtypeCache
 */
public class ShapeDrawerCache {

    private static ShapeDrawerCache instance;
    private SubtypeCache<ShapeDrawer> shapeCache;

    private ShapeDrawerCache() {
        shapeCache = new SubtypeCache<>(ShapeDrawer.class, "ObjectPainterApp.view.shapes");
    }

    public static ShapeDrawerCache getInstance() {
        if (instance == null)
            return instance = new ShapeDrawerCache();
        return instance;
    }

    public Collection<String> getShapeTypes() {
        return shapeCache.getKeys();
    }

    public Collection<ShapeDrawer> getShapePrototypes() {
        return shapeCache.getInstances();
    }

    public ShapeDrawer getShape(String key) {
        return shapeCache.getInstance(key);
    }

    public Collection<Class> getShapeClasses() {
        return shapeCache.getClasses();
    }

    public Class getClass(String key) {
        return shapeCache.getClass(key);
    }

}
