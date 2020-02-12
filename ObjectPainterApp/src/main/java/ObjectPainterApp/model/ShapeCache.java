package ObjectPainterApp.model;

import ObjectPainterApp.model.shapes.Shape;
import ObjectPainterApp.utils.SubtypeCache;

import java.util.Collection;

/**
 * Singleton wrapper for the SubtypeCache
 */
public class ShapeCache {

    private static ShapeCache instance;
    private SubtypeCache<Shape> shapeCache;

    private ShapeCache() {
        shapeCache = new SubtypeCache<>(Shape.class, "ObjectPainterApp.model.shapes");
    }

    public static ShapeCache getInstance() {
        if (instance == null)
            return instance = new ShapeCache();
        return instance;
    }

    public Collection<String> getShapeTypes() {
        return shapeCache.getKeys();
    }

    public Collection<Shape> getShapePrototypes() {
        return shapeCache.getInstances();
    }

    public Shape getShape(String key) {
        return shapeCache.getInstance(key);
    }

    public Collection<Class> getShapeClasses() {
        return shapeCache.getClasses();
    }

    public Class getShapeClass(String key) {
        return shapeCache.getClass(key);
    }

}
