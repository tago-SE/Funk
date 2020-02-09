package ObjectPainterApp.utils;

import ObjectPainterApp.model.shapes.Shape;
import org.reflections.Reflections;
import org.reflections.scanners.SubTypesScanner;

import java.util.Collection;
import java.util.Hashtable;
import java.util.Set;
import java.util.logging.Logger;

/**
 * This caches information found belonging to all Sub Types of a Super Class. Uses reflection to scan a specified
 * package location for the subtypes at run time.
 *
 * Preconditions:
 * 1) The subtype class must have an empty constructor
 * 2) The subtype must have similar naming convention Shape (Super) and SquareShape (Sub Type).
 */
public class SubtypeCache<SuperClass> {

    private static final Logger LOGGER = Logger.getLogger(SubtypeCache.class.getName());
    private Hashtable<String, SuperClass> instanceMap;
    private Hashtable<String, Class> classMap;

    private final Class subClass;
    private final String packageLocation;

    public SubtypeCache(Class subClass, String packageLocation) {
        this.subClass = subClass;
        this.packageLocation = packageLocation;
        loadCache();
    }

    public Collection<String> getKeys() {
        return instanceMap.keySet();
    }

    public Collection<SuperClass> getInstances() {
        return instanceMap.values();
    }

    public SuperClass getInstance(String key) {
        return instanceMap.get(key);
    }

    public Collection<Class> getClasses() {
        return classMap.values();
    }

    public Class getClass(String key) {
        return classMap.get(key);
    }

    private void loadCache() {
        Reflections reflections = new Reflections(packageLocation, new SubTypesScanner());
        Set<Class<? extends Shape>> subTypes = reflections.getSubTypesOf(subClass);
        instanceMap = new Hashtable<>(subTypes.size());
        classMap = new Hashtable<>(subTypes.size());
        for (Class c : subTypes) {
            String name = c.getSimpleName().replace(subClass.getSimpleName(), "");
            try {
                SuperClass subPrototype = (SuperClass) c.getDeclaredConstructor().newInstance();
                instanceMap.put(name, subPrototype);
                classMap.put(name, c);
            } catch (Exception e) {
                LOGGER.severe("Failed to load SubType: " + name);
                e.printStackTrace();
            }
        }
    }

}
