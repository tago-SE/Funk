package ObjectPainterApp.model.shapes.factory;


import ObjectPainterApp.model.shapes.*;

import java.util.ArrayList;
import java.util.List;

public class DrawableShapeFactory implements IShapeFactory {

    private List<Shape> shapes = new ArrayList<>();

    public DrawableShapeFactory() {

        shapes.add(new Line());
        shapes.add(new Rectangle());
        shapes.add(new Oval());
        shapes.add(new Octagon());
        shapes.add(new Hexagon());
    }

    private static DrawableShapeFactory instance;

    public static IShapeFactory getInstance() {
        if (instance == null) {
            return instance = new DrawableShapeFactory();
        }
        return instance;
    }

    @Override
    public Shape getShapePrototype(ShapeType type) {
        for (Shape s : shapes) {
            if (s.getType().equals(type)) {
                return s.clone();
            }
        }
        throw new IllegalArgumentException("Shape is not supported by this factory: " + type);
    }

    public List<ShapeType> types() {
        List<ShapeType> resultList = new ArrayList<>();
        for (Shape s : shapes) {
            resultList.add(s.getType());
        }
        return resultList;
    }

}
