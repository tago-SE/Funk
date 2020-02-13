package ObjectPainterApp.model;

import ObjectPainterApp.model.shapes.Shape;
import ObjectPainterApp.utils.ISubject;

import java.util.Collection;

public interface IFileManager extends ISubject {

    boolean shapesFileExists(String filename);

    void saveShapesToFile(Collection<Shape> shapes, String filename);

    void deleteFile(String filename);

    Collection<Shape> loadShapesFromFile(String filename);

    Collection<String> getSavedShapeFiles();




}
