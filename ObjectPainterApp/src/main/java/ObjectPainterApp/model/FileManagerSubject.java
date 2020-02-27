package ObjectPainterApp.model;

import ObjectPainterApp.model.shapes.Shape;
import ObjectPainterApp.utils.IObserver;

import java.io.*;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.Paths;
import java.util.ArrayList;
import java.util.Collection;
import java.util.List;
import java.util.logging.Logger;
import java.util.stream.Collectors;
import java.util.stream.Stream;

public class FileManagerSubject implements IFileManager {

    private static final Logger LOGGER = Logger.getLogger(FileManagerSubject.class.getName());

    private List<IObserver> observerList;
    private static FileManagerSubject instance;

    private String saveDirectoryPath;

    public static IFileManager getInstance() {
        if (instance == null) {
            return instance = new FileManagerSubject();
        }
        return instance;
    }

    private FileManagerSubject() {
        try {
            saveDirectoryPath = new File(".").getCanonicalPath() + "/saves/";
            initializeShapeDirectory();
        } catch (IOException e) {
            e.printStackTrace();
        }
    }

    private void initializeShapeDirectory() {
        File directory = new File(saveDirectoryPath);
        if (!directory.exists()) {
            directory.mkdir();
        }
    }

    @Override
    public void saveShapesToFile(Collection<Shape> shapes, String filename) {
        FileOutputStream fos = null;
        ObjectOutputStream oos = null;
        try {
            initializeShapeDirectory();
            fos = new FileOutputStream(saveDirectoryPath + filename);
            oos = new ObjectOutputStream(fos);
            oos.writeObject(shapes);
            LOGGER.info(String.format("Saved (%d): %s", shapes.size(), shapes));

            // Alerts the observer that the files managed have changed

            notifyObservers();
        } catch (Exception ex) {
            ex.printStackTrace();
        } finally {
            if (oos != null ) try {
                oos.flush();
                oos.close();
            } catch (IOException e) {
                e.printStackTrace();
            }
            if (fos != null) try {
                fos.close();
            } catch (IOException e) {
                e.printStackTrace();
            }
        }
    }

    @Override
    public void deleteFile(String filename) {
        File file = new File(saveDirectoryPath + "/" + filename);
        if (file.exists()) {
            file.delete();
            notifyObservers();
        }
    }

    @Override
    public Collection<Shape> loadShapesFromFile(String filename) {
        FileInputStream fis = null;
        ObjectInputStream ois = null;
        try {
            fis = new FileInputStream(saveDirectoryPath + filename);
            ois = new ObjectInputStream(fis);
            List<Shape> shapes = (List<Shape>) ois.readObject();
            LOGGER.info(String.format("Loaded (%d): %s", shapes.size(), shapes));
            return shapes;
        } catch (Exception ex) {
            ex.printStackTrace();
        } finally {
            if (ois != null ) try {
                ois.close();
            } catch (IOException e) {
                e.printStackTrace();
            }
            if (fis != null) try {
                fis.close();
            } catch (IOException e) {
                e.printStackTrace();
            }
        }
        return new ArrayList<>();
    }

    @Override
    public Collection<String> getSavedShapeFiles() {
        final List<String> savedFiles = new ArrayList<>();
        try (Stream<Path> walk = Files.walk(Paths.get(saveDirectoryPath))) {
            walk.filter(Files::isRegularFile).map(Path::toString).collect(Collectors.toList()).forEach(pathname -> {
                savedFiles.add(new File(pathname).getName());
            });
        } catch (IOException e) {
            e.printStackTrace();
        }
        return savedFiles;
    }

    @Override
    public boolean shapesFileExists(String filename) {
        return new File(saveDirectoryPath + filename).exists();
    }

    @Override
    public void addObserver(IObserver observer) {
        if (observerList == null)
            observerList = new ArrayList<>();
        else if (observerList.contains(observer))
            return;
        observerList.add(observer);
    }

    @Override
    public void removeObserver(IObserver observer) {
        if (observerList == null)
            return;
        observerList.remove(observer);
    }

    @Override
    public void notifyObservers() {
        for (IObserver o : observerList)
            o.onChange(this);
    }
}
