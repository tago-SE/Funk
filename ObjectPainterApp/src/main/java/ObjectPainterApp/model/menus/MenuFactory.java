package ObjectPainterApp.model.menus;

import ObjectPainterApp.model.FileManagerSubject;
import ObjectPainterApp.model.menus.filemenu.DeleteCanvasMenuItem;
import ObjectPainterApp.model.menus.filemenu.LoadCanvasMenuItem;
import ObjectPainterApp.model.menus.filemenu.NewCanvasMenuItem;
import ObjectPainterApp.model.menus.filemenu.SaveCanvasMenuItem;

/**
 * Concrete factory implementing the IMenuFactory
 */
public class MenuFactory implements IMenuFactory {

    private static IMenuFactory instance;

    public static IMenuFactory getInstance() {
        if (instance == null) {
            return instance = new MenuFactory();
        }
        return instance;
    }

    public MenuComponent getFileMenu() {
        MenuComposite fileMenu = new MenuComposite("File");
        fileMenu.addChild(new NewCanvasMenuItem("New"));
        fileMenu.addChild(new SaveCanvasMenuItem("Save"));
        MenuComponent loadMenu = new MenuComposite("Load");
        MenuComponent deleteMenu = new MenuComposite("Delete");
        fileMenu.addChild(loadMenu);
        fileMenu.addChild(deleteMenu);
        FileManagerSubject.getInstance().getSavedShapeFiles().forEach(filename -> {
            loadMenu.addChild(new LoadCanvasMenuItem(filename));
            deleteMenu.addChild(new DeleteCanvasMenuItem(filename));
        });
        return fileMenu;
    }

}
