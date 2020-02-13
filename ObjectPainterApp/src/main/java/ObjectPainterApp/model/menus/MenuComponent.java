package ObjectPainterApp.model.menus;

import java.util.ArrayList;
import java.util.Collection;
import java.util.List;

public abstract class MenuComponent {

    private List<MenuComponent> children;
    private String name;

    public MenuComponent(String name) {
        this.name = name;
    }

    public void addChild(MenuComponent child) {
        if (child == null)
            throw new NullPointerException();
        if (children == null) {
            children = new ArrayList<>();
        }
        children.add(child);
    }

    public void removeChild(MenuComponent child) {
        if (children != null) {
            children.remove(child);
        } else {
            throw new NullPointerException();
        }
    }

    public Collection<MenuComponent> getChildren() {
        if (children == null)
            children = new ArrayList<>();
        return children;
    }

    public void setChildren(List<MenuComponent> children) {
        this.children = children;
    }

    public String getName() {
        return name;
    }

    public void setName(String name) {
        this.name = name;
    }

    public abstract void onAction();

    public abstract boolean hasAction();

}
