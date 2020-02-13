package ObjectPainterApp.model.menus;

import java.util.Collection;

public class LogicalMenu extends MenuComponent {

    public LogicalMenu(String name) {
        super(name);
    }


    @Override
    public void onAction() {
        System.out.println("executed: " + getName());
    }

    @Override
    public boolean hasAction() {
        return false;
    }

}
