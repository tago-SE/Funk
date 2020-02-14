package ObjectPainterApp.model.menus;

public class MenuComposite extends MenuComponent {

    public MenuComposite(String name) {
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
