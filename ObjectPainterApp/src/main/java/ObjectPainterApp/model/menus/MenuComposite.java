package ObjectPainterApp.model.menus;

public class MenuComposite extends MenuComponent {

    public MenuComposite(String name) {
        super(name);
    }

    @Override
    public void onAction() {
        throw new IllegalStateException("Not implemented");
    }

    @Override
    public boolean hasAction() {
        return false;
    }

}
