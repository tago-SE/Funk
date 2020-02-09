package ObjectPainterApp.model.commands;

public interface ICommand {

    /**
     * Called when the command needs to perform a required operation.
     */
    void execute();

    /**
     * Called when the command gets undone.
     */
    void undo();


    /**
     * Will be used to
     * @return
     */
    String getName();

}
