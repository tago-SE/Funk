package ObjectPainterApp.model.commands;

public interface ICommand {

    /**
     * Called when the command needs to perform a required operation.
     */
    ICommand doAction();

    /**
     * Called when the command gets undone.
     */
    ICommand undoAction();


    String getName();

}
