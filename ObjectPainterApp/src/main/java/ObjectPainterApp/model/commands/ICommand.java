package ObjectPainterApp.model.commands;

public interface ICommand {

    /**
     *  Executes the command.
     * @return Command instance
     */
    ICommand doAction();

    /**
     * Performs the reverse of a do action.
     * @return Command instance
     */
    ICommand undoAction();

    /**
     * Returns a string containing the command name, mainly for logging purposes.
     * @return The command name
     */
    String getName();

}
