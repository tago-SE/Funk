package ObjectPainterApp.model.commands;

import java.util.LinkedList;
import java.util.List;

/**
 * Manages the states of commands and history by saving commands in a stack. The CommandManager allows the execution of
 * commands, performing undo, and redo.
 *
 * Reminder:
 * https://medium.com/better-programming/utilizing-the-command-pattern-to-support-undo-redo-and-history-of-operations-b28fa9d58910
 * https://github.com/iluwatar/java-design-patterns/tree/master/command
 */
public class CommandManager {


    private List<ICommand> stackForward = new LinkedList<>();
    private List<ICommand> stackReverse = new LinkedList<>();

    public void execute(ICommand command) {
        command.execute();
    }

    public void execute(List<ICommand> commands) {
        for (ICommand command: commands)
            execute(command);
    }

    public void undo() {

    }

    public void redo() {

    }

    public void clear() {

    }

}
