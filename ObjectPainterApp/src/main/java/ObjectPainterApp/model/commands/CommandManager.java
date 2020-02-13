package ObjectPainterApp.model.commands;


import java.util.List;
import java.util.Stack;
import java.util.logging.Logger;

/**
 * Manages the states of commands and history by saving commands in a stack. The CommandManager allows the execution of
 * commands, performing undo, and redo.
 *
 */
public class CommandManager {

    private static final Logger LOGGER = Logger.getLogger(CommandManager.class.getName());
    private final Stack<ICommand> undoStack = new Stack<>();
    private final Stack<ICommand> redoStack = new Stack<>();

    public void execute(ICommand cmd) {
        cmd.doAction();
        if (cmd.hasUndo()) {
            undoStack.push(cmd);
            redoStack.clear();
        }
        LOGGER.info(cmd.getName());
    }

    public void execute(List<ICommand> commands) {
        for (ICommand command: commands)
            execute(command);
    }

    public void undo() {
        if (!undoStack.isEmpty())  {
            ICommand cmd = undoStack.pop();
            LOGGER.info(cmd.getName());
            redoStack.push(cmd.undoAction());
        }
    }

    public void redo() {
        if (!redoStack.isEmpty()) {
            ICommand cmd = redoStack.pop();
            LOGGER.info(cmd.getName());
            undoStack.push(cmd.doAction());
        }
    }

    public void clear() {
        undoStack.clear();
        redoStack.clear();
    }

}
