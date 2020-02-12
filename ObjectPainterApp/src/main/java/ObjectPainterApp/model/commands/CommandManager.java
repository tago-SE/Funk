package ObjectPainterApp.model.commands;

import java.util.LinkedList;
import java.util.List;
import java.util.Stack;

/**
 * Manages the states of commands and history by saving commands in a stack. The CommandManager allows the execution of
 * commands, performing undo, and redo.
 *
 * Reminder:
 * https://medium.com/better-programming/utilizing-the-command-pattern-to-support-undo-redo-and-history-of-operations-b28fa9d58910
 * https://github.com/iluwatar/java-design-patterns/tree/master/command
 */
public class CommandManager {

    private final Stack<ICommand> undoStack = new Stack<>();
    private final Stack<ICommand> redoStack = new Stack<>();

    public void execute(ICommand command) {
        command.doAction();
        undoStack.push(command);
        redoStack.clear();
    }

    public void execute(List<ICommand> commands) {
        for (ICommand command: commands)
            execute(command);
    }

    public void undo() {
        if (!undoStack.isEmpty())  {
            redoStack.push(undoStack.pop().undoAction());
        }
    }

    public void redo() {
        if (!redoStack.isEmpty()) {
            undoStack.push(redoStack.pop().doAction());
        }
    }

    public void clear() {
        undoStack.clear();
        redoStack.clear();
    }

}
