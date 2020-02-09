package ObjectPainterApp.model.commands;

import ObjectPainterApp.model.shapes.Shape;

import java.util.List;

public class DeleteShapesCommand implements ICommand {

    public DeleteShapesCommand(List<Shape> shapesToDelete) {

    }

    @Override
    public void execute() {

    }

    @Override
    public void undo() {

    }

    @Override
    public String getName() {
        return null;
    }

}
