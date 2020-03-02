package ObjectPainterApp.model.shapes;

import java.io.Serializable;

/**
 * Holds the current state of a shape, including color, position if its has filled color, and so on.
 *
 *
 *  The set methods allow for chaining, e.g: this.setColor(color).setWidth(5)...
 */
public class ShapeState implements IShapeMemento, Serializable, Cloneable {

    public String color;
    public boolean filled;
    public int lineWidth;
    public double startX, startY, endX, endY;
    public int lineDashes;

    public String getColor() {
        return color;
    }

    public ShapeState setColor(String color) {
        this.color = color;
        return this;
    }

    public boolean isFilled() {
        return filled;
    }

    public ShapeState setFilled(boolean filled) {
        this.filled = filled;
        return this;
    }

    public int getLineWidth() {
        return lineWidth;
    }

    public ShapeState setLineWidth(int lineWidth) {
        this.lineWidth = lineWidth;
        return this;
    }

    public double getStartX() {
        return startX;
    }

    public ShapeState setStartX(double startX) {
        this.startX = startX;
        return this;
    }

    public double getStartY() {
        return startY;
    }

    public ShapeState setStartY(double startY) {
        this.startY = startY;
        return this;
    }

    public double getEndX() {
        return endX;
    }

    public ShapeState setEndX(double endX) {
        this.endX = endX;
        return this;
    }

    public double getEndY() {
        return endY;
    }

    public ShapeState setEndY(double endY) {
        this.endY = endY;
        return this;
    }

    public int getLineDashes() {
        return lineDashes;
    }

    public ShapeState setLineDashes(int lineDashes) {
        this.lineDashes = lineDashes;
        return this;
    }

    @Override
    public ShapeState clone() {
        try {
            return (ShapeState) super.clone();
        } catch (CloneNotSupportedException e) {
            e.printStackTrace();
            return null;
        }
    }

    @Override
    public String toString() {
        return "ShapeState{" +
                "color='" + color + '\'' +
                ", filled=" + filled +
                ", lineWidth=" + lineWidth +
                ", startX=" + startX +
                ", startY=" + startY +
                ", endX=" + endX +
                ", endY=" + endY +
                ", lineDashes=" + lineDashes +
                '}';
    }
}
