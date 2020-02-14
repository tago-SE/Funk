package ObjectPainterApp.model.shapes;

import javafx.scene.canvas.GraphicsContext;

public class OctagonShape extends Shape {

    private final int sides = 8;
    private double[] x = new double[sides];
    private double[] y = new double[sides];

    public OctagonShape() {
        super();  // required
    }

    private void getVertices() {
        if (startX == endX && startY == endY)
            return;
        double w = Math.abs(super.startX - super.endX);
        double h = Math.abs(super.startY - super.endY);
        double radius = Math.min(w, h);
        for (int i = 0; i < sides; i++) {
            x[i] = getCenterX() + radius*Math.cos(2*Math.PI*i/sides);
            y[i] = getCenterY() + radius*Math.sin(2*Math.PI*i/sides);
        }
    }

    @Override
    protected void drawShape(GraphicsContext gc) {
        if (startX == endX && startY == endY)
            return;
        getVertices();
        if (super.filled)
            gc.fillPolygon(x, y, sides);
        else
            gc.strokePolygon(x, y, sides);
    }

    @Override
    public double getLeftX() {
        double n = Integer.MAX_VALUE;
        for (int i = 0; i < sides; i++) {
            n = Math.min(n, x[i]);
        }
        return n;
    }

    @Override
    public double getRightX() {
        double n = Integer.MIN_VALUE;
        for (int i = 0; i < sides; i++) {
            n = Math.max(n, x[i]);
        }
        return n;
    }

    @Override
    public double getTopY() {
        double n = Integer.MAX_VALUE;
        for (int i = 0; i < sides; i++) {
            n = Math.min(n, y[i]);
        }
        return n;
    }

    @Override
    public double getBotY() {
        double n = Integer.MIN_VALUE;
        for (int i = 0; i < sides; i++) {
            n = Math.max(n, y[i]);
        }
        return n;
    }

}
