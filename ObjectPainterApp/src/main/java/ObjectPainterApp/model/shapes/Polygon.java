package ObjectPainterApp.model.shapes;

import javafx.scene.canvas.GraphicsContext;

public abstract class Polygon extends Shape {

    final int sides;
    double[] x;
    double[] y;

    protected Polygon(int sides) {
        super();
        this.sides = sides;
        this.x = new double[sides];
        this.y = new double[sides];
        this.x[0] = Integer.MAX_VALUE;
    }

    protected void getVertices() {
        double startX = getStartX();
        double endX = getEndX();
        double startY = getStartY();
        double endY = getEndY();
        if (startX == endX && startY == endY)
            return;
        double w = Math.abs(startX - endX);
        double h = Math.abs(startY - endY);
        double radius = Math.min(w, h);
        for (int i = 0; i < sides; i++) {
            x[i] = getCenterX() + radius*Math.cos(2*Math.PI*i/sides);
            y[i] = getCenterY() + radius*Math.sin(2*Math.PI*i/sides);
        }
    }

    @Override
    protected void drawShape(GraphicsContext gc) {
        double startX = getStartX();
        double endX = getEndX();
        double startY = getStartY();
        double endY = getEndY();
        if (startX == endX && startY == endY)
            return;
        getVertices();
        if (super.isFilled())
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

