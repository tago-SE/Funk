module ObjectPainterApp {
    requires javafx.controls;
    requires javafx.fxml;
    requires java.logging;
    requires reflections;

    opens ObjectPainterApp to javafx.fxml;
    exports ObjectPainterApp;
}