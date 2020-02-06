module ObjectPainterApp {
    requires javafx.controls;
    requires javafx.fxml;

    opens ObjectPainterApp to javafx.fxml;
    exports ObjectPainterApp;
}