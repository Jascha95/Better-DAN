import javafx.application.Application;
import javafx.scene.Scene;
import javafx.scene.control.TextInputDialog;
import javafx.stage.Stage;

import java.util.Optional;

/**
 * Controller, Hauptklasse mit fester Spielfeldgröße
 *
 * Für eine Variante mit Eingabe der Spielfeldgröße
 * bitte stattdessen die Klasse FiveInARowFXML verwenden.
 */
public class FiveInARow extends Application  {

    @Override
    public void start(Stage primaryStage) throws Exception{
        FiveGame game = new FiveGame();
        FiveView view = new FiveView(game);
        Scene scene = new Scene(view);
        primaryStage.setTitle("Five-In-A-Row");
        primaryStage.setScene(scene);
        primaryStage.show();
    }


    public static void main(String[] args) {
        launch(args);
    }

}
