/**
 * Vorlesung "Einführung in die Programmierung"
 * LMU München, Lehrstuhl TCS
 * Wintersemester 2017/18
 *
 */

import javafx.application.Application;
import javafx.fxml.FXMLLoader;
import javafx.scene.Parent;
import javafx.scene.Scene;
import javafx.stage.Stage;

public class Minesweeper extends Application {

    @Override
    public void start(Stage primaryStage) throws Exception{
        MineGame game = new MineGame();
        MineView view = new MineView(game);
        Scene scene = new Scene(view);
        primaryStage.setTitle("Minesweeper");
        primaryStage.setScene(scene);
        primaryStage.show();
    }


    public static void main(String[] args) {
        launch(args);
    }

}
