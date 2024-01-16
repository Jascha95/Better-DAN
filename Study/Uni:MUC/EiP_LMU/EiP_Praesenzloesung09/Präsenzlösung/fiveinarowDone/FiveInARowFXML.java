/**
 * Diese Application startet das Spiel Five-In-A-Row,
 * allerdings wird zuvor eine mit dem Gluon SceneBuilder
 * erstellte Eingabemaske für die Spielfeldgröße geladen
 * und gestartet.
 *
 * @author jost
 * Created Do, 21.Dezember 2017
 */

import javafx.application.Application;
import javafx.event.ActionEvent;
import javafx.fxml.FXML;
import javafx.fxml.FXMLLoader;
import javafx.scene.Parent;
import javafx.scene.Scene;
import javafx.scene.control.Button;
import javafx.scene.control.Label;
import javafx.scene.control.TextField;
import javafx.stage.Stage;

public class FiveInARowFXML extends Application{

  public static final int MINIMAL = 6;
  private int width = 15;
  private int height = 7;
  private boolean valsOkay = false;
  private Stage stage;

  @FXML
  private Label widthLabel;
  @FXML
  private TextField heightInput;
  @FXML
  private Label heightLabel;
  @FXML
  private TextField widthInput;
  @FXML
  private Label errorLabel;
  @FXML
  private Button startButton;

  @Override
  public void start(Stage primaryStage) throws Exception {
    /* Reicht nicht:
        Parent root = FXMLLoader.load(getClass().getResource("startscreenSheet.fxml"));
       da wir den Controller explizit setzen möchten.
    */
    FXMLLoader loader = new FXMLLoader(getClass().getResource("startscreenSheet.fxml"));
    loader.setController(this);
    Parent root = loader.load();

    Scene scene = new Scene(root);
    this.stage = primaryStage;
    this.stage.setTitle("Five-In-A-Row");
    this.stage.setScene(scene);
    this.stage.show();
  }

  @FXML
  void startAction(ActionEvent event) {
    this.valsOkay = true;
    widthEntered(event);
    heightEntered(event);
    if (this.valsOkay) {
      // Start game
      FiveGame game = new FiveGame(this.width, this.height);
      FiveView view = new FiveView(game);
      Scene scene = new Scene(view);
      this.stage.setScene(scene);
      // Bemerkung: Event-Handler sollten klein sein, evtl. mit runLater ausführen (nicht behandelt)
    }
  }

  @FXML
  void widthEntered(ActionEvent event) {
    this.width = verifyInt(this.width, widthLabel, widthInput);
  }

  @FXML
  void heightEntered(ActionEvent event) {
    this.height = verifyInt(this.height, heightLabel, heightInput);
  }

  private int verifyInt(int old, Label label, TextField field) {
    try {
      old = Integer.parseInt(field.textProperty().get());
    } catch (NumberFormatException e) {
      errorLabel.textProperty().set(label.getText() + " muss eine ganze Zahl sein!");
      valsOkay = false;
    }
    if (old < MINIMAL) {
      errorLabel.textProperty().set(label.getText() + " muss größer als " + MINIMAL + " sein!");
      valsOkay = false;
    }
    return old;
  }

  public static void main(String[] args) {
    launch(args);
  }
}
