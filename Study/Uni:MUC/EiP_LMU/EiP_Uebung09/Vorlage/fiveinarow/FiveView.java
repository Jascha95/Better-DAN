import javafx.scene.control.Button;
import javafx.scene.control.Label;
import javafx.scene.layout.GridPane;

/**
 * View
 */
public class FiveView extends GridPane {

    public static final int scale = 30;

    public FiveView(FiveGame game) {
        super();
        // Board
        GridPane boardView = new GridPane();
        // TODO
            //BEISPIEL ZUR VERWENDUNG VON BUTTON:
            Position pos = new Position(7,9);
            Button button = new Button();
            FiveCell cell = game.getBoard().getCell(pos);
            button.setText(showPlayer(cell.getOwner()));
            button.setOnAction(event -> { System.out.println("Button(7,9)"); });
            // Observer für cell einbauen?
            boardView.add(button,7,9);
        // TODO
        this.add(boardView,0,0);
        // Statuszeile
        Label status = new Label(showStatus(game.isGameOngoing(),game.getActivePlayer()));
        game.addObserver((o,arg) -> {
            status.setText(showStatus(game.isGameOngoing(),game.getActivePlayer()));
        });
        this.add(status,0,1);
    }

    public static String showPlayer(int zustand) {
        if (zustand == FiveCell.PLAYER_O) {
            return "O";
        } else if (zustand == FiveCell.PLAYER_X) {
            return "X";
        } else return " ";
    }

    public static String showWinner(int zustand) {
        if (zustand == FiveCell.EMPTY)
            return "Kein Spieler";
        else
            return showPlayer(zustand);
    }

    public static String showStatus(boolean ongoing, int zustand) {
        if (ongoing) {
            return showWinner(zustand) + " ist am Zug";
        } else {
            return "Spiel beendet. " + showWinner(zustand) + " hat gewonnen!";
        }
    }

}
