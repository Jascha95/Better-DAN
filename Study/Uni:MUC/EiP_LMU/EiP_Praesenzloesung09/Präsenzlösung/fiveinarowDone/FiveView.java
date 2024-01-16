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
        for (FiveCell cell : game.getBoard().getCells()) {
            Position pos = cell.getPosition();
            Button btn = new Button(showPlayer(cell.getOwner()));
            btn.setMinWidth(scale);
            btn.setMinHeight(scale);
            btn.setMaxWidth(Double.MAX_VALUE);
            btn.setMaxHeight(Double.MAX_VALUE);
            btn.setOnAction(event -> {
                game.moveAt(pos);
            });
            cell.addObserver((o,arg) -> {
                btn.setText(showPlayer(cell.getOwner()));
            });
            boardView.add(btn, pos.getX(), pos.getY());
        }
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
        } else return "";
    }

    public static String showWinner(int zustand) {
        if (zustand == FiveCell.EMPTY)
            return "Kein Spieler";
        else
            return showPlayer(zustand);
    }

    public static String showStatus(boolean ongoing, int zustand) {
        if (ongoing) {
            return showPlayer(zustand) + " ist am Zug";
        } else {
            return "Spiel beendet. " + showPlayer(zustand) + " hat gewonnen!";
        }
    }

}
