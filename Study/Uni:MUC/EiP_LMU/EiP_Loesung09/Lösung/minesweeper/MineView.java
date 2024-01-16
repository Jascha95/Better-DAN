

import javafx.scene.Node;
import javafx.scene.Parent;
import javafx.scene.control.Button;
import javafx.scene.control.Label;
import javafx.scene.input.KeyCode;
import javafx.scene.layout.GridPane;

/**
 * View
 */
public class MineView extends GridPane {

    public static final int scale = 30;

    public MineView(MineGame game) {
        super();
        // Board
        GridPane boardView = new GridPane();
        for (MineCell cell : game.getBoard().getCells()) {
            Position pos = cell.getPosition();
            Button btn = new Button();
            btn.setMinWidth(scale);
            btn.setMinHeight(scale);
            btn.setMaxWidth(Double.MAX_VALUE);
            btn.setMaxHeight(Double.MAX_VALUE);
            cell.addObserver((o,arg) -> {
                btn.setText(fieldContent(cell));
                btn.setDisable(cell.isPressed());
            });
            btn.setOnMousePressed(event -> {
                if (event.isPrimaryButtonDown()) {
                    game.explore(pos);
                }
                if (event.isSecondaryButtonDown()) {
                    game.toggleMark(pos);
                }
            });
            // React to Keyboard input as well (war nicht gefragt)
            btn.setOnKeyPressed(event -> {
                if (event.getCode() == KeyCode.ENTER)
                    game.explore(pos);
                if (event.getCode() == KeyCode.SPACE)
                    game.toggleMark(pos);
            });
            boardView.add(btn, pos.getX(), pos.getY());
        }
        this.add(boardView,0,0);
        // Statuszeile
        Label status = new Label(showStatus(game.getStatus()));
        game.addObserver((o,arg) -> {
            MinefieldStatus gamestatus = game.getStatus();
            status.setText(showStatus(gamestatus));
        });
        this.add(status,0,1);
        // Click anywhere after gameend restarts as well
        this.setOnMousePressed(event -> {
            game.restartPossibly();
        });
    }


    public static String showStatus(MinefieldStatus status) {
        if (status.ongoing) {
            int verbleibend = status.getMines() - status.getFlags();
            return "Noch " + verbleibend + " von "
                           + status.getMines() + " Minen in " + status.getRemaining_fields()
                           + " Feldern zu finden.";
        } else {
            if (status.getRemaining_fields() == status.getMines()) {
                return "Alle Minen erfolgreich gefunden! (Spiel beendet.)";
            } else {
                return "Badaboom! (Spiel verloren.)";
            }
        }
    }

    public static String fieldContent(MineCell cell) {
        if (cell.isFlagged()) {
            if (cell.isHidden() || cell.isDangerous())
                 return "\u2691"; // Schwarze Flagge (korrekte Fahne oder noch unbekanntes Feld)
            else return "\u2690"; // Weisse   Flagge (falsche Fahne gesetzt)
        } else {
            if (!cell.isHidden()) {
                if (cell.isDangerous()) return "*";
                int nms = cell.getNeighbor_mines();
                if (nms > 0) return ""+nms;
            }
        }
        return " ";
    }

}

