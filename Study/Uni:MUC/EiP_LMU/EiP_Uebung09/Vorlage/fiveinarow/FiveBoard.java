import java.util.ArrayList;
import java.util.Observable;

/**
 * Modell
 */
public class FiveBoard extends Observable {

    FiveCell[][] board;
    private final int max_x; //Breite
    private final int max_y; //Höhe

    public FiveBoard(int width, int height) {
        this.max_x = width;
        this.max_y = height;
        this.board = new FiveCell[max_x][max_y];
        for (int x=0; x< max_x; x++) {
            for (int y = 0; y < max_y; y++) {
                board[x][y] = new FiveCell(new Position(x,y));
            }
        }
    }

    /**
     * @return Alle Zellen des Spielfelds in beliebiger Reihenfolge
     */
    public ArrayList<FiveCell> getCells() {
        // TODO
        return null;
    }


    /**
     *
     * @param pos
     * @return Zelle an der gegebenen postion falls möglich, sonst null
     */
    public FiveCell getCell(Position pos) {
        if (pos.getX() >= 0 && pos.getX() < max_x && pos.getY() >= 0 && pos.getY() < max_y) {
            return board[pos.getX()][pos.getY()];
        } else {
            return null;
        }
    }

    /**
     * Prüft ob der letzte Zug an Position pos das Spiel bereits gewonnen hat.
     * @param pos
     * @return true, falls der Zug an Position pos das Spiel gewinnt.
     */
    public boolean isWinningMove(Position pos) {
        // TODO
        return  false;
    }

    /**
     *
     * @return true, falls es noch mindestens eine leere Spielposition gibt
     */
    public boolean hasEmptyCells() {
        // TODO
        return  false;
    }

}
