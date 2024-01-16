import java.util.Observable;

/**
 * Modell
 */
public class FiveGame extends Observable {

    public static final int height = 17;
    public static final int width  = 17;

    private FiveBoard board;
    private int activePlayer;
    private boolean gameOngoing;

    public FiveGame() {
        // TODO
    }


    public void moveAt(Position pos) {
        // TODO
    }

    public void reset() {
        // TODO
    }


     public void nextPlayer() {
        if (activePlayer == FiveCell.PLAYER_O) {
            activePlayer = FiveCell.PLAYER_X;
        } else {
            activePlayer = FiveCell.PLAYER_O;
        }
    }

    public FiveBoard getBoard() {
        return board;
    }

    public static int getWidth() {
        return width;
    }

    public static int getHeight() {
        return height;
    }

    public boolean isGameOngoing() {
        return gameOngoing;
    }

    public int getActivePlayer() {
        return activePlayer;
    }
}
