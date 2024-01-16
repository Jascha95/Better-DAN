import java.util.Observable;

/**
 *  Modell
 */
public class FiveGame extends Observable {

    public int height;
    public int width;

    private FiveBoard board;
    private int activePlayer = FiveCell.EMPTY;
    private boolean gameOngoing = false;

    public FiveGame() {
        this(15,7);
    }

    public FiveGame(int width, int height) {
        this.height = height;
        this.width  = width;
        this.board  = new FiveBoard(width,height);
        reset();
    }

    public void reset() {
        for (FiveCell cell : this.board.getCells()){
            cell.resetOwner();
        }
        this.gameOngoing = true;
        this.activePlayer = FiveCell.PLAYER_X;
        setChanged();
        notifyObservers();
    }


    public void moveAt(Position pos) {
        if (gameOngoing) {
            if (board.getCell(pos).setOwner(activePlayer)) {
                if (board.isWinningMove(pos)) {
                    gameOngoing = false;
                } else {
                    if (board.hasEmptyCells()) {
                        nextPlayer();
                    } else {
                        activePlayer = FiveCell.EMPTY;
                        gameOngoing = false;
                    }
                }
                setChanged();
                notifyObservers();
            }
        } else {
            reset();
        }

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

    public int getWidth() {
        return width;
    }

    public int getHeight() {
        return height;
    }

    public boolean isGameOngoing() {
        return gameOngoing;
    }

    public int getActivePlayer() {
        return activePlayer;
    }
}
