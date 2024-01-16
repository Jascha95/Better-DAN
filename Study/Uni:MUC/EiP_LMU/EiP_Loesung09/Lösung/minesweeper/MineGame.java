

import java.util.Observable;
import java.util.Random;

/**
 * Spielzustand
 */
public class MineGame extends Observable{


    private final int mines;

    private Minefield field;

    public MineGame() {
       this(30,20,99);
//       this(20,10,22);
//       this(10,5,10);
    }

    public MineGame(int width, int height, int mines) {
        this.mines  = mines;
        this.field = new Minefield(width,height);
        placeMines();
    }

    public void reset() {
        for (MineCell cell : this.field.getCells()) {
            cell.reset();
        }
        placeMines();
        setChanged();
        notifyObservers();
    }

    private void placeMines() {
        Random random = new Random();
        for (int i=0; i<mines; i++) {
            Position pos;
            do {
                pos = new Position(random.nextInt(field.getWidth()), random.nextInt(field.getHeight()));
                MineCell cell = field.getCell(pos);
                if (cell == null || cell.isDangerous()) pos = null;
            } while (pos == null);
            MineCell cell = field.getCell(pos);
            cell.arm();
            for (MineCell neighbor : field.getNachbarn(pos)) {
                neighbor.addArmedNeighbor();
            }
        }
    }


    public Minefield getBoard() {
        return field;
    }

    public void explore(Position pos) {
        restartPossibly();  // Button-Klick nach Spielende macht einen Reset und wird gleich ausgewertet.
        MineCell cell = field.getCell(pos);
        if (cell != null && cell.isHidden()) {
            cell.discover();
            if (cell.isDangerous()) {
                // Player lost
                for (MineCell hidden : field.getHiddenCells()) {
                    hidden.reveal();
                }
            } else if (cell.getNeighbor_mines() == 0) {
                // Keine Minen in Nachbarschaft, alle Nachbarn automatisch aufdecken
                for (MineCell neighbor : field.getNachbarn(pos)) {
                    this.explore(neighbor.getPosition());
                }
            }
            setChanged();
            notifyObservers();
        }
    }
    
    public void toggleMark(Position pos) {
        MineCell cell = field.getCell(pos);
        if (cell != null) {
            cell.toggleFlag();
            setChanged();
            notifyObservers();
        }
    }


    public void restartPossibly() {
        if (!getStatus().ongoing) reset();
    }

    public MinefieldStatus getStatus() {
        // Könnte man auch einmal berechnen und updaten,
        // das wäre effizienter, birgt aber auch die Gefahr,
        // das man dabei einen Fehler macht.
        int flags = 0;
        int hidden = 0;
        int mines = 0;
        boolean gameOngoing = true;
        for (MineCell cell : field.getCells()) {
            if (cell.isFlagged()) flags++;
            if (cell.isHidden()) hidden++;
            if (cell.isDangerous()) {
                mines++;
                gameOngoing = gameOngoing && cell.isHidden();
            }
        }
        gameOngoing = gameOngoing && (hidden != mines || flags != mines);
        return new MinefieldStatus(gameOngoing, flags, mines, hidden);
    }

}
