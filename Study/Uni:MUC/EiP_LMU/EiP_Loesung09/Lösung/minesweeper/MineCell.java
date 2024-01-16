

import java.util.Observable;

/**
 * Zelle, Mutable
 */
public class MineCell extends Observable {


    private final Position position;
    private int neighbor_mines;
    private boolean mine;
    private boolean hidden;
    private boolean pressed;
    private boolean flagged;

    public MineCell(Position position) {
        this.position = position;
        reset();
    }

    public void reset(){
        this.neighbor_mines = 0;
        this.mine    = false;
        this.hidden  = true;
        this.pressed = false;
        this.flagged = false;
        setChanged();
        notifyObservers();
    }

    public Position getPosition() {
        return position;
    }

    public boolean isDangerous() {
        return mine;
    }

    public void arm() {
        this.mine = true;
    }


    public void discover() {
        this.hidden  = false;
        this.pressed = true;
        this.flagged = false;
        setChanged();
        notifyObservers();
    }

    public void reveal() {
        this.hidden = false;
        setChanged();
        notifyObservers();
    }

    public void toggleFlag() {
        this.flagged = !this.flagged;
        setChanged();
        notifyObservers();
    }

    public int getNeighbor_mines() {
        return neighbor_mines;
    }

    public void addArmedNeighbor() {
        this.neighbor_mines++;
    }

    public boolean isHidden() {
        return hidden;
    }

    public boolean isPressed() {
        return pressed;
    }

    public boolean isFlagged() {
        return flagged;
    }

}
