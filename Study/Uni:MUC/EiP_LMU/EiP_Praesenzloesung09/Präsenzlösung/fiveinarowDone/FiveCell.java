import java.util.Observable;

/**
 * Mutable, denn sonst macht Oberservable keinen Sinn.
 *
 * Modell
 */
public class FiveCell extends Observable {

    /* Ein eigene ENUM Deklaration wäre hier besser,
       wurde aber noch nicht in der Vorlesung behandelt */
    public static final int EMPTY = 0;
    public static final int PLAYER_X = 1;
    public static final int PLAYER_O = 2;

    private final Position position;
    private       int owner;

    public FiveCell(Position position) {
        this(position, EMPTY);
    }

    public FiveCell(Position position, int owner) {
        this.position = position;
        this.owner = owner;
    }

    public Position getPosition() {
        return position;
    }

    public boolean isEmpty() {
        return owner == EMPTY;
    }

    public int getOwner() {
        return owner;
    }

    public boolean setOwner (int owner) {
        if (isEmpty()){
            this.owner = owner;
            setChanged();
            notifyObservers();
            return true;
        } else {
            return false;
        }
    }

    public void resetOwner () {
        this.owner = EMPTY;
        setChanged();
        notifyObservers();
    }

}
