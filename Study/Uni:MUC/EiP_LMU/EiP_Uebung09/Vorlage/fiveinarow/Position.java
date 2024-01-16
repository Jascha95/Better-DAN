/**
 * Modell
 *
 * Ähnlich zu gleichlautender Klasse aus Game of Life Beispiel
 *
 * Immutable
 *
 */
public class Position {

    private final int x;
    private final int y;

    public Position(int x, int y) {
        this.x = x;
        this.y = y;
    }

    public int getX() {
        return this.x;
    }

    public int getY() {
        return this.y;
    }

    public Position up() {
       return new Position(x,y-1);
    }

    public Position down() {
        return new Position(x,y+1);
    }

    public Position left() {
        return new Position(x-1,y);
    }

    public Position right() { return new Position(x+1,y); }

    @Override
    public boolean equals(Object o) {
        if (this == o) return true;
        if (o == null || getClass() != o.getClass()) return false;

        Position position = (Position) o;

        if (x != position.x) return false;
        return y == position.y;
    }

    @Override
    public int hashCode() {
        int result = x;
        result = 31 * result + y;
        return result;
    }
}
