/**
 * Functional interface to reduce code redundancies in FiveBoard.
 *
 * Verwendungsbeispiel:
 *
 * Hat man eine Methode
 *   public void foo(int x, Direction dir) {
 *      ...
 *      Position newpos = dir.goFrom(oldpos);
 *      ...
 *   };
 *
 * kann man diese dann so aufrufen:
 *  foo(42, pos -> pos.up().up().right())
 *
 */

public interface Direction {
    Position goFrom(Position pos);
}
