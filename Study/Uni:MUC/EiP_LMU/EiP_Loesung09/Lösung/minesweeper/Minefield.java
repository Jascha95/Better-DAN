

import java.util.ArrayList;

/**
 * Spielfeld, mutable
 */
public class Minefield {
    

    private final int max_x; //Breite
    private final int max_y; //Höhe

    MineCell[][] board;

    public Minefield(int width, int height) {
        this.max_x = width;
        this.max_y = height;
        this.board = new MineCell[max_x][max_y];
        for (int x=0; x< max_x; x++) {
            for (int y = 0; y < max_y; y++) {
                board[x][y] = new MineCell(new Position(x,y));
            }
        }
    }

    public ArrayList<MineCell> getCells() {
        ArrayList<MineCell> result = new ArrayList<>(max_x*max_y);
        for (int x=0; x< max_x; x++) {
            for (int y = 0; y < max_y; y++) {
                result.add(board[x][y]);
            }
        }
        return  result;
    }


    public MineCell getCell(Position pos) {
        if (pos.getX() >= 0 && pos.getX() < max_x && pos.getY() >= 0 && pos.getY() < max_y) {
            return board[pos.getX()][pos.getY()];
        } else {
            return null;
        }
    }


    public ArrayList<MineCell> getHiddenCells() {
        ArrayList<MineCell> result = new ArrayList<>();
        for (int x=0; x < max_x; x++) {
            for (int y = 0; y < max_y; y++) {
              if (board[x][y].isHidden()) result.add(board[x][y]);
            }
        }
        return result;
    }

    /**
     * Berechnet alle Nachbarn einer Zelle.
     *
     * @param pos dessen Nachbarn auf dem Spielfeld berechnet werden sollen
     * @return alle Nachbarn von {@code pos}, Anzahl Nachbarn variabel, pos selbst nicht enthalten
     */
    public ArrayList<MineCell> getNachbarn(Position pos) {
        ArrayList<MineCell> result = new ArrayList<>();
        for (int x=-1; x<=1; x++) {
            for (int y = -1; y <= 1; y++) {
                if (x!=0 || y!=0) {
                    MineCell nachbar = this.getCell(new Position(pos.getX() + x, pos.getY() + y));
                    if (nachbar != null) {
                        result.add(nachbar);
                    }
                }
            }
        }
        return result;
    }


    public int getWidth() {
        return max_x;
    }

    public int getHeight() {
        return max_y;
    }
}
