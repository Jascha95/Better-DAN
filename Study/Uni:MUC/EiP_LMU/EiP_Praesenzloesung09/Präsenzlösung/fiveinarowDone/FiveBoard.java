import java.util.ArrayList;

/**
 * Modell
 */
public class FiveBoard {

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

    public ArrayList<FiveCell> getCells() {
        ArrayList<FiveCell> result = new ArrayList<>(max_x*max_y);
        for (int x=0; x< max_x; x++) {
            for (int y = 0; y < max_y; y++) {
                result.add(board[x][y]);
            }
        }
        return  result;
    }


    public FiveCell getCell(Position pos) {
        if (pos.getX() >= 0 && pos.getX() < max_x && pos.getY() >= 0 && pos.getY() < max_y) {
            return board[pos.getX()][pos.getY()];
        } else {
            return null;
        }
    }

    public boolean isWinningMove(Position pos) {
        FiveCell actcell = getCell(pos);
        int player       = actcell.getOwner();

        FiveCell check;
        // Check Row
        Position minPos = dominion(player,pos, p -> p.left());
        Position maxPos = dominion(player,pos, p -> p.right());
        if (maxPos.getX()-minPos.getX() > 5) return true;
        // Check Column
        minPos = dominion(player,pos, p -> p.up());
        maxPos = dominion(player,pos, p -> p.down());
        if (maxPos.getY()-minPos.getY() > 5) return true;
        // Check Diag1
        minPos = dominion(player,pos, p -> p.up().left());
        maxPos = dominion(player,pos, p -> p.down().right());
        if (maxPos.getX()-minPos.getX() > 5) return true;
        // Check Diag2
        minPos = dominion(player,pos, p -> p.down().left());
        maxPos = dominion(player,pos, p -> p.up().right());
        if (maxPos.getX()-minPos.getX() > 5) return true;
        // No more possibilites
        return  false;
    }

    public Position dominion(int player, Position pos, Direction dir) {
        Position curPos = pos;
        FiveCell check;
        do {
            curPos = dir.goFrom(curPos);
            check  = getCell(curPos);
        } while (check != null && check.getOwner() == player);
        return curPos;
    }

    public boolean hasEmptyCells() {
      for (int x=0; x < max_x; x++) {
          for (int y = 0; y < max_y; y++) {
              if (board[x][y].isEmpty()) return true;
          }
      }
      return false;
    }

}
