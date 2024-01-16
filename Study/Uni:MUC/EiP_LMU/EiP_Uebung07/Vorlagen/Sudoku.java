public class Sudoku {

    private static final int MAX    = 9; // numbers 1..9, array dim 0..8
    private static final int SQUARE = 3; // size of sub-boards
    private static final int EMPTY  = 0; // special for empty field

    private int[][] board;  // entries: 0 = empty, 1..9

    // construct empty Sudoku board of size MAX
    public Sudoku() { board = new int[MAX][MAX]; }

    // parse initial set-up of Sudoku board
    // expects an array of rows (as Strings)
    public Sudoku (String[] arr) {

        // ******************** TODO ********************

    }
 
    // print board in human-readable form
    public String toString() {

        // ******************** TODO ********************

    }

    // checks if it is legal to put number in field (row,col)
    public boolean legalAt (int row, int col, int number) {

        // ******************** TODO ********************

    }

    // tries to fill position (row,col) with the unique matching number
    // returns true if successful
    public boolean putUniqueAt (int row, int col) {
        int number = EMPTY;
        for (int n = 1; n <= MAX; n++) 
            if (legalAt(row,col,n)) 
                if (number == EMPTY) number = n; // first match
                else return false;               // second match
        board[row][col] = number;
        return true;
    }

    // tries to fill with unique match at all positions
    // returns true if successful
    public boolean putUniques () {
        boolean result = false;
        for (int r = 0; r < MAX; r++)
            for (int c = 0; c < MAX; c++) 
                if (board[r][c] == EMPTY && putUniqueAt(r,c)) 
                    result = true;
        return result;
    }

    // repeats filling all uniquely determined positions 
    // as long as it finds one
    // returns true if it could fill at least one field
    public boolean repeatPutUniques() {
        boolean result = putUniques();
        if (result) while (putUniques()) {}
        return result;
    }

    // read initial board configuration from the first MAX command line 
    // arguments and fill all uniquely determined positions
    public static void main(String[] args) {
        System.out.println("Sudoku Assistant");
        Sudoku s = new Sudoku(args);
        System.out.println("Initial board");
        System.out.println(s.toString());
        if (s.repeatPutUniques()) {
            System.out.println("Filled board");
            System.out.println(s.toString());
        }
    }
}
