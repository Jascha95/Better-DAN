

/**
 * Spielstatus, könnte auch in MineGame intergriert werden
 */
public class MinefieldStatus {

    public final boolean ongoing;
    public final int     flags;
    public final int     mines;
    public final int     remaining_fields;


    public MinefieldStatus(boolean ongoing, int flags, int mines, int remaining_fields) {
        this.ongoing = ongoing;
        this.flags = flags;
        this.mines = mines;
        this.remaining_fields = remaining_fields;
    }

    public boolean isOngoing() {
        return ongoing;
    }

    public int getRemaining_fields() {
        return remaining_fields;
    }

    public int getFlags() {
        return flags;
    }

    public int getMines() {
        return mines;
    }
}
