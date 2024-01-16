package tetris;

public class Tetris {
	
    /**
     * Create the GUI and show it.  For thread safety,
     * this method should be invoked from the
     * event-dispatching thread.
     */
    private static void createAndShowGUI() {
        //Create and set up the window.
    	MainWindow frame = new MainWindow("Hallo Welt!");
        
        //Display the window.
        frame.pack();
        frame.setVisible(true);
    }

	
	/**
	 * @param args
	 */
	public static void main(String[] args) {
		//System.out.println("Hallo Welt!");
	    
		//Schedule a job for the event-dispatching thread:
        //creating and showing this application's GUI.
        javax.swing.SwingUtilities.invokeLater(new Runnable() {
            public void run() {
                createAndShowGUI();
            }
        });
	}
}
