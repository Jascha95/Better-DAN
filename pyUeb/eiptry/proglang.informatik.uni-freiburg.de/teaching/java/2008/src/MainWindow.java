package tetris;

import java.awt.event.KeyEvent;
import java.awt.event.KeyListener;
import javax.swing.JFrame;

public class MainWindow extends JFrame implements KeyListener {

	/**
	 * 
	 */
	private static final long serialVersionUID = -1971095875872532134L;

	// close with this funktion,
	// later we can check here
	// if we need to save a game,
	// update highsoure, ask user
	// if he would like to close the 
	// game, and so on.
	private void close() {
		System.exit(0);
	}
	
	MainWindow(String s) {
		super(s);
		this.setDefaultCloseOperation(JFrame.EXIT_ON_CLOSE);
	    
		// add the Field to ContentPane of the Frame
		Field f = new Field();
	    this.getContentPane().add(f);

	    // add the object as keyListener
	    this.addKeyListener(this);
	}

	public void keyPressed(KeyEvent e) {
		// TODO Auto-generated method stub
		if (e.getKeyChar() == 'q') {
			this.close();
		}
	}

	public void keyReleased(KeyEvent e) {
		// TODO Auto-generated method stub
		
	}

	public void keyTyped(KeyEvent e) {
		// TODO Auto-generated method stub
		
	}
	

	
}
