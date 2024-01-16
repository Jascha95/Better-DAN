package intro.proglang.informatik.unifreiburg.de;

import java.awt.event.KeyEvent;
import java.awt.event.MouseEvent;

import javax.swing.JFrame;

/**
 * @author Phillip Heidegger
 *
 */
public final class World extends AWorld {

	public static void main(String args[]) {
		new World();
	}
	
	/** 
	 * This function is called by the World class to create your
	 * main window. 
	 */
	@Override
	protected void createAndShowGUI() {
	    this.frame = new JFrame("How to react on key events?");

        // Create the Field
		this.field = new Field();
		
	    // What happens when the frame closes?
	    // Because in the first version we do not use menus, 
	    // we would like to close our application, if the
	    // windows is closed.
	    frame.setDefaultCloseOperation(JFrame.EXIT_ON_CLOSE);
	    
	    // Add Panel to the frame, on this we will paint our game
	    // Look in the class Field to see how to paint on our
	    // window, and how to resize it
	    frame.add(this.field);

	    // Pack the Elements on the frame
	    frame.pack();

        // Show the window.
	    frame.setVisible(true);
    }

	@Override
	public void keyPressed(KeyEvent e) {
		if (e.getKeyCode() == KeyEvent.VK_DOWN) {
			System.out.println("down");
		}
		if (e.getKeyCode() == KeyEvent.VK_UP) {
			System.out.println("up");
		}
		if (e.getKeyCode() == KeyEvent.VK_LEFT) {
			System.out.println("left");
		}
		if (e.getKeyCode() == KeyEvent.VK_RIGHT) {
			System.out.println("right");
		}	
		if (e.getKeyCode() == KeyEvent.VK_K) {
			this.field.toggleCircle();
		}
		if (e.getKeyCode() == KeyEvent.VK_R) {
			this.field.toggleRectangle();
		}
		if (e.getKeyCode() == KeyEvent.VK_L) {
			this.field.toggleLine();
		}
		if (e.getKeyCode() == KeyEvent.VK_N) {
			this.field.clear();
		}
	}
    
    @Override
    public void mousePressed(MouseEvent e) {
        // TODO
    }

    @Override
    public void mouseReleased(MouseEvent e) {
        // TODO
    }

}
