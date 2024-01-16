package intro.proglang.informatik.unifreiburg.de;

import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.awt.event.KeyEvent;
import java.awt.event.KeyListener;
import java.awt.event.MouseEvent;
import java.awt.event.MouseListener;

import javax.swing.JFrame;
import javax.swing.SwingUtilities;
import javax.swing.Timer;

public abstract class AWorld implements ActionListener, KeyListener, MouseListener {
	protected JFrame frame;
	protected Field field;

	/**
	 * This constructor creates a world without ticks.
	 */
	public AWorld() {
		this.init();
	}

	/**
	 * This constructor is used to create a world that
	 * ticks with an give regular interval.
	 * @param ticks
	 */
	public AWorld(int ticks) {
		this.timerticks = true;
		this.ticks = ticks;
	}
	
	/**
	 * Override these method to create your main window and initialize 
	 * your GUI.
	 */
	abstract protected void createAndShowGUI();

	/**
	 * Override these method to react on ticks. Please note,
	 * that you need to create the world with the constructor
	 * that takes the ticks as a parameter.
	 * Otherwise the method is never called.
	 */
	protected void onTick() {}

	/**
	 * Changes the tick rate of the timer. This method has only an
	 * effect if the world was created with an timer. 
	 * @param ticks
	 */
	public final void setTicks(int ticks) {
		if (this.timer != null && this.timerticks) {
			if (this.ticks != ticks) {
				this.ticks = ticks;
				this.timer.setDelay(ticks);
			}
		}	
	}

	@Override
	public final void actionPerformed(ActionEvent e) {
		this.onTick();
	}

	@Override
	public void keyPressed(KeyEvent e) {}

	@Override
	public void keyReleased(KeyEvent e) {}

	@Override
	public void keyTyped(KeyEvent e) {}

	@Override
	public void mouseClicked(MouseEvent e) {}

	@Override
	public void mouseEntered(MouseEvent e) {}

	@Override
	public void mouseExited(MouseEvent e) {}

	@Override
	public void mousePressed(MouseEvent e) {}

	@Override
	public void mouseReleased(MouseEvent e) {}

	
	// Timer Infos 
	private Timer timer;
	private int ticks = 0; 
	private boolean timerticks = false;
	

	/**
	 * Initialize the World. It forces the run of the
	 * createAndShowGUIWrapper Method inside the event
	 * dispatch thread.
	 */
	private void init() {
		SwingUtilities.invokeLater(new Runnable() {
			public void run() {
				createAndShowGUIWrapper();
			}
		});	
		
	}

	/**
	 * This function is called inside the constructor to
	 * ensure that the createAndShowGUI method is called
	 * inside the event dispatch thread.
	 * It also handles timer initialization and registers
	 * the key and mouse listeners.
	 */
	private void createAndShowGUIWrapper() {
		System.out.println("Created GUI on EDT? "+
                SwingUtilities.isEventDispatchThread());
        if (!SwingUtilities.isEventDispatchThread()) {
    		// You are only allowed to call this method from
        	// the event dispatcher thread. Otherwise some
        	// swing will not work correctly.
        	return;        	
        }
        
           
	    // call the method that shows the frame and initialize it
		this.createAndShowGUI(); 

		// initialize timer
		if (this.timerticks) {
			this.timer = new Timer(ticks, this);
			this.timer.start();
		}
		// add listeners (key and mouse)
		this.frame.addKeyListener(this);
		this.field.addKeyListener(this);
		this.frame.addMouseListener(this);
		this.field.addMouseListener(this);
	}
}
