package intro.proglang.informatik.unifreiburg.de;

import java.awt.Color;
import java.awt.Dimension;
import java.awt.Graphics;

import javax.swing.BorderFactory;
import javax.swing.JPanel;

public class Field extends JPanel {

	/**
	 * 
	 */
	private static final long serialVersionUID = -5841286264742683204L;
	private boolean circle = false;
	private boolean rectangle = false;
	private boolean line = false;

	public Field() {
		setBorder(BorderFactory.createLineBorder(Color.black));
	}
	
	
	public Dimension getPreferredSize() {
        return new Dimension(250,200);
    }

    public void paintComponent(Graphics g) {
        super.paintComponent(g);       

        // Draw Text
        //g.drawString("This is my custom Panel!",10,20);
        if (this.line) g.drawLine(1, 1, 50, 50);
        if (this.circle) g.drawArc(80, 80, 40, 40, 0, 360);
        if (this.rectangle) g.draw3DRect(20, 80, 15, 5, true);
    } 
    
    public void toggleCircle() {
    	this.circle = !this.circle;
    	this.repaint();
    }

    public void toggleRectangle() {
    	this.rectangle = !this.rectangle;
    	this.repaint();
    }

    public void toggleLine() {
    	this.line = !this.line;
    	this.repaint();
    }
    
    public void clear() {
    	this.circle = false;
    	this.rectangle = false;
    	this.line = false;
    	this.repaint();
    }
    
}
