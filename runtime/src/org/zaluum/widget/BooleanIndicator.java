package org.zaluum.widget;

import java.awt.Color;
import java.awt.Graphics;

import javax.swing.JComponent;

import org.zaluum.annotation.Apply;
import org.zaluum.annotation.Box;
@Box
public class BooleanIndicator extends JComponent{
	public BooleanIndicator() {
		apply(false);
	}
	@Apply
	public void apply(boolean b){
		if (b) setBackground(Color.green);
		else setBackground(Color.red);
	}
	@Override
	protected void paintComponent(Graphics g) {
		g.setColor(getBackground());
		g.fillRect(0,0, getBounds().width, getBounds().height);
	}
	private static final long serialVersionUID = 1L;
	
}
