package org.zaluum.example.sound;

import java.awt.Graphics;

import javax.swing.JComponent;

import org.zaluum.annotation.Apply;
import org.zaluum.annotation.Box;

@Box
public class SimpleOsc extends JComponent{
	volatile double[] data = new double[1024];
	@Apply
	public void apply(double[] data){
		this.data=data;
		repaint();
	}
	private static final long serialVersionUID = 1L;
	@Override
	public void paint(Graphics g) {
		double dx = ((double)getWidth())/(data.length/2);
		int midy = getHeight()/2;
		double before = data[0];
		double counter = 0;
		for (int i=1; i<data.length/2; i++) {
			double now = data[i];
			double newCounter = counter+dx;
			g.drawLine((int)counter, (int)(midy+before), (int)newCounter, (int)(midy+now));
			before=now;
			counter = newCounter;
			
		}
	}

}
