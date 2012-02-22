package org.zaluum.tutorial.sound;

import java.awt.Color;
import java.awt.Graphics;

import javax.swing.JComponent;
import javax.swing.border.LineBorder;

import org.zaluum.annotation.Apply;
import org.zaluum.annotation.Box;

@Box
public class SimpleOscilloscope extends JComponent {
	private static final long serialVersionUID = 1L;
	// Mark it as volatile since it will be accessed by different threads.
	private volatile double[] data;
	private double yzoom = 1;
	private boolean centery;
	
	public SimpleOscilloscope(){
		super();
		setBackground(Color.white);
		setBorder(new LineBorder(Color.black));
	}
	@Apply
	public void apply(double[] data) {
		this.data = data;
		repaint(); // ask Swing to repaint when she can.
	}
	// This paint method will be executed in the Swing thread.
	@Override
	public void paintComponent(Graphics g) {
		double[] hold = data; // We "sample-and-hold" the data to avoid locking.
		g.setColor(getBackground());
		g.fillRect(0, 0, getWidth(), getHeight());
		g.setColor(getForeground());
		if (hold != null) {
			double dx = ((double) getWidth()) / hold.length;
			int midy = centery ? getHeight() / 2 : 0;
			double before = hold[0] * yzoom;
			double x = 0;
			for (int i = 1; i < hold.length; i++) {
				double now = hold[i] * yzoom;
				double newX = x + dx;
				g.drawLine((int) x, (int) (midy + before), (int) newX,
						(int) (midy + now));
				before = now;
				x = newX;
			}
		}
	}
	// getters and setters
	public void setYZoom(double yzoom) {
		this.yzoom = yzoom;
	}
	public double getYZoom() {
		return yzoom;
	}
	public void setCenterY(boolean centery) {
		this.centery = centery;
	}
	public boolean isCenterY() {
		return centery;
	}

}
