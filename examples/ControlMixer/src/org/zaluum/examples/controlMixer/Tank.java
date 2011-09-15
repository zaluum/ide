package org.zaluum.examples.controlMixer;

import java.awt.BorderLayout;
import java.awt.Color;
import java.awt.Graphics;

import javax.swing.JComponent;
import javax.swing.JLabel;

import org.zaluum.annotation.Apply;
import org.zaluum.annotation.Box;
@Box
public class Tank extends JComponent {

	private static final long serialVersionUID = 1L;
	private double value;
	private double capacity;
	private JLabel label;
	public Tank() {
		label = new JLabel();
		setLayout(new BorderLayout());
		add(label,BorderLayout.CENTER);
		setValue(50);
		setCapacity(100);
		setForeground(Color.blue);
		setBackground(Color.gray);
	}
	public double getValue() {
		return value;
	}
	@Apply
	public void setValue(double value) {
		if (this.value!=value) {
			this.value = value;
			label.setText(""+value);
			repaint();
		}
	}
	public double getCapacity() {
		return capacity;
	}
	public void setCapacity(double capacity) {
		if (this.capacity!=capacity) {
			this.capacity = capacity;
			repaint();
		}
	}
	@Override
	protected void paintComponent(Graphics g) {
		super.paintComponent(g);
		g.setColor(getBackground());
		g.fillRect(0, 0, getBounds().width, getBounds().height);
		if (capacity!=0) {
			double fillRatio = getValue()/getCapacity();
			if (fillRatio>0) {
				g.setColor(getForeground());
				int sizey = (int) (fillRatio * getBounds().height);
				int y = getBounds().height-sizey;
				g.fillRect(0, y, getBounds().width, sizey);
				g.setColor(Color.black);
				g.drawLine(0, y, getBounds().width, y);
			}
		}
	}
	
}
