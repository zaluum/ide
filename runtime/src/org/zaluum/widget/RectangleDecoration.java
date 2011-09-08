package org.zaluum.widget;

import java.awt.BasicStroke;
import java.awt.Color;
import java.awt.Graphics;
import java.awt.Graphics2D;

import javax.swing.JComponent;

import org.zaluum.annotation.Box;

@Box
public class RectangleDecoration extends JComponent {
	private static final long serialVersionUID = 1L;
	public BasicStroke stroke = new BasicStroke(1);

	public RectangleDecoration() {
		setForeground(Color.black);
		setBackground(Color.white);
	}

	@Override
	public void paintComponent(Graphics graphics) {
		Graphics2D g = (Graphics2D) graphics;
		g.setColor(getBackground());
		g.fillRect(0, 0, getWidth() + 1, getHeight() + 1);
		g.setColor(getForeground());
		g.setStroke(stroke);
		int half = (int) Math.floor(stroke.getLineWidth() / 2);
		g.drawRect(half, half, getWidth() - (int) stroke.getLineWidth() - 1,
				getHeight() - (int) stroke.getLineWidth() - 1);
	}

	public void setLineWidth(float width) {
		stroke = new BasicStroke(width);
	}

	public float getLineWidth() {
		return stroke.getLineWidth();
	}
}
