package org.zaluum.examples.controlMixer;

import java.awt.Color;
import java.awt.Graphics;

import javax.swing.JComponent;
import javax.swing.border.BevelBorder;

import org.zaluum.annotation.Apply;
import org.zaluum.annotation.Box;

@Box
public class BooleanPipe extends JComponent {

	private static final long serialVersionUID = 1L;
	private Color trueColor = Color.green;
	private Color falseColor = Color.red;
	private boolean status = false;

	public BooleanPipe() {
		updateBackgroundColor();
		setBorder(new BevelBorder(BevelBorder.RAISED));
	}

	public Color getTrueColor() {
		return trueColor;
	}

	public void setTrueColor(Color trueColor) {
		if (trueColor != this.trueColor) {
			this.trueColor = trueColor;
			updateBackgroundColor();
			repaint();
		}
	}

	public Color getFalseColor() {
		return falseColor;
	}

	public void setFalseColor(Color falseColor) {
		if (this.falseColor != falseColor) {
			this.falseColor = falseColor;
			updateBackgroundColor();
			repaint();
		}
	}

	public boolean getStatus() {
		return status;
	}

	private void updateBackgroundColor() {
		setBackground(status ? trueColor : falseColor);
	}

	@Apply
	public void setStatus(boolean status) {
		if (this.status != status) {
			this.status = status;
			updateBackgroundColor();
			repaint();
		}
	}

	@Override
	protected void paintComponent(Graphics g) {
		super.paintComponent(g);
		g.setColor(getBackground());
		g.fillRect(0, 0, getBounds().width, getBounds().height);

	}
}
