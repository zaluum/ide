package org.zaluum.widget;

import java.awt.Color;
import java.awt.Graphics;
import java.awt.Image;
import javax.swing.JComponent;

import org.zaluum.annotation.Apply;
import org.zaluum.annotation.Box;

@Box
public class BooleanIndicator extends JComponent {

	private Image trueImage;
	private Image falseImage;
	private boolean status;

	public BooleanIndicator() {
		apply(false);
	}

	public void setTrueImage(Image image) {
		trueImage = image;
	}

	public Image getTrueImage() {
		return trueImage;
	}

	public Image getFalseImage() {
		return falseImage;
	}

	public void setFalseImage(Image image) {
		falseImage = image;
	}

	public void setValue(boolean b) {
		apply(b);
	}

	public boolean getValue() {
		return status;
	}

	@Apply
	public void apply(boolean b) {
		status = b;
		if (b)
			setBackground(Color.green);
		else
			setBackground(Color.red);
	}

	@Override
	protected void paintComponent(Graphics g) {
		g.setColor(getBackground());
		g.fillRect(0, 0, getBounds().width, getBounds().height);
		Image i = status ? trueImage : falseImage;
		g.drawImage(i, 0, 0, null);
	}

	private static final long serialVersionUID = 1L;

}
