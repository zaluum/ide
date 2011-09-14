package org.zaluum.examples.tank;

import java.awt.Color;
import java.awt.Graphics;

import javax.swing.JComponent;

import org.zaluum.annotation.Box;

@Box
public class TestComponent extends JComponent {
	private static final long serialVersionUID = 1L;

	@Override
	protected void paintComponent(Graphics g) {
		super.paintComponent(g);
		g.setColor(Color.red);
		g.fillRect(0, 0 , getBounds().width, getBounds().height);
	}

}
