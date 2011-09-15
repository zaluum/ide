package org.zaluum.examples.controlMixer;

import javax.swing.JFrame;

public class Main {
	public static void main(String[] args) {
		JFrame frame = new JFrame();
		ControlMixer m = new ControlMixer();
		frame.add(m);
		frame.pack();
		frame.setDefaultCloseOperation(JFrame.EXIT_ON_CLOSE);
		frame.setVisible(true);
		m.run();
	}
}
