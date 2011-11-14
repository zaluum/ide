package org.zaluum.example.sound;

import javax.swing.JFrame;

public class Main {

	public static void main(String[] args) {
		
		TestWavelet filterTest = new TestWavelet();
		JFrame jFrame = new JFrame();
		jFrame.setDefaultCloseOperation(JFrame.EXIT_ON_CLOSE);
		jFrame.add(filterTest);
		jFrame.pack();
		jFrame.setVisible(true);
		filterTest.run();
	}

}
