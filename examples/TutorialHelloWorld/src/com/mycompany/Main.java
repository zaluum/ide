package com.mycompany;

import javax.swing.JFrame;

public class Main {
	public static void main(String[] args) {
		Hello hello = new Hello();
		JFrame jFrame = new JFrame();
		jFrame.setDefaultCloseOperation(JFrame.EXIT_ON_CLOSE);
		jFrame.add(hello);
		jFrame.pack();
		jFrame.setVisible(true);
		while (true) {
			hello.run();
		}
	}
}
