package org.zaluum.example.sound;

import javax.swing.JPanel;
import javax.swing.JTextField;

import org.zaluum.annotation.Box;

@Box
public class TestJPanel {
	public JPanel _widget;
	public TestJPanel() {
		_widget = new JPanel(null);
		JTextField jTextField = new JTextField("hola");
		jTextField.setBounds(0,0,100,100);
		_widget.add(jTextField);
	}
	public void apply() {
		
	}
}
