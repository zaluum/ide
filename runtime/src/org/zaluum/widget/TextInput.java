package org.zaluum.widget;

import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.awt.event.FocusEvent;
import java.awt.event.FocusListener;
import java.lang.reflect.InvocationTargetException;
import java.util.concurrent.atomic.AtomicReference;

import javax.swing.InputVerifier;
import javax.swing.JComponent;
import javax.swing.JTextField;

import org.zaluum.annotation.Apply;
import org.zaluum.annotation.Box;

@Box
public class TextInput extends JTextField {

	private static final long serialVersionUID = 1L;
	private AtomicReference<Double> value = new AtomicReference<Double>(0.0);

	public TextInput() {
		setEnabled(true);
		setText("0.0");
		setInputVerifier(new InputVerifier() {
			@Override
			public boolean verify(JComponent input) {
				try {
					Double.parseDouble(getText());
					return true;
				} catch (Exception e) {
					return false;
				}
			}
		});
		addFocusListener(new FocusListener() {
			
			public void focusLost(FocusEvent e) {
				setText(""+value.get());
			}
			
			public void focusGained(FocusEvent e) {
				
			}
		});
		addActionListener(new ActionListener() {
			public void actionPerformed(ActionEvent e) {
				update();
			}
		});
	}

	private void update() {
		try {
			value.set(Double.parseDouble(getText()));
		} catch (Exception e) {
		}
	}

	@Override
	public void setText(String t) {
		super.setText(t);
		update();
	}

	@Apply
	public double apply() throws InterruptedException,
			InvocationTargetException {
		return value.get();
	}
}
