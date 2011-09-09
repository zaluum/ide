package org.zaluum.widget;

import java.lang.reflect.InvocationTargetException;
import java.text.NumberFormat;
import java.util.concurrent.atomic.AtomicReference;

import javax.swing.JTextField;

import org.zaluum.annotation.Apply;
import org.zaluum.annotation.Box;
import org.zaluum.basic.Zaluum;

@Box
public class TextWidget extends JTextField {

	private static final long serialVersionUID = 1L;

	public TextWidget() {
		setEnabled(false);
	}

	private AtomicReference<Double> value = new AtomicReference<Double>(0.0);
	private Runnable r = new Runnable() {
		public void run() {
			setText(NumberFormat.getInstance().format((double)value.get()));
		}
	};

	@Apply
	public void apply(double d) throws InterruptedException,
			InvocationTargetException {
		value.set(d);
		Zaluum.fastUpdate(this, r);
	}
}
