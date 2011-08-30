package org.zaluum.widget;

import java.lang.reflect.InvocationTargetException;
import java.util.concurrent.atomic.AtomicReference;

import javax.swing.JTextField;

import org.zaluum.annotation.Apply;
import org.zaluum.annotation.Box;
import org.zaluum.basic.Zaluum;

@Box
public class TextWidget {
	public JTextField _widget = new JTextField();

	public TextWidget() {
		_widget.setEnabled(false);
	}

	private AtomicReference<Double> value = new AtomicReference<Double>(0.0);
	private Runnable r = new Runnable() {
		@Override
		public void run() {
			_widget.setText("" + value.get());
		}
	};

	@Apply
	public void apply(double d) throws InterruptedException,
			InvocationTargetException {
		value.set(d);
		Zaluum.fastUpdate(_widget, r);
	}
}
