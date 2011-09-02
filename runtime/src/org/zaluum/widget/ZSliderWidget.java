package org.zaluum.widget;

import java.util.concurrent.atomic.AtomicReference;

import javax.swing.JSlider;
import javax.swing.event.ChangeEvent;
import javax.swing.event.ChangeListener;

import org.zaluum.annotation.Apply;
import org.zaluum.annotation.Box;

/**
 * 
 * @author frede
 * 
 */
@Box
public class ZSliderWidget extends JSlider {
	private static final long serialVersionUID = 1L;
	private AtomicReference<Double> out = new AtomicReference<Double>();

	public ZSliderWidget() {
	}

	public ZSliderWidget(int min, int max) {
		setMaximum(max);
		setMinimum(min);
		setValue(0);
		addChangeListener(new ChangeListener() {
			@Override
			public void stateChanged(ChangeEvent e) {
				out.set((double) getValue());
			}
		});
	}

	@Apply
	public double apply() {
		return out.get();
	}
}
