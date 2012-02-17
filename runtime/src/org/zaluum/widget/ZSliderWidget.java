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
		setMaximum(1000);
		setMinimum(0);
		setValue(0);
		out.set((double) 0);
		addChangeListener(new ChangeListener() {
			public void stateChanged(ChangeEvent e) {
				out.set((double) getValue()/1000.0);
			}
		});
	}

	@Apply
	public double apply() {
		return out.get();
	}
}
