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
public class SliderWidget {
	private AtomicReference<Double> out = new AtomicReference<Double>();
	public JSlider _widget = new JSlider();

	public SliderWidget(int min, int max) {
		_widget.setMaximum(max);
		_widget.setMinimum(min);
		_widget.setValue(0);
		_widget.addChangeListener(new ChangeListener() {
			@Override
			public void stateChanged(ChangeEvent e) {
				out.set((double) _widget.getValue());
			}
		});
	}

	@Apply
	public double apply() {
		return out.get();
	}
}
