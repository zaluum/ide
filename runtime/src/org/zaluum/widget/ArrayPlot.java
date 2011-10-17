package org.zaluum.widget;

import org.zaluum.annotation.Apply;
import org.zaluum.annotation.Box;

@Box(configurer = PlotConfigurer.class)
public class ArrayPlot extends PlotBase {
	private static final long serialVersionUID = 1L;

	@Apply
	@Override
	public void array(double[] arr) {
		super.array(arr);
	}
}
