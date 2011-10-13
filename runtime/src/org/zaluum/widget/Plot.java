package org.zaluum.widget;

import org.zaluum.annotation.Apply;
import org.zaluum.annotation.Box;

@Box(configurer = ChartConfigurer.class)
public class Plot extends PlotBase {
	private static final long serialVersionUID = 1L;

	@Apply
	public void apply(Object o) {
		if (o instanceof Double)
			single((Double) o);
		else if (o instanceof double[])
			multi((double[]) o);
		else if (o instanceof double[][])
			multi((double[][]) o);
	}
}
