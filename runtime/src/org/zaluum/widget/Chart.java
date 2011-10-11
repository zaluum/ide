package org.zaluum.widget;

import info.monitorenter.gui.chart.Chart2D;
import info.monitorenter.gui.chart.traces.Trace2DLtd;

import java.awt.Color;

import org.zaluum.annotation.Apply;
import org.zaluum.annotation.Box;
import org.zaluum.basic.Zaluum;

@Box(configurer=ChartConfigurer.class)
public class Chart extends Chart2D {
	private static final long serialVersionUID = 1L;
	private long i = 0;
	private final CircularBuffer buffer;
	private final int maxlen = 2000;

	public Chart() {
		super();
		buffer = new CircularBuffer(maxlen);
		Trace2DLtd trace = new Trace2DLtd(maxlen);
		trace.setColor(Color.BLUE);
		addTrace(trace);
	}

	private Runnable runnable = new Runnable() {
		public void run() {
			final double[] copy;
			final long savedi;
			synchronized (Chart.this) {
				copy = buffer.orderedCopy();
				savedi = i - copy.length;
				buffer.clear();
			}
			for (int j = 0; j < copy.length; j++)
				getTraces().first().addPoint(savedi + j, copy[j]);
		}
	};

	@Apply
	public void apply(double data) {
		synchronized (this) {
			buffer.add(data);
			i++;
		}
		Zaluum.fastUpdate(this, runnable);
	}
}
