package org.zaluum.widget;

import info.monitorenter.gui.chart.Chart2D;
import info.monitorenter.gui.chart.ITrace2D;
import info.monitorenter.gui.chart.traces.Trace2DLtd;

import java.awt.Color;

import org.zaluum.annotation.Apply;
import org.zaluum.annotation.Box;
import org.zaluum.basic.Zaluum;

@Box
public class Chart extends Chart2D {
	private static final long serialVersionUID = 1L;
	private ITrace2D trace;
	private long i = 0;
	private final CircularBuffer buffer;
	private final int maxlen = 2000;

	public Chart() {
		trace = new Trace2DLtd(maxlen);
		buffer = new CircularBuffer(maxlen);
		trace.setColor(Color.BLUE);
		addTrace(trace);
		getAxisX().setPaintScale(true);
		getAxisX().setRangePolicy(new MyRangePolicy(maxlen));
		setPaintLabels(false);
	}

	private Runnable runnable = new Runnable() {
		@Override
		public void run() {
			final double[] copy;
			final long savedi;
			synchronized (Chart.this) {
				copy = buffer.orderedCopy();
				savedi = i - copy.length;
				buffer.clear();
			}
			for (int j = 0; j < copy.length; j++)
				trace.addPoint(savedi + j, copy[j]);
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
