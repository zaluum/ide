package org.zaluum.widget;

import info.monitorenter.gui.chart.Chart2D;
import info.monitorenter.gui.chart.ITrace2D;
import info.monitorenter.gui.chart.traces.Trace2DLtd;

import java.awt.Color;

import org.zaluum.annotation.Apply;
import org.zaluum.annotation.Box;
import org.zaluum.basic.Zaluum;

@Box
public class Chart {
	private ITrace2D trace;
	public Chart2D _widget;
	private long i = 0;
	private final CircularBuffer buffer;
	private final int maxlen = 2000;

	public Chart() {
		_widget = new Chart2D();
		trace = new Trace2DLtd(maxlen);
		buffer = new CircularBuffer(maxlen);
		trace.setColor(Color.BLUE);
		_widget.addTrace(trace);
		_widget.getAxisX().setPaintScale(true);
		_widget.getAxisX().setRangePolicy(new MyRangePolicy(maxlen));
		_widget.setPaintLabels(false);
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
		Zaluum.fastUpdate(_widget, runnable);
	}
}
