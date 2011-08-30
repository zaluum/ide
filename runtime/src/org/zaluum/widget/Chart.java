package org.zaluum.widget;

import info.monitorenter.gui.chart.Chart2D;
import info.monitorenter.gui.chart.ITrace2D;
import info.monitorenter.gui.chart.traces.Trace2DLtd;

import java.awt.Color;
import java.util.ArrayList;
import java.util.List;

import org.zaluum.annotation.Apply;
import org.zaluum.annotation.Box;
import org.zaluum.basic.Zaluum;

@Box
public class Chart {
	private ITrace2D trace;
	public Chart2D _widget;
	private long i = 0;
	private final List<Double> buffer = new ArrayList<Double>(1000);

	public Chart() {
		_widget = new Chart2D();
		trace = new Trace2DLtd(2000);
		trace.setColor(Color.BLUE);
		_widget.addTrace(trace);
		_widget.getAxisX().setPaintScale(true);
		_widget.getAxisX().setRangePolicy(new MyRangePolicy(2000));
		_widget.setPaintLabels(false);
	}

	private Runnable runnable = new Runnable() {
		@Override
		public void run() {
			final double[] copy;
			synchronized (Chart.this) {
				copy = new double[buffer.size()];
				for (int i = 0; i < buffer.size(); i++) {
					copy[i] = buffer.get(i);
				}
				buffer.clear();
			}
			for (Double d : copy)
				trace.addPoint(i++, d);
		}
	};

	@Apply
	public void apply(double data) {
		synchronized (this) {
			buffer.add(data);
		}
		Zaluum.fastUpdate(_widget, runnable);
	}
}
