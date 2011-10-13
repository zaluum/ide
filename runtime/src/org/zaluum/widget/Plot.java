package org.zaluum.widget;

import java.util.Iterator;

import org.zaluum.annotation.Apply;
import org.zaluum.annotation.Box;

import info.monitorenter.gui.chart.Chart2D;
import info.monitorenter.gui.chart.ITrace2D;
import info.monitorenter.gui.chart.traces.Trace2DLtd;

@Box(configurer=ChartConfigurer.class)
public class Plot extends Chart2D {
	private static final long serialVersionUID = 1L;
	long x = 0;

	public void createTraces(int n) {
		int toCreate = n - getTraces().size();
		if (toCreate > 0) {
			for (int i = 0; i < toCreate; i++) {
				Trace2DLtd trace = new Trace2DLtd(200, "trace-"
						+ getTraces().size());
				addTrace(trace);
			}
		}
	}
	@Apply
	public void apply(Object o) {
		if (o instanceof Double) 
			single((Double)o);
		else if (o instanceof double[]) 
			multi((double[])o);
		else if (o instanceof double[][])
			multi((double[][])o);
	}
	public void single(double data) {
		createTraces(1);
		ITrace2D trace = getTraces().first();
		trace.addPoint(x, data);
		x++;
	}

	public void array(double[] data) {
		if (data == null || data.length == 0)
			return;
		createTraces(1);
		ITrace2D trace = getTraces().first();
		for (int i = 0; i < data.length; i++) {
			trace.addPoint(x, data[i]);
			x++;
		}
	}

	public void multi(double[] data) {
		if (data == null || data.length == 0)
			return;
		createTraces(data.length);
		Iterator<ITrace2D> iterator = getTraces().iterator();
		for (int i = 0; i < data.length; i++) {
			ITrace2D trace2d = iterator.next();
			trace2d.addPoint(x, data[i]);
		}
		x++;
	}

	public void multi(double[][] data) {
		if (data == null || data.length == 0)
			return;
		createTraces(data.length);
		ITrace2D trace = getTraces().first();
		long initx = x;
		long maxl = 0;
		for (int i = 0; i < data.length; i++) {
			maxl = Math.max(maxl, data[i].length);
			for (int j = 0; j < data[i].length; j++) {
				trace.addPoint(initx + j, data[i][j]);
			}
		}
		x += maxl;
	}

}
