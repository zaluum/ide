package org.zaluum.widget;

import info.monitorenter.gui.chart.Chart2D;
import info.monitorenter.gui.chart.IAxis;
import info.monitorenter.gui.chart.ITrace2D;
import info.monitorenter.gui.chart.axis.AxisLinear;
import info.monitorenter.gui.chart.traces.Trace2DLtd;

import java.beans.PropertyChangeEvent;
import java.beans.PropertyChangeListener;

public class PlotBase extends Chart2D {
	private static final long serialVersionUID = 1L;
	private long x = 0;
	private ITrace2D[] traces = null;
	
	public PlotBase() {
		super();
		PropertyChangeListener listener = new PropertyChangeListener() {
			@Override
			public void propertyChange(PropertyChangeEvent evt) {
				updateTraces();
			}
		};
		addPropertyChangeListener(IAxis.PROPERTY_ADD_REMOVE_TRACE, listener);
		addPropertyChangeListener(ITrace2D.PROPERTY_ZINDEX, listener);
		updateTraces();
	}
	private synchronized void updateTraces() {
		traces = getTraces().toArray(new ITrace2D[0]);
	}
	public synchronized void createTraces(int n) {
		int toCreate = n - traces.length;
		if (toCreate > 0) {
			for (int i = 0; i < toCreate; i++) {
				Trace2DLtd trace = new Trace2DLtd(200, "trace-"
						+ getTraces().size());
				int max = -1;
				for (ITrace2D t : traces) {
					max= Math.max(t.getZIndex(),max);
				}
				addTrace(trace, anyXAxisOrNew(), anyYAxisOrNew());
				trace.setZIndex(max+1);
			}
		}
	}
	private IAxis anyXAxisOrNew() {
		if (getAxesXBottom().size()!=0) return getAxisX();
		else if (!getAxesXTop().isEmpty()) return getAxesXTop().get(0);
		else {
			AxisLinear axis = new AxisLinear();
			addAxisXBottom(axis);
			return axis;
		}
	}
	private IAxis anyYAxisOrNew() {
		if (getAxesYLeft().size()!=0) return getAxisY();
		else if (!getAxesYRight().isEmpty()) return getAxesYRight().get(0);
		else {
			AxisLinear axis = new AxisLinear();
			addAxisYLeft(axis);
			return axis;
		}
	}
	public synchronized void single(double data) {
		createTraces(1);
		ITrace2D trace = traces[0];
		trace.addPoint(x, data);
		x++;
	}

	public synchronized void array(double[] data) {
		if (data == null || data.length == 0)
			return;
		createTraces(1);
		ITrace2D trace = traces[0];
		for (int i = 0; i < data.length; i++) {
			trace.addPoint(x, data[i]);
			x++;
		}
	}

	public synchronized void multi(double[] data) {
		if (data == null || data.length == 0)
			return;
		createTraces(data.length);
		for (int i = 0; i < data.length; i++) {
			ITrace2D trace2d = traces[i];
			trace2d.addPoint(x, data[i]);
		}
		x++;
	}

	public synchronized void multi(double[][] data) {
		if (data == null || data.length == 0)
			return;
		createTraces(data.length);
		long initx = x;
		long maxl = 0;
		for (int i = 0; i < data.length; i++) {
			ITrace2D trace = traces[i];
			maxl = Math.max(maxl, data[i].length);
			for (int j = 0; j < data[i].length; j++) {
				trace.addPoint(initx + j, data[i][j]);
			}
		}
		x += maxl;
	}

}
