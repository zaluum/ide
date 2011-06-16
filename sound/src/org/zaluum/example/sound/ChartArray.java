package org.zaluum.example.sound;

import info.monitorenter.gui.chart.Chart2D;
import info.monitorenter.gui.chart.ITrace2D;
import info.monitorenter.gui.chart.rangepolicies.RangePolicyMinimumViewport;
import info.monitorenter.gui.chart.traces.Trace2DLtd;
import info.monitorenter.util.Range;

import java.awt.Color;
import java.util.Arrays;

import org.zaluum.annotation.Box;
import org.zaluum.annotation.In;


@Box
public class ChartArray {
  @In public double[] data;
  private ITrace2D trace;
  public Chart2D _widget;
  public ChartArray() {
    _widget = new Chart2D();
    trace = new Trace2DLtd(2000);
    trace.setColor(Color.BLUE);
    _widget.addTrace(trace);
    _widget.getAxisX().setPaintScale(true);
    _widget.getAxisY().setRangePolicy(new RangePolicyMinimumViewport(new Range(0,0.2)));
    //_widget.getAxisX().setRangePolicy(new MyRangePolicy(2000));
    _widget.setPaintLabels(false);
  }
  private double[] cache;
  public void apply() {
	  if (Arrays.equals(cache, data)) return;
	  trace.removeAllPoints();
	  for (int i=0;i<data.length;i++) {		  
		  trace.addPoint(i,data[i]);
	  }
	  cache=data.clone();
  }
}
