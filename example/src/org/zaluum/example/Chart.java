package org.zaluum.example;

import info.monitorenter.gui.chart.Chart2D;
import info.monitorenter.gui.chart.ITrace2D;
import info.monitorenter.gui.chart.rangepolicies.RangePolicyUnbounded;
import info.monitorenter.gui.chart.traces.Trace2DLtd;

import java.awt.Color;

import org.zaluum.runtime.Box;
import org.zaluum.runtime.In;

@Box
public class Chart {
  @In(x = 0, y = 24) public double data;
  private ITrace2D trace;
  private long i = 0;
  public Chart2D _widget;
  public Chart() {
    _widget = new Chart2D();
    trace = new Trace2DLtd(500);
    trace.setColor(Color.BLUE);
    _widget.addTrace(trace);
    _widget.getAxisX().setPaintScale(true);
    _widget.getAxisX().setRangePolicy(new MyRangePolicy(500));
    _widget.setPaintLabels(false);
    //_widget = new ChartPanel(chart);
  }
  public void apply() {
    trace.addPoint(i++, data);
  }
}
 