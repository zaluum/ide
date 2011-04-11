package org.zaluum.example;

import info.monitorenter.gui.chart.Chart2D;
import info.monitorenter.gui.chart.ITrace2D;
import info.monitorenter.gui.chart.traces.Trace2DLtd;

import java.awt.Color;

import org.zaluum.runtime.Box;
import org.zaluum.runtime.In;

@Box
public class Chart {
  @In(x = 0, y = 24) public double data;
  public Chart2D _widget;
  private ITrace2D trace;
  private long i = 0;
  public Chart() {
    _widget = new Chart2D();
    trace = new Trace2DLtd(200); 
    trace.setColor(Color.RED);
    _widget.addTrace(trace);
  }
  public void apply() {
    trace.addPoint(i++, data);
  }
}
 