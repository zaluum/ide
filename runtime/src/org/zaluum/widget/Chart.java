package org.zaluum.widget;

import info.monitorenter.gui.chart.Chart2D;
import info.monitorenter.gui.chart.ITrace2D;
import info.monitorenter.gui.chart.traces.Trace2DLtd;

import java.awt.Color;

import org.zaluum.annotation.Apply;
import org.zaluum.annotation.Box;

@Box
public class Chart {
  private ITrace2D trace;
  private long i = 0;
  public Chart2D _widget;
  public Chart() {
    _widget = new Chart2D();
    trace = new Trace2DLtd(2000);
    trace.setColor(Color.BLUE);
    _widget.addTrace(trace);
    _widget.getAxisX().setPaintScale(true);
    _widget.getAxisX().setRangePolicy(new MyRangePolicy(2000));
    _widget.setPaintLabels(false);
  }
  @Apply
  public void apply(double data) {
    trace.addPoint(i++, data);
  }
}
