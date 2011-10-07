package org.zaluum.widget.plot;

import java.util.Random;

import info.monitorenter.gui.chart.Chart2D;
import info.monitorenter.gui.chart.ITrace2D;
import info.monitorenter.gui.chart.traces.Trace2DSimple;

import org.eclipse.core.databinding.observable.Realm;
import org.eclipse.jface.databinding.swt.SWTObservables;
import org.eclipse.swt.SWT;
import org.eclipse.swt.events.DisposeEvent;
import org.eclipse.swt.events.DisposeListener;
import org.eclipse.swt.layout.FillLayout;
import org.eclipse.swt.widgets.Display;
import org.eclipse.swt.widgets.Shell;

public class PlotMain {
	public static void main(String[] args) {
		final Display display = new Display();
		final Shell shell = new Shell(display);
		shell.setSize(600, 700);
		shell.setLayout(new FillLayout());
		try {
			System.setProperty("sun.awt.noerasebackground", "true");
		} catch (NoSuchMethodError error) {
		}
		PlotComposite plotComposite = new PlotComposite(shell, SWT.NONE);
		Chart2D c = plotComposite.chart;
		ITrace2D trace = new Trace2DSimple();
		c.addTrace(trace);
		c.setUseAntialiasing(true);
		Random random = new Random();
		for (int i = 100; i >= 0; i--) {
			trace.addPoint(i, random.nextDouble() * 10.0 + i-50);
		}
		plotComposite.refresh();
		shell.addDisposeListener(new DisposeListener() {
			
			@Override
			public void widgetDisposed(DisposeEvent e) {
				System.exit(0);
			}
		});
		shell.open();
		while (!shell.isDisposed()) {
			if (!display.readAndDispatch())
				display.sleep();
		}
		display.dispose();

	}
}
