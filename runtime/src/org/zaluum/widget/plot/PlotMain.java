package org.zaluum.widget.plot;

import org.eclipse.swt.events.DisposeEvent;
import org.eclipse.swt.events.DisposeListener;
import org.eclipse.swt.widgets.Display;
import org.eclipse.swt.widgets.Shell;

public class PlotMain {
	public static void main(String[] args) {
		final Display display = new Display();
		final Shell shell = new Shell(display);
		try {
			System.setProperty("sun.awt.noerasebackground", "true");
		} catch (NoSuchMethodError error) {
		}
		PlotDialog plotDialog = new PlotDialog(shell, "");
		plotDialog.open();
		PlotDialog plot2 = new PlotDialog(shell, plotDialog.getScript());
		plot2.open();
		shell.addDisposeListener(new DisposeListener() {

			public void widgetDisposed(DisposeEvent e) {
				System.exit(0);
			}
		});
		while (!shell.isDisposed()) {
			if (!display.readAndDispatch())
				display.sleep();
		}
		display.dispose();

	}
}
