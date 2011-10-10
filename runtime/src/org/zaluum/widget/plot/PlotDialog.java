package org.zaluum.widget.plot;

import info.monitorenter.gui.chart.Chart2D;
import info.monitorenter.gui.chart.ITrace2D;
import info.monitorenter.gui.chart.traces.Trace2DLtd;

import java.io.FileNotFoundException;
import java.io.FileReader;
import java.io.FileWriter;
import java.io.IOException;
import java.util.Random;

import javax.script.ScriptEngine;
import javax.script.ScriptEngineManager;
import javax.script.ScriptException;

import org.eclipse.jface.dialogs.Dialog;
import org.eclipse.swt.SWT;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Control;
import org.eclipse.swt.widgets.Shell;

public class PlotDialog extends Dialog {

	public PlotComposite plotComposite;

	protected PlotDialog(Shell parentShell) {
		super(parentShell);
	}

	@Override
	protected Control createDialogArea(Composite parent) {
		Composite composite = (Composite) super.createDialogArea(parent);
		// add controls to composite as necessary
		plotComposite = new PlotComposite(parent, SWT.NONE);
		Chart2D c = plotComposite.chart;
		ScriptEngineManager manager = new ScriptEngineManager();
		ScriptEngine engine = manager.getEngineByExtension("js");
		try {
			FileReader f = new FileReader("conf.js");
			engine.put("c", c);
			engine.eval(f);
			f.close();
		}catch (ScriptException ex) {
			ex.printStackTrace();
		} catch (FileNotFoundException e) {
			e.printStackTrace();
		} catch (IOException e) {
			// TODO Auto-generated catch block
			e.printStackTrace();
		}
		if (c.getTraces().size()==0)
			c.addTrace(new Trace2DLtd(20));
		Random random = new Random();
		for (ITrace2D t: c.getTraces()) {
			for (int i = 10; i >= 0; i--) {
				t.addPoint(i, random.nextDouble() * 10.0 + i-50);
			}
		}
		plotComposite.refresh();
		return composite;
	}
	@Override
	protected void okPressed() {
		try {
			FileWriter file = new FileWriter("conf.js");
			String string = PlotConfiguration.javaScriptConfigure(plotComposite.chart);
			file.write(string);
			file.flush();
			file.close();
			new PlotDialog(getShell()).open();
		} catch (IOException e) {
			// TODO Auto-generated catch block
			e.printStackTrace();
		}
		super.okPressed();
	}
}
