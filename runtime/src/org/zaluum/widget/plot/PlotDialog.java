package org.zaluum.widget.plot;

import info.monitorenter.gui.chart.Chart2D;
import info.monitorenter.gui.chart.ITrace2D;
import info.monitorenter.gui.chart.traces.Trace2DLtd;

import java.util.Random;

import javax.script.ScriptEngine;
import javax.script.ScriptEngineManager;

import org.eclipse.jface.dialogs.Dialog;
import org.eclipse.swt.SWT;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Control;
import org.eclipse.swt.widgets.Shell;

public class PlotDialog extends Dialog {

	public PlotConfigurerComposite plotComposite;
	private String script;
	private final String initScript;

	public PlotDialog(Shell parentShell, String initScript) {
		super(parentShell);
		this.initScript = initScript;
	}

	@Override
	protected Control createDialogArea(Composite parent) {
		Composite composite = (Composite) super.createDialogArea(parent);
		// add controls to composite as necessary
		plotComposite = new PlotConfigurerComposite(parent, SWT.NONE);
		Chart2D c = plotComposite.chart;
		if (initScript != null) {
			ScriptEngineManager manager = new ScriptEngineManager();
			ScriptEngine engine = manager.getEngineByExtension("js");
			try {
				engine.put("c", c);
				engine.eval(initScript);
			} catch (Exception ex) {
				ex.printStackTrace();
			}
		}
		if (c.getTraces().size() == 0)
			c.addTrace(new Trace2DLtd(20));
		Random random = new Random();
		for (ITrace2D t : c.getTraces()) {
			for (int i = 10; i >= 0; i--) {
				t.addPoint(i, random.nextDouble() * 10.0 + i - 50);
			}
		}
		plotComposite.refresh();
		return composite;
	}

	@Override
	protected void okPressed() {
		script = new PlotConfiguration().javaScriptConfigure(plotComposite.chart);
		super.okPressed();
	}

	public String getScript() {
		return script;
	}
}
