package org.zaluum.widget.plot;

import info.monitorenter.gui.chart.Chart2D;
import info.monitorenter.gui.chart.IAxis;

import java.awt.BorderLayout;
import java.awt.Frame;
import java.awt.Panel;

import org.eclipse.swt.SWT;
import org.eclipse.swt.awt.SWT_AWT;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Control;
import org.eclipse.swt.widgets.TabFolder;
import org.eclipse.swt.widgets.TabItem;

public class PlotConfigurerComposite extends Composite {

	public Chart2D chart;
	TraceCompositePlotTab traceC;
	AxesCompositePlotTab scalesC;
	ChartCompositePlotTab chartC;

	public void createAWTChart2d(Composite parent) {
		Frame frame = SWT_AWT.new_Frame(parent);
		Panel panel = new Panel(new BorderLayout()) {
			private static final long serialVersionUID = 1L;

			public void update(java.awt.Graphics g) {
				/* Do not erase the background */
				paint(g);
			}
		};
		frame.add(panel);
		chart = new Chart2D();
		for (IAxis a : chart.getAxes()) {
			a.setFormatter(new LabelFormatterDecimal());
		}
		panel.add(chart);
		panel.validate();
		parent.layout();
	}

	/**
	 * Create the composite.
	 * 
	 * @param parent
	 * @param style
	 */
	public PlotConfigurerComposite(Composite parent, int style) {
		super(parent, style);
		setLayout(new GridLayout(1, false));

		Composite previewComposite = new Composite(this, SWT.NONE);
		GridData gd_previewComposite = new GridData(SWT.FILL, SWT.FILL, true,
				true, 1, 1);
		gd_previewComposite.heightHint = 205;
		previewComposite.setLayoutData(gd_previewComposite);
		previewComposite.setLayout(new GridLayout(1, false));

		Composite previewChart = new Composite(previewComposite,
				SWT.NO_BACKGROUND | SWT.EMBEDDED);
		GridData gd_previewChart = new GridData(SWT.FILL, SWT.FILL, true, true,
				1, 1);
		gd_previewChart.widthHint = 252;
		gd_previewChart.heightHint = 222;
		previewChart.setLayoutData(gd_previewChart);
		createAWTChart2d(previewChart);

		TabFolder tabFolder = new TabFolder(this, SWT.NONE);
		GridData gd_tabFolder = new GridData(SWT.FILL, SWT.CENTER, true, false,
				1, 1);
		gd_tabFolder.heightHint = 373;
		gd_tabFolder.widthHint = 503;
		tabFolder.setLayoutData(gd_tabFolder);

		TabItem tbtmChart = new TabItem(tabFolder, SWT.NONE);
		tbtmChart.setText("Chart");
		chartC = new ChartCompositePlotTab(tabFolder, SWT.NONE, this);
		tbtmChart.setControl(chartC);

		TabItem tbtmScales = new TabItem(tabFolder, SWT.NONE);
		tbtmScales.setText("Axes");
		scalesC = new AxesCompositePlotTab(tabFolder, SWT.NONE, this);
		tbtmScales.setControl(scalesC);

		TabItem tbtmPlot = new TabItem(tabFolder, SWT.NONE);
		tbtmPlot.setText("Traces");

		traceC = new TraceCompositePlotTab(tabFolder, SWT.NONE, chart);
		tbtmPlot.setControl(traceC);

	}

	public void refresh() {
		chartC.refresh();
		scalesC.refresh();
		traceC.refresh();
	}

	public static void setAllEnabled(Composite c, boolean b) {
		c.setEnabled(b);
		for (Control control : c.getChildren()) {
			if (control instanceof Composite)
				setAllEnabled((Composite) control, b);
			else
				control.setEnabled(b);
		}
	}

}
