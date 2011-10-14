package org.zaluum.widget.plot;

import info.monitorenter.gui.chart.Chart2D;
import info.monitorenter.gui.chart.IAxis;

import java.awt.BorderLayout;
import java.awt.Frame;
import java.awt.Panel;

import org.eclipse.swt.SWT;
import org.eclipse.swt.awt.SWT_AWT;
import org.eclipse.swt.custom.SashForm;
import org.eclipse.swt.custom.ScrolledComposite;
import org.eclipse.swt.layout.FillLayout;
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
		setLayoutData(new GridData(SWT.FILL, SWT.FILL, true, true));

		SashForm sashForm = new SashForm(this, SWT.VERTICAL);
		sashForm.setLayoutData(new GridData(SWT.FILL, SWT.FILL, true, true, 1, 1));
		
		Composite previewChart = new Composite(sashForm, SWT.NO_BACKGROUND
				| SWT.EMBEDDED);
		GridData gd_previewChart = new GridData(SWT.FILL, SWT.FILL, true, true,
				1, 1);
		gd_previewChart.minimumHeight = 300;
		gd_previewChart.widthHint = 500;
		previewChart.setLayoutData(gd_previewChart);
		createAWTChart2d(previewChart);

		
		ScrolledComposite scrolledComposite = new ScrolledComposite(sashForm, SWT.BORDER | SWT.H_SCROLL | SWT.V_SCROLL);
		scrolledComposite.setExpandHorizontal(true);
		scrolledComposite.setExpandVertical(true);
		
		Composite menusComposite = new Composite(scrolledComposite, SWT.NONE);
		menusComposite.setLayout(new FillLayout(SWT.HORIZONTAL));
		
		TabFolder tabFolder = new TabFolder(menusComposite, SWT.NONE);

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
		
		scrolledComposite.setContent(menusComposite);
		scrolledComposite.setMinSize(menusComposite.computeSize(SWT.DEFAULT, SWT.DEFAULT));
		sashForm.setWeights(new int[] {50,100});
		

	
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
