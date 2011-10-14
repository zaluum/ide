package org.zaluum.widget.plot;

import info.monitorenter.gui.chart.Chart2D;
import info.monitorenter.gui.chart.Chart2D.ToolTipType;

import org.eclipse.swt.SWT;
import org.eclipse.swt.events.ModifyEvent;
import org.eclipse.swt.events.ModifyListener;
import org.eclipse.swt.events.SelectionAdapter;
import org.eclipse.swt.events.SelectionEvent;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Button;
import org.eclipse.swt.widgets.Combo;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Group;
import org.eclipse.swt.widgets.Label;
import org.eclipse.swt.widgets.Spinner;

public class ChartCompositePlotTab extends Group {
	private Button btnLegend;
	private Chart2D chart;
	private Button btnUseantialias;
	private Spinner latency;

	@Override
	protected void checkSubclass() {
	}

	public ChartCompositePlotTab(Composite parent, int style,
			PlotConfigurerComposite comp) {
		super(parent, style);
		chart = comp.chart;
		setText("Behaviour");
		this.setLayout(new GridLayout(2, false));

		btnLegend = new Button(this, SWT.CHECK);
		btnLegend.addSelectionListener(new SelectionAdapter() {
			@Override
			public void widgetSelected(SelectionEvent e) {
				chart.setPaintLabels(btnLegend.getSelection());
			}
		});
		btnLegend.setText("Show legend");
		new Label(this, SWT.NONE);
		btnUseantialias = new Button(this, SWT.CHECK);
		btnUseantialias.setText("Paint using antialias");
		btnUseantialias.addSelectionListener(new SelectionAdapter() {
			@Override
			public void widgetSelected(SelectionEvent e) {
				chart.setUseAntialiasing(btnUseantialias.getSelection());
				chart.setRequestedRepaint(true);
			}
		});
		new Label(this, SWT.NONE);

		Label lblTimeBetweenRefreshes = new Label(this, SWT.NONE);
		lblTimeBetweenRefreshes.setText("Minimum paint latency (ms)");

		latency = new Spinner(this, SWT.BORDER);
		latency.addModifyListener(new ModifyListener() {
			
			@Override
			public void modifyText(ModifyEvent e) {
				chart.setMinPaintLatency(latency.getSelection());
			}
		});
		Label lblMode = new Label(this, SWT.NONE);
		lblMode.setText("Tooltip Mode");

		final Combo tooltipCombo = new Combo(this, SWT.NONE);
		tooltipCombo.addSelectionListener(new SelectionAdapter() {
			@Override
			public void widgetSelected(SelectionEvent e) {
				switch (tooltipCombo.getSelectionIndex()) {
				case -1:
				case 0:
					chart.setToolTipType(ToolTipType.NONE);
					break;
				case 1:
					chart.setToolTipType(ToolTipType.DATAVALUES);
					break;
				case 2:
					chart.setToolTipType(ToolTipType.PIXEL);
					break;
				case 3:
					chart.setToolTipType(ToolTipType.VALUE_SNAP_TO_TRACEPOINTS);
					break;
				}
			}
		});
		tooltipCombo.setItems(new String[] { "None", "Data Value",
				"Pixel Position", "Snap to trace" });
		tooltipCombo.setLayoutData(new GridData(SWT.FILL, SWT.CENTER, true,
				false, 1, 1));
		tooltipCombo.select(1);
	}

	public void refresh() {
		btnLegend.setSelection(chart.isPaintLabels());
		btnUseantialias.setSelection(chart.isUseAntialiasing());
		latency.setSelection(chart.getMinPaintLatency());
	}
}
