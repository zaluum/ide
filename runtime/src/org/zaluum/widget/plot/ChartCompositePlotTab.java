package org.zaluum.widget.plot;

import info.monitorenter.gui.chart.Chart2D;
import info.monitorenter.gui.chart.Chart2D.ToolTipType;

import org.eclipse.swt.SWT;
import org.eclipse.swt.events.SelectionAdapter;
import org.eclipse.swt.events.SelectionEvent;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Button;
import org.eclipse.swt.widgets.Combo;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Group;
import org.eclipse.swt.widgets.Label;

public class ChartCompositePlotTab extends Group {
	private Button btnLegend;
	private Chart2D chart;
	private Button btnUseantialias;
	@Override
	protected void checkSubclass() {
	}
	public ChartCompositePlotTab(Composite parent, int style,
			PlotConfigurerComposite comp) {
		super(parent, style);
		chart = comp.chart;
		setText("Behaviour");
		this.setLayout(new GridLayout(3, false));
		
				btnLegend = new Button(this, SWT.CHECK);
				btnLegend.setLayoutData(new GridData(SWT.LEFT, SWT.CENTER, false, false, 3, 1));
				btnLegend.addSelectionListener(new SelectionAdapter() {
					@Override
					public void widgetSelected(SelectionEvent e) {
						chart.setPaintLabels(btnLegend.getSelection());
					}
				});
				btnLegend.setText("Show legend");
				
				btnUseantialias = new Button(this, SWT.CHECK);
				btnUseantialias.setLayoutData(new GridData(SWT.LEFT, SWT.CENTER, false, false, 2, 1));
				btnUseantialias.setText("UseAntialias");
				btnUseantialias.addSelectionListener(new SelectionAdapter() {
					@Override
					public void widgetSelected(SelectionEvent e) {
						chart.setUseAntialiasing(btnUseantialias.getSelection());
						chart.setRequestedRepaint(true);
					}
				});
				new Label(this, SWT.NONE);
		
				Label lblMode = new Label(this, SWT.NONE);
				lblMode.setLayoutData(new GridData(SWT.RIGHT, SWT.CENTER, false, false,
						1, 1));
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
		new Label(this, SWT.NONE);
	}

	public void refresh() {
		btnLegend.setSelection(chart.isPaintLabels());
		btnUseantialias.setSelection(chart.isUseAntialiasing());
		
	}
}
