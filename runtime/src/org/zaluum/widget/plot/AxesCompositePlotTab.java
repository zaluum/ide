package org.zaluum.widget.plot;

import info.monitorenter.gui.chart.Chart2D;
import info.monitorenter.gui.chart.IAxis;
import info.monitorenter.gui.chart.IAxis.AxisTitle;
import info.monitorenter.gui.chart.IAxisLabelFormatter;
import info.monitorenter.gui.chart.axis.AAxis;
import info.monitorenter.gui.chart.axis.AxisLinear;
import info.monitorenter.gui.chart.axis.AxisLog10;
import info.monitorenter.gui.chart.labelformatters.LabelFormatterNumber;
import info.monitorenter.gui.chart.rangepolicies.RangePolicyFixedViewport;
import info.monitorenter.gui.chart.rangepolicies.RangePolicyUnbounded;
import info.monitorenter.util.Range;

import java.text.DecimalFormat;

import org.eclipse.jface.viewers.ArrayContentProvider;
import org.eclipse.jface.viewers.ComboViewer;
import org.eclipse.jface.viewers.ISelectionChangedListener;
import org.eclipse.jface.viewers.IStructuredSelection;
import org.eclipse.jface.viewers.SelectionChangedEvent;
import org.eclipse.jface.viewers.StructuredSelection;
import org.eclipse.swt.SWT;
import org.eclipse.swt.events.ModifyEvent;
import org.eclipse.swt.events.ModifyListener;
import org.eclipse.swt.events.SelectionAdapter;
import org.eclipse.swt.events.SelectionEvent;
import org.eclipse.swt.layout.FillLayout;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Button;
import org.eclipse.swt.widgets.Combo;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Group;
import org.eclipse.swt.widgets.Label;
import org.eclipse.swt.widgets.Text;

public class AxesCompositePlotTab extends Composite {
	private static final String[] Positions = new String[] { "Top", "Bottom",
			"Left", "Right" };
	private Chart2D chart;
	private ComboViewer scalesComboViewer;
	private Composite scalesContents;
	private Text scaleName;
	private Button scaleShowChk;
	private Button scaleLogChk;
	private Button scaleAutoChk;
	private Text scaleMin;
	private Text scaleMax;
	private Button scaleGridNone;
	private Button scaleGridMajor;
	private Combo scalesPos;

	private Text scaleFormat;

	public AxesCompositePlotTab(Composite parent, int style,
			final PlotConfigurerComposite plotc) {
		super(parent, style);
		chart = plotc.chart;

		this.setLayout(new GridLayout(3, false));

		scalesComboViewer = new ComboViewer(this, SWT.READ_ONLY);
		scalesComboViewer
				.setContentProvider(ArrayContentProvider.getInstance());
		scalesComboViewer.setLabelProvider(new AxisTitleLabelProvider());
		scalesComboViewer
				.addPostSelectionChangedListener(new ISelectionChangedListener() {
					@Override
					public void selectionChanged(SelectionChangedEvent event) {
						refreshScales();
					}
				});
		Combo scalesCombo = scalesComboViewer.getCombo();
		scalesCombo.select(0);
		scalesCombo.setLayoutData(new GridData(SWT.FILL, SWT.CENTER, true,
				false, 1, 1));

		Button btnNew = new Button(this, SWT.NONE);
		btnNew.addSelectionListener(new SelectionAdapter() {
			@Override
			public void widgetSelected(SelectionEvent e) {
				AxisLinear axisLinear = new AxisLinear();
				axisLinear.setAxisTitle(new AxisTitle("new axis"));
				chart.addAxisXBottom(axisLinear);
				refresh(axisLinear);
			}
		});
		btnNew.setText("New");

		Button btnDelete = new Button(this, SWT.NONE);
		btnDelete.addSelectionListener(new SelectionAdapter() {
			@Override
			public void widgetSelected(SelectionEvent e) {
				AAxis axis = getAxis();
				if (axis != null) {
					chart.removeAxisXBottom(axis);
					chart.removeAxisXTop(axis);
					chart.removeAxisYLeft(axis);
					chart.removeAxisYRight(axis);
				}
				refresh();
			}
		});
		btnDelete.setText("Delete");

		scalesContents = new Composite(this, SWT.NONE);
		scalesContents.setLayoutData(new GridData(SWT.FILL, SWT.CENTER, true,
				false, 3, 1));
		scalesContents.setLayout(new GridLayout(3, false));

		scaleName = new Text(scalesContents, SWT.BORDER);
		scaleName.addModifyListener(new ModifyListener() {
			public void modifyText(ModifyEvent e) {
				AAxis axis = getAxis();
				axis.getAxisTitle().setTitle(scaleName.getText());
				scalesComboViewer.refresh();
				plotc.traceC.refresh();
			}
		});
		scaleName.setLayoutData(new GridData(SWT.FILL, SWT.CENTER, true, false,
				3, 1));

		Group scalesCheckComposite = new Group(scalesContents, SWT.NONE);
		scalesCheckComposite.setLayoutData(new GridData(SWT.LEFT, SWT.FILL,
				false, false, 1, 1));
		scalesCheckComposite.setLayout(new FillLayout(SWT.VERTICAL));

		scaleShowChk = new Button(scalesCheckComposite, SWT.CHECK);
		scaleShowChk.addSelectionListener(new SelectionAdapter() {
			@Override
			public void widgetSelected(SelectionEvent e) {
				getAxis().setVisible(scaleShowChk.getSelection());
				chart.setRequestedRepaint(true);
			}
		});
		scaleShowChk.setText("Show scale");

		scaleLogChk = new Button(scalesCheckComposite, SWT.CHECK);
		scaleLogChk.addSelectionListener(new SelectionAdapter() {
			@Override
			public void widgetSelected(SelectionEvent e) {
				syncLog();
			}
		});
		scaleLogChk.setText("Log");
		Group groupAuto = new Group(scalesContents, SWT.NONE);
		groupAuto.setLayoutData(new GridData(SWT.FILL, SWT.FILL, false, false,
				2, 1));
		groupAuto.setLayout(new GridLayout(2, false));

		scaleAutoChk = new Button(groupAuto, SWT.CHECK);
		scaleAutoChk.addSelectionListener(new SelectionAdapter() {
			@Override
			public void widgetSelected(SelectionEvent e) {
				syncAuto();
				refreshScales();
			}

		});
		scaleAutoChk.setLayoutData(new GridData(SWT.LEFT, SWT.CENTER, false,
				false, 2, 1));
		scaleAutoChk.setText("Auto scale");
		
		
		Label lblMax = new Label(groupAuto, SWT.NONE);
		lblMax.setLayoutData(new GridData(SWT.RIGHT, SWT.CENTER, false, false,
				1, 1));
		lblMax.setText("Max");

		scaleMax = new Text(groupAuto, SWT.BORDER);
		scaleMax.addModifyListener(new ModifyListener() {
			public void modifyText(ModifyEvent e) {
				syncAuto();
			}
		});

		scaleMax.setLayoutData(new GridData(SWT.FILL, SWT.CENTER, true, false,
				1, 1));

		Label lblMin = new Label(groupAuto, SWT.NONE);
		lblMin.setLayoutData(new GridData(SWT.RIGHT, SWT.CENTER, false, false,
				1, 1));
		lblMin.setText("Min");

		scaleMin = new Text(groupAuto, SWT.BORDER);
		scaleMin.addModifyListener(new ModifyListener() {
			public void modifyText(ModifyEvent e) {
				syncAuto();
			}
		});
		scaleMin.setLayoutData(new GridData(SWT.FILL, SWT.CENTER, true, false,
				1, 1));

		Group grpGrid = new Group(scalesContents, SWT.NONE);
		grpGrid.setLayoutData(new GridData(SWT.FILL, SWT.CENTER, false, false,
				1, 1));
		grpGrid.setText("Grid");
		grpGrid.setLayout(new GridLayout(1, false));
		SelectionAdapter gridListener = new SelectionAdapter() {
			@Override
			public void widgetSelected(SelectionEvent e) {
				syncGrid();
			}
		};
		scaleGridNone = new Button(grpGrid, SWT.RADIO);
		scaleGridNone.addSelectionListener(gridListener);
		scaleGridNone.setText("None");

		scaleGridMajor = new Button(grpGrid, SWT.RADIO);
		scaleGridMajor.addSelectionListener(gridListener);
		scaleGridMajor.setText("Major ticks");
		new Label(scalesContents, SWT.NONE);

		Group grpPosition = new Group(scalesContents, SWT.NONE);
		grpPosition.setLayout(new GridLayout(1, false));
		grpPosition.setLayoutData(new GridData(SWT.FILL, SWT.FILL, true, false,
				1, 1));
		grpPosition.setText("Position");

		scalesPos = new Combo(grpPosition, SWT.READ_ONLY);
		scalesPos.setItems(Positions);
		scalesPos.addSelectionListener(new SelectionAdapter() {
			@Override
			public void widgetSelected(SelectionEvent e) {
				int sel = scalesPos.getSelectionIndex();
				AAxis axis = getAxis();
				chart.removeAxisXBottom(axis);
				chart.removeAxisXTop(axis);
				chart.removeAxisYLeft(axis);
				chart.removeAxisYRight(axis);
				if (sel == 0) // TOP
					chart.addAxisXTop(axis);
				else if (sel == 1) // BOTTOM
					chart.addAxisXBottom(axis);
				else if (sel == 2) // LEFT
					chart.addAxisYLeft(axis);
				else if (sel == 3) // RIGHT
					chart.addAxisYRight(axis);
				refresh(axis);
			}
		});
		scalesPos.setLayoutData(new GridData(SWT.FILL, SWT.CENTER, true, false,
				1, 1));

		Group grpFormat = new Group(scalesContents, SWT.NONE);
		grpFormat.setLayoutData(new GridData(SWT.FILL, SWT.CENTER, false,
				false, 5, 1));
		grpFormat.setLayout(new GridLayout(2, false));
		grpFormat.setText("Format");
		new Label(grpFormat, SWT.NONE);

		scaleFormat = new Text(grpFormat, SWT.BORDER);
		scaleFormat.addModifyListener(new ModifyListener() {
			@Override
			public void modifyText(ModifyEvent e) {
				syncFormat();
			}
		});
		scaleFormat.setLayoutData(new GridData(SWT.FILL, SWT.CENTER, true,
				false, 1, 1));

	}

	protected void syncFormat() {
		AAxis a = getAxis();
		if (a.getFormatter() instanceof LabelFormatterNumber) {
			LabelFormatterNumber n = (LabelFormatterNumber) a.getFormatter();
			n.setNumberFormat(new DecimalFormat(scaleFormat.getText()));
		}
	}

	protected void syncLog() {
		AAxis a = getAxis();
		AAxis newAxis;
		String oldTitle = a.getAxisTitle().getTitle();
		if (scaleLogChk.getSelection())
			newAxis = new AxisLog10();
		else
			newAxis = new AxisLinear();
		int ibottom = chart.getAxesXBottom().indexOf(a);
		if (ibottom != -1) {
			chart.setAxisXBottom(newAxis, ibottom);
		} else {
			int ileft = chart.getAxesYLeft().indexOf(a);
			if (ileft != -1)
				chart.setAxisYLeft(newAxis, ileft);
		}
		if (oldTitle != null)
			newAxis.setAxisTitle(new AxisTitle(oldTitle));
		newAxis.setPaintGrid(a.isPaintGrid());
		newAxis.setVisible(a.isVisible());
		newAxis.setPaintGrid(a.isPaintGrid());
		refresh(newAxis);
	}

	protected void syncGrid() {
		AAxis a = getAxis();
		if (scaleGridNone.getSelection())
			a.setPaintGrid(false);
		else if (scaleGridMajor.getSelection()) {
			a.setPaintGrid(true);
			a.setMinorTickSpacing(5);
		} else {
			a.setPaintGrid(true);
			a.setMinorTickSpacing(10);
		}
	}

	protected double parseOr(String text, double or) {
		try {
			return Double.parseDouble(text);
		} catch (NumberFormatException e) {
			return or;
		}
	}

	protected AAxis getAxis() {
		IStructuredSelection selection = (IStructuredSelection) scalesComboViewer
				.getSelection();
		if (selection.isEmpty())
			return null;
		else
			return (AAxis) selection.getFirstElement();
	}

	private void syncAuto() {
		AAxis axis = getAxis();
		if (scaleAutoChk.getSelection()) {
			axis.setRangePolicy(new RangePolicyUnbounded());
		} else {
			double min = parseOr(scaleMin.getText(),0);
			double max = parseOr(scaleMax.getText(), 1);			
			System.out.println("min = " + min + " max = " + max);
			axis.setRangePolicy(new RangePolicyFixedViewport(new Range(min,
					max)));
		}
		chart.setRequestedRepaint(true);
	}

	public void refresh() {
		if (chart.getAxes().size() == 0)
			refresh(null);
		else
			refresh(chart.getAxes().get(0));
	}

	private void refresh(IAxis iAxis) {
		scalesComboViewer.setInput(chart.getAxes().toArray());
		if (iAxis == null) {
			PlotConfigurerComposite.setAllEnabled(scalesContents, false);

		} else {
			PlotConfigurerComposite.setAllEnabled(scalesContents, true);
			StructuredSelection sel = new StructuredSelection(iAxis);
			if (scalesComboViewer.getCombo().getSelectionIndex() == -1)
				scalesComboViewer.setSelection(sel);
			refreshScales();
		}
	}

	protected void refreshScales() {
		AAxis axis = getAxis();
		if (axis != null) {
			Range range = axis.getRangePolicy().getRange();
			scaleLogChk.setSelection(axis instanceof AxisLog10);
			boolean auto = axis.getRangePolicy() instanceof RangePolicyUnbounded;
			scaleAutoChk.setSelection(auto);
			scaleMin.setEnabled(!auto);
			scaleMax.setEnabled(!auto);
			if (!auto) {
				scaleMax.setText("" + range.getMax());
				scaleMin.setText("" + range.getMin());
			}
			scaleName.setText(axis.getAxisTitle() == null ? "" : axis
					.getAxisTitle().getTitle());
			scaleShowChk.setSelection(axis.isVisible());
			scaleGridMajor.setSelection(axis.isPaintGrid());
			scaleGridNone.setSelection(!axis.isPaintGrid());
			IAxisLabelFormatter formatter = axis.getFormatter();
			if (formatter instanceof LabelFormatterDecimal) {
				LabelFormatterDecimal d = (LabelFormatterDecimal) axis
						.getFormatter();
				scaleFormat.setText(d.toPattern());
			}
			int pos = axis.getAxisPosition();
			if (pos == Chart2D.CHART_POSITION_TOP)
				scalesPos.select(0);
			else if (pos == Chart2D.CHART_POSITION_BOTTOM)
				scalesPos.select(1);
			else if (pos == Chart2D.CHART_POSITION_LEFT)
				scalesPos.select(2);
			else
				scalesPos.select(3);
		}
	}
}
