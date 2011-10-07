package org.zaluum.widget.plot;

import info.monitorenter.gui.chart.Chart2D;
import info.monitorenter.gui.chart.Chart2D.ToolTipType;
import info.monitorenter.gui.chart.IAxis;
import info.monitorenter.gui.chart.axis.AAxis;
import info.monitorenter.gui.chart.axis.AxisLog10;
import info.monitorenter.gui.chart.labelformatters.LabelFormatterNumber;
import info.monitorenter.gui.chart.rangepolicies.RangePolicyFixedViewport;
import info.monitorenter.gui.chart.rangepolicies.RangePolicyUnbounded;
import info.monitorenter.util.Range;

import java.awt.BorderLayout;
import java.awt.Frame;
import java.awt.Panel;
import java.text.DecimalFormat;

import org.eclipse.jface.viewers.ArrayContentProvider;
import org.eclipse.jface.viewers.ComboViewer;
import org.eclipse.jface.viewers.ISelectionChangedListener;
import org.eclipse.jface.viewers.IStructuredSelection;
import org.eclipse.jface.viewers.LabelProvider;
import org.eclipse.jface.viewers.SelectionChangedEvent;
import org.eclipse.jface.viewers.StructuredSelection;
import org.eclipse.swt.SWT;
import org.eclipse.swt.awt.SWT_AWT;
import org.eclipse.swt.custom.StyledText;
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
import org.eclipse.swt.widgets.TabFolder;
import org.eclipse.swt.widgets.TabItem;
import org.eclipse.swt.widgets.Text;

public class PlotComposite extends Composite {
	public static class AxisTitleLabelProvider extends LabelProvider {
		public String getText(Object element) {
			return element == null ? "" : ((IAxis) element).getAxisTitle().getTitle();//$NON-NLS-1$
		}
	}

	private Text formatText;
	private Text scaleName;
	private Text scaleMin;
	private Text scaleMax;
	public Chart2D chart;
	private ComboViewer comboViewer;
	private ComboViewer scalesComboViewer;
	private Button scaleShowChk;
	private Button scaleLogChk;
	private Button scaleInvertedChk;
	private Button scaleAutoChk;
	private Button scaleGridNone;
	private Button scaleGridMajor;
	private PlotCompositePlotTab plotComposite;
	private Button btnLegend;

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
	public PlotComposite(Composite parent, int style) {
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
		gd_tabFolder.widthHint = 503;
		tabFolder.setLayoutData(gd_tabFolder);

		TabItem tbtmChart = new TabItem(tabFolder, SWT.NONE);
		tbtmChart.setText("Chart");

		Group grpBehaviour = new Group(tabFolder, SWT.NONE);
		tbtmChart.setControl(grpBehaviour);
		grpBehaviour.setText("Behaviour");
		grpBehaviour.setLayout(new GridLayout(2, false));
		new Label(grpBehaviour, SWT.NONE);
		
		btnLegend = new Button(grpBehaviour, SWT.CHECK);
		btnLegend.addSelectionListener(new SelectionAdapter() {
			@Override
			public void widgetSelected(SelectionEvent e) {
				chart.setPaintLabels(btnLegend.getSelection());
			}
		});
		btnLegend.setText("Show legend");
		new Label(grpBehaviour, SWT.NONE);

		Button btnShowMenu = new Button(grpBehaviour, SWT.CHECK);
		btnShowMenu.setText("Show menu");

		Label lblMode = new Label(grpBehaviour, SWT.NONE);
		lblMode.setLayoutData(new GridData(SWT.RIGHT, SWT.CENTER, false, false,
				1, 1));
		lblMode.setText("Tooltip Mode");

		final Combo tooltipCombo = new Combo(grpBehaviour, SWT.NONE);
		tooltipCombo.addSelectionListener(new SelectionAdapter() {
			@Override
			public void widgetSelected(SelectionEvent e) {
				System.out.println("ok");
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

		TabItem tbtmFormat = new TabItem(tabFolder, SWT.NONE);
		tbtmFormat.setText("Format");

		Composite formatComposite = new Composite(tabFolder, SWT.NONE);
		tbtmFormat.setControl(formatComposite);
		formatComposite.setLayout(new GridLayout(2, false));

		comboViewer = new ComboViewer(formatComposite, SWT.NONE);
		comboViewer.setContentProvider(ArrayContentProvider.getInstance());
		comboViewer.setLabelProvider(new AxisTitleLabelProvider());
		comboViewer
				.addPostSelectionChangedListener(new ISelectionChangedListener() {
					@Override
					public void selectionChanged(SelectionChangedEvent event) {
						updateFormat();
					}
				});

		Combo combo = comboViewer.getCombo();
		combo.select(0);
		combo.setLayoutData(new GridData(SWT.FILL, SWT.CENTER, true, false, 2,
				1));

		Label lblFormatString = new Label(formatComposite, SWT.NONE);
		lblFormatString.setLayoutData(new GridData(SWT.RIGHT, SWT.CENTER,
				false, false, 1, 1));
		lblFormatString.setText("Format String");

		formatText = new Text(formatComposite, SWT.BORDER);
		formatText.addModifyListener(new ModifyListener() {
			public void modifyText(ModifyEvent e) {
				IAxis a = (IAxis) ((IStructuredSelection) comboViewer
						.getSelection()).getFirstElement();
				if (a != null) {
					LabelFormatterNumber number = new LabelFormatterNumber();
					try {
						number.setNumberFormat(new DecimalFormat(formatText
								.getText()));
						a.setFormatter(number);
					} catch (Exception ex) {
					}
				}
			}
		});
		GridData gd_formatText = new GridData(SWT.FILL, SWT.CENTER, true,
				false, 1, 1);
		gd_formatText.widthHint = 149;
		formatText.setLayoutData(gd_formatText);

		StyledText formatCheat = new StyledText(formatComposite, SWT.BORDER);
		formatCheat.setEnabled(false);
		formatCheat.setWrapIndent(1);
		formatCheat
				.setText("Lorem ipsum dolor sit amet, consectetur adipiscing elit.\n\n");
		formatCheat.setEditable(false);
		GridData gd_formatCheat = new GridData(SWT.FILL, SWT.TOP, true, true,
				2, 1);
		gd_formatCheat.widthHint = 312;
		formatCheat.setLayoutData(gd_formatCheat);

		TabItem tbtmScales = new TabItem(tabFolder, SWT.NONE);
		tbtmScales.setText("Scales");

		Composite scalesComposite = new Composite(tabFolder, SWT.NONE);
		tbtmScales.setControl(scalesComposite);
		scalesComposite.setLayout(new GridLayout(1, false));

		scalesComboViewer = new ComboViewer(scalesComposite, SWT.NONE);
		scalesComboViewer
				.setContentProvider(ArrayContentProvider.getInstance());
		scalesComboViewer.setLabelProvider(new AxisTitleLabelProvider());
		scalesComboViewer
				.addPostSelectionChangedListener(new ISelectionChangedListener() {
					@Override
					public void selectionChanged(SelectionChangedEvent event) {
						updateScales();
					}
				});
		Combo scalesCombo = scalesComboViewer.getCombo();
		scalesCombo.select(0);
		scalesCombo.setLayoutData(new GridData(SWT.FILL, SWT.CENTER, true,
				false, 1, 1));

		Composite scalesComposite2 = new Composite(scalesComposite, SWT.NONE);
		scalesComposite2.setLayoutData(new GridData(SWT.FILL, SWT.CENTER, true,
				false, 1, 1));
		scalesComposite2.setLayout(new GridLayout(4, false));

		Label lblName = new Label(scalesComposite2, SWT.NONE);
		lblName.setText("Name");

		scaleName = new Text(scalesComposite2, SWT.BORDER);
		scaleName.addModifyListener(new ModifyListener() {
			public void modifyText(ModifyEvent e) {
				AAxis axis = getAxis(scalesComboViewer);
				axis.getAxisTitle().setTitle(scaleName.getText());
				scalesComboViewer.refresh();
				comboViewer.refresh();
			}
		});
		scaleName.setLayoutData(new GridData(SWT.FILL, SWT.CENTER, true, false,
				3, 1));

		Group scalesCheckComposite = new Group(scalesComposite2, SWT.NONE);
		scalesCheckComposite.setLayoutData(new GridData(SWT.LEFT, SWT.FILL,
				false, false, 2, 1));
		scalesCheckComposite.setLayout(new FillLayout(SWT.VERTICAL));

		scaleShowChk = new Button(scalesCheckComposite, SWT.CHECK);
		scaleShowChk.addSelectionListener(new SelectionAdapter() {
			@Override
			public void widgetSelected(SelectionEvent e) {
				getAxis(scalesComboViewer).setVisible(
						scaleShowChk.getSelection());
				chart.setRequestedRepaint(true);
			}
		});
		scaleShowChk.setText("Show scale");

		scaleLogChk = new Button(scalesCheckComposite, SWT.CHECK);
		scaleLogChk.setEnabled(false);
		scaleLogChk.setText("Log");

		scaleInvertedChk = new Button(scalesCheckComposite, SWT.CHECK);
		scaleInvertedChk.setEnabled(false);
		scaleInvertedChk.setText("Inverted");
		new Label(scalesComposite2, SWT.NONE);

		Group groupAuto = new Group(scalesComposite2, SWT.NONE);
		groupAuto.setLayoutData(new GridData(SWT.FILL, SWT.FILL, false, false,
				1, 1));
		groupAuto.setLayout(new GridLayout(2, false));

		scaleAutoChk = new Button(groupAuto, SWT.CHECK);
		scaleAutoChk.addSelectionListener(new SelectionAdapter() {
			@Override
			public void widgetSelected(SelectionEvent e) {
				syncAuto();
				updateScales();
			}

		});
		scaleAutoChk.setLayoutData(new GridData(SWT.LEFT, SWT.CENTER, false,
				false, 2, 1));
		scaleAutoChk.setText("Auto scale");

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

		Group grpGrid = new Group(scalesComposite2, SWT.NONE);
		grpGrid.setLayoutData(new GridData(SWT.FILL, SWT.CENTER, false, false,
				4, 1));
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

		TabItem tbtmPlot = new TabItem(tabFolder, SWT.NONE);
		tbtmPlot.setText("Plot");

		plotComposite = new PlotCompositePlotTab(tabFolder, SWT.NONE,chart);
		tbtmPlot.setControl(plotComposite);

	}

	protected void syncGrid() {
		AAxis a = getAxis(scalesComboViewer);
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
			return Double.parseDouble(scaleMax.getText());
		} catch (NumberFormatException e) {
			return or;
		}
	}

	public void refresh() {
		plotComposite.refresh();
		comboViewer.setInput(chart.getAxes().toArray());
		scalesComboViewer.setInput(chart.getAxes().toArray());
		StructuredSelection sel = new StructuredSelection(chart.getAxes()
				.get(0));
		if (comboViewer.getCombo().getSelectionIndex() == -1)
			comboViewer.setSelection(sel);
		if (scalesComboViewer.getCombo().getSelectionIndex() == -1)
			scalesComboViewer.setSelection(sel);
		btnLegend.setSelection(chart.isPaintLabels());
		updateFormat();
		updateScales();

	}

	protected AAxis getAxis(ComboViewer viewer) {
		return (AAxis) ((IStructuredSelection) viewer.getSelection())
				.getFirstElement();
	}

	private void syncAuto() {
		AAxis axis = getAxis(scalesComboViewer);
		if (scaleAutoChk.getSelection()) {
			axis.setRangePolicy(new RangePolicyUnbounded());
		} else {
			axis.setRangePolicy(new RangePolicyFixedViewport(new Range(0,// parseOr(scaleMin.getText(),0),
					parseOr(scaleMax.getText(), 1))));
		}
		chart.setRequestedRepaint(true);
	}

	protected void updateScales() {
		AAxis axis = getAxis(scalesComboViewer);
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

	}

	private void updateFormat() {
		AAxis a = (AAxis) ((IStructuredSelection) comboViewer.getSelection())
				.getFirstElement();
		formatText.setText(a == null ? "" : a.getFormatter().toString());
	}

	@Override
	protected void checkSubclass() {
	}

}
