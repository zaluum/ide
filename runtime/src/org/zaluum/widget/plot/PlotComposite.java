package org.zaluum.widget.plot;

import info.monitorenter.gui.chart.Chart2D;

import java.awt.BorderLayout;
import java.awt.Frame;
import java.awt.Panel;

import org.eclipse.jface.viewers.ListViewer;
import org.eclipse.swt.SWT;
import org.eclipse.swt.awt.SWT_AWT;
import org.eclipse.swt.custom.StyledText;
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
import org.eclipse.swt.widgets.List;
import org.eclipse.swt.widgets.TabFolder;
import org.eclipse.swt.widgets.TabItem;
import org.eclipse.swt.widgets.Text;

public class PlotComposite extends Composite {
	private Text formatText;
	private Text previewText;
	private Text numberText;
	private Text scaleName;
	private Text scaleMin;
	private Text scaleMax;
	private Combo plotXCombo;
	private Chart2D chart;
	
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
	 * @param parent
	 * @param style
	 */
	public PlotComposite(Composite parent, int style) {
		super(parent, style);
		setLayout(new GridLayout(1, false));
		
		Composite previewComposite = new Composite(this, SWT.NONE);
		GridData gd_previewComposite = new GridData(SWT.FILL, SWT.FILL, true, true, 1, 1);
		gd_previewComposite.heightHint = 205;
		previewComposite.setLayoutData(gd_previewComposite);
		previewComposite.setLayout(new GridLayout(1, false));
		
		Composite previewChart = new Composite(previewComposite, SWT.NO_BACKGROUND | SWT.EMBEDDED);
		GridData gd_previewChart = new GridData(SWT.FILL, SWT.FILL, true, true, 1, 1);
		gd_previewChart.widthHint = 252;
		gd_previewChart.heightHint = 222;
		previewChart.setLayoutData(gd_previewChart);
		createAWTChart2d(previewChart);
		
		TabFolder tabFolder = new TabFolder(this, SWT.NONE);
		GridData gd_tabFolder = new GridData(SWT.FILL, SWT.CENTER, true, false, 1, 1);
		gd_tabFolder.widthHint = 503;
		tabFolder.setLayoutData(gd_tabFolder);
		
		TabItem tbtmChart = new TabItem(tabFolder, SWT.NONE);
		tbtmChart.setText("Chart");
		
		Group grpBehaviour = new Group(tabFolder, SWT.NONE);
		tbtmChart.setControl(grpBehaviour);
		grpBehaviour.setText("Behaviour");
		grpBehaviour.setLayout(new GridLayout(2, false));
		new Label(grpBehaviour, SWT.NONE);
		
		Button btnShowMenu = new Button(grpBehaviour, SWT.CHECK);
		btnShowMenu.setText("Show menu");
		
		Label lblMode = new Label(grpBehaviour, SWT.NONE);
		lblMode.setLayoutData(new GridData(SWT.RIGHT, SWT.CENTER, false, false, 1, 1));
		lblMode.setText("Mode");
		
		Combo combo_5 = new Combo(grpBehaviour, SWT.NONE);
		combo_5.setItems(new String[] {"strip", "scope", "sweep"});
		combo_5.setLayoutData(new GridData(SWT.FILL, SWT.CENTER, true, false, 1, 1));
		
		TabItem tbtmFormat = new TabItem(tabFolder, SWT.NONE);
		tbtmFormat.setText("Format");
		
		Composite formatComposite = new Composite(tabFolder, SWT.NONE);
		tbtmFormat.setControl(formatComposite);
		formatComposite.setLayout(new GridLayout(2, false));
		
		Combo combo = new Combo(formatComposite, SWT.NONE);
		combo.setLayoutData(new GridData(SWT.FILL, SWT.CENTER, true, false, 2, 1));
		
		Label lblFormatString = new Label(formatComposite, SWT.NONE);
		lblFormatString.setLayoutData(new GridData(SWT.RIGHT, SWT.CENTER, false, false, 1, 1));
		lblFormatString.setText("Format String");
		
		formatText = new Text(formatComposite, SWT.BORDER);
		formatText.addSelectionListener(new SelectionAdapter() {
			@Override
			public void widgetSelected(SelectionEvent e) {
				
			}
		});
		formatText.setText("%s");
		GridData gd_formatText = new GridData(SWT.FILL, SWT.CENTER, true, false, 1, 1);
		gd_formatText.widthHint = 149;
		formatText.setLayoutData(gd_formatText);
		
		Group grpPreview = new Group(formatComposite, SWT.NONE);
		grpPreview.setLayoutData(new GridData(SWT.FILL, SWT.CENTER, false, false, 2, 1));
		grpPreview.setText("Preview");
		grpPreview.setLayout(new GridLayout(2, false));
		
		Label lblPreview = new Label(grpPreview, SWT.NONE);
		lblPreview.setText("Preview");
		
		previewText = new Text(grpPreview, SWT.BORDER);
		GridData gd_previewText = new GridData(SWT.FILL, SWT.CENTER, true, false, 1, 1);
		gd_previewText.widthHint = 119;
		previewText.setLayoutData(gd_previewText);
		previewText.setEnabled(false);
		previewText.setText("1243.0");
		
		Label lblTestNumber = new Label(grpPreview, SWT.NONE);
		lblTestNumber.setText("Test number");
		
		numberText = new Text(grpPreview, SWT.BORDER);
		numberText.setText("1234");
		GridData gd_numberText = new GridData(SWT.FILL, SWT.CENTER, false, false, 1, 1);
		gd_numberText.widthHint = 133;
		numberText.setLayoutData(gd_numberText);
		
		StyledText formatCheat = new StyledText(formatComposite, SWT.BORDER);
		formatCheat.setEnabled(false);
		formatCheat.setWrapIndent(1);
		formatCheat.setText("Lorem ipsum dolor sit amet, consectetur adipiscing elit.\n\n");
		formatCheat.setEditable(false);
		GridData gd_formatCheat = new GridData(SWT.FILL, SWT.TOP, true, true, 2, 1);
		gd_formatCheat.widthHint = 312;
		formatCheat.setLayoutData(gd_formatCheat);
		
		TabItem tbtmScales = new TabItem(tabFolder, SWT.NONE);
		tbtmScales.setText("Scales");
		
		Composite scalesComposite = new Composite(tabFolder, SWT.NONE);
		tbtmScales.setControl(scalesComposite);
		scalesComposite.setLayout(new GridLayout(1, false));
		
		Combo scaleCombo = new Combo(scalesComposite, SWT.NONE);
		scaleCombo.setLayoutData(new GridData(SWT.FILL, SWT.CENTER, true, false, 1, 1));
		
		Composite scalesComposite2 = new Composite(scalesComposite, SWT.NONE);
		scalesComposite2.setLayoutData(new GridData(SWT.FILL, SWT.CENTER, true, false, 1, 1));
		scalesComposite2.setLayout(new GridLayout(4, false));
		
		Label lblName = new Label(scalesComposite2, SWT.NONE);
		lblName.setText("Name");
		
		scaleName = new Text(scalesComposite2, SWT.BORDER);
		scaleName.setLayoutData(new GridData(SWT.FILL, SWT.CENTER, true, false, 3, 1));
		
		Group scalesCheckComposite = new Group(scalesComposite2, SWT.NONE);
		scalesCheckComposite.setLayoutData(new GridData(SWT.LEFT, SWT.FILL, false, false, 2, 1));
		scalesCheckComposite.setLayout(new FillLayout(SWT.VERTICAL));
		
		Button scaleLabelChk = new Button(scalesCheckComposite, SWT.CHECK);
		scaleLabelChk.setText("Show label");
		
		Button scaleShowChk = new Button(scalesCheckComposite, SWT.CHECK);
		scaleShowChk.setText("Show scale");
		
		Button scaleLogChk = new Button(scalesCheckComposite, SWT.CHECK);
		scaleLogChk.setText("Log");
		
		Button scaleInvertedChk = new Button(scalesCheckComposite, SWT.CHECK);
		scaleInvertedChk.setText("Inverted");
		new Label(scalesComposite2, SWT.NONE);
		
		Group groupAuto = new Group(scalesComposite2, SWT.NONE);
		groupAuto.setLayoutData(new GridData(SWT.FILL, SWT.FILL, false, false, 1, 1));
		groupAuto.setLayout(new GridLayout(2, false));
		
		Button scaleAutoChk = new Button(groupAuto, SWT.CHECK);
		scaleAutoChk.setLayoutData(new GridData(SWT.LEFT, SWT.CENTER, false, false, 2, 1));
		scaleAutoChk.setText("Auto scale");
		
		Label lblMin = new Label(groupAuto, SWT.NONE);
		lblMin.setLayoutData(new GridData(SWT.RIGHT, SWT.CENTER, false, false, 1, 1));
		lblMin.setText("Min");
		
		scaleMin = new Text(groupAuto, SWT.BORDER);
		scaleMin.setLayoutData(new GridData(SWT.FILL, SWT.CENTER, true, false, 1, 1));
		
		Label lblMax = new Label(groupAuto, SWT.NONE);
		lblMax.setLayoutData(new GridData(SWT.RIGHT, SWT.CENTER, false, false, 1, 1));
		lblMax.setText("Max");
		
		scaleMax = new Text(groupAuto, SWT.BORDER);
		scaleMax.setLayoutData(new GridData(SWT.FILL, SWT.CENTER, true, false, 1, 1));
		
		Group grpGrid = new Group(scalesComposite2, SWT.NONE);
		grpGrid.setLayoutData(new GridData(SWT.FILL, SWT.CENTER, false, false, 4, 1));
		grpGrid.setText("Grid");
		grpGrid.setLayout(new GridLayout(1, false));
		
		Button scaleGridNone = new Button(grpGrid, SWT.RADIO);
		scaleGridNone.setText("None");
		
		Button scaleGridMajor = new Button(grpGrid, SWT.RADIO);
		scaleGridMajor.setText("Major ticks");
		
		Button scaleGridMinor = new Button(grpGrid, SWT.RADIO);
		scaleGridMinor.setText("Major and minor ticks");
		
		TabItem tbtmPlot = new TabItem(tabFolder, SWT.NONE);
		tbtmPlot.setText("Plot");
		
		Composite plotComposite = new Composite(tabFolder, SWT.NONE);
		tbtmPlot.setControl(plotComposite);
		plotComposite.setLayout(new GridLayout(3, false));
		
		Combo plotCombo = new Combo(plotComposite, SWT.NONE);
		plotCombo.setLayoutData(new GridData(SWT.FILL, SWT.CENTER, true, false, 3, 1));
		new Label(plotComposite, SWT.NONE);
		
		Group grpAxis = new Group(plotComposite, SWT.NONE);
		grpAxis.setLayoutData(new GridData(SWT.FILL, SWT.CENTER, false, false, 2, 1));
		grpAxis.setText("Axis");
		grpAxis.setLayout(new GridLayout(2, false));
		
		Label lblXAxis = new Label(grpAxis, SWT.NONE);
		lblXAxis.setLayoutData(new GridData(SWT.RIGHT, SWT.CENTER, false, false, 1, 1));
		lblXAxis.setText("X Axis");
		
		plotXCombo = new Combo(grpAxis, SWT.BORDER);
		plotXCombo.setLayoutData(new GridData(SWT.FILL, SWT.CENTER, true, false, 1, 1));
		
		Label lblYAxis = new Label(grpAxis, SWT.NONE);
		lblYAxis.setLayoutData(new GridData(SWT.RIGHT, SWT.CENTER, false, false, 1, 1));
		lblYAxis.setText("Y Axis");
		
		Combo plotYCombo = new Combo(grpAxis, SWT.NONE);
		plotYCombo.setLayoutData(new GridData(SWT.FILL, SWT.CENTER, true, false, 1, 1));
		new Label(plotComposite, SWT.NONE);
		
		Group grpStyle = new Group(plotComposite, SWT.NONE);
		grpStyle.setLayoutData(new GridData(SWT.FILL, SWT.FILL, false, true, 1, 2));
		grpStyle.setText("Style");
		grpStyle.setLayout(new GridLayout(3, false));
		
		ListViewer listViewer = new ListViewer(grpStyle, SWT.BORDER | SWT.V_SCROLL);
		List plotSizeList = listViewer.getList();
		plotSizeList.setLayoutData(new GridData(SWT.LEFT, SWT.FILL, false, false, 1, 2));
		plotSizeList.setItems(new String[] {"a", "b", "c", "d"});
		
		ListViewer listViewer_1 = new ListViewer(grpStyle, SWT.BORDER | SWT.V_SCROLL);
		List plotPointList = listViewer_1.getList();
		plotPointList.setLayoutData(new GridData(SWT.LEFT, SWT.FILL, false, false, 1, 2));
		plotPointList.setItems(new String[] {"dot", "square", "circle", "cross"});
		
		ListViewer listViewer_2 = new ListViewer(grpStyle, SWT.BORDER | SWT.V_SCROLL);
		List plotLinesList = listViewer_2.getList();
		plotLinesList.setLayoutData(new GridData(SWT.LEFT, SWT.FILL, false, false, 1, 2));
		plotLinesList.setItems(new String[] {"points", "lines", "ortogonal"});
		
		Group grpColor = new Group(plotComposite, SWT.NONE);
		grpColor.setLayoutData(new GridData(SWT.FILL, SWT.FILL, true, false, 1, 1));
		grpColor.setLayout(new GridLayout(2, false));
		grpColor.setText("Color");
		
		Button btnC = new Button(grpColor, SWT.NONE);
		btnC.setText("c");
		
		Label lblLine = new Label(grpColor, SWT.NONE);
		lblLine.setText("Line");
		
		Button btnC_1 = new Button(grpColor, SWT.NONE);
		btnC_1.setText("c");
		
		Label lblPointsAndFill = new Label(grpColor, SWT.NONE);
		lblPointsAndFill.setText("Points and fill");
		new Label(plotComposite, SWT.NONE);
		
		Group grpFill = new Group(plotComposite, SWT.NONE);
		grpFill.setLayoutData(new GridData(SWT.FILL, SWT.FILL, false, false, 1, 1));
		grpFill.setText("Fill");
		grpFill.setLayout(new GridLayout(1, false));
		
		Combo plotFillCombo = new Combo(grpFill, SWT.NONE);

	}

	@Override
	protected void checkSubclass() {
		// Disable the check that prevents subclassing of SWT components
	}
}
