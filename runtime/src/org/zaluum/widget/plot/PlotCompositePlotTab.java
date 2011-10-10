package org.zaluum.widget.plot;

import info.monitorenter.gui.chart.Chart2D;
import info.monitorenter.gui.chart.IAxis;
import info.monitorenter.gui.chart.ITrace2D;
import info.monitorenter.gui.chart.ITracePainter;
import info.monitorenter.gui.chart.traces.painters.TracePainterDisc;
import info.monitorenter.gui.chart.traces.painters.TracePainterFill;
import info.monitorenter.gui.chart.traces.painters.TracePainterLine;
import info.monitorenter.gui.chart.traces.painters.TracePainterVerticalBar;

import java.awt.BasicStroke;
import java.awt.Color;
import java.util.ArrayList;

import org.eclipse.jface.viewers.ArrayContentProvider;
import org.eclipse.jface.viewers.ComboViewer;
import org.eclipse.jface.viewers.ISelectionChangedListener;
import org.eclipse.jface.viewers.IStructuredSelection;
import org.eclipse.jface.viewers.LabelProvider;
import org.eclipse.jface.viewers.SelectionChangedEvent;
import org.eclipse.jface.viewers.StructuredSelection;
import org.eclipse.swt.SWT;
import org.eclipse.swt.events.ModifyEvent;
import org.eclipse.swt.events.ModifyListener;
import org.eclipse.swt.events.MouseAdapter;
import org.eclipse.swt.events.MouseEvent;
import org.eclipse.swt.events.SelectionAdapter;
import org.eclipse.swt.events.SelectionEvent;
import org.eclipse.swt.graphics.GC;
import org.eclipse.swt.graphics.Image;
import org.eclipse.swt.graphics.RGB;
import org.eclipse.swt.graphics.Rectangle;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.ColorDialog;
import org.eclipse.swt.widgets.Combo;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Group;
import org.eclipse.swt.widgets.Label;
import org.eclipse.swt.widgets.List;
import org.eclipse.swt.widgets.Spinner;
import org.eclipse.swt.widgets.Text;
import org.eclipse.wb.swt.SWTResourceManager;

public class PlotCompositePlotTab extends Composite {
	private final Chart2D chart;
	private ComboViewer comboViewer;
	private ComboViewer comboViewerX;
	private ComboViewer comboViewerY;
	private Combo plotFillCombo;
	private Text name;
	private Composite lineColor;
	private Spinner widthSpinner;
	private List stroke;

	public static float[] dot_dash = new float[] { 10f, 3f, 1f, 3f };

	public static float[] dot(float width) {
		float d = Math.max(1, width /2.0f);
		return new float[] { d, 3f *d };
	}

	public static float[] dash(float width) {
		float d = Math.max(10, 10 * (width /5.0f));
		return new float[] { d,d};
	}

	public static float[] dot_dash(float width) {
		float d = Math.max(1, width /2.0f);
		return new float[] { 10f * d, 3f * d, d, 3f * d };
	}

	public PlotCompositePlotTab(Composite parent, int style, final Chart2D chart) {
		super(parent, style);
		this.chart = chart;
		this.setLayout(new GridLayout(2, false));

		comboViewer = new ComboViewer(this, SWT.NONE);
		comboViewer.setContentProvider(ArrayContentProvider.getInstance());
		comboViewer.setLabelProvider(new LabelProvider() {
			@Override
			public String getText(Object element) {
				return ((ITrace2D) element).getName();
			}
		});
		comboViewer
				.addPostSelectionChangedListener(new ISelectionChangedListener() {
					@Override
					public void selectionChanged(SelectionChangedEvent event) {
						refreshPlot();
					}
				});
		Combo combo = comboViewer.getCombo();
		combo.setLayoutData(new GridData(SWT.FILL, SWT.CENTER, true, false, 2,
				1));

		Composite composite = new Composite(this, SWT.NONE);
		composite.setLayoutData(new GridData(SWT.FILL, SWT.TOP, true, false, 2,
				1));
		composite.setLayout(new GridLayout(2, false));

		Label lblName = new Label(composite, SWT.NONE);
		lblName.setLayoutData(new GridData(SWT.RIGHT, SWT.CENTER, false, false,
				1, 1));
		lblName.setText("Name");

		name = new Text(composite, SWT.BORDER);
		name.addModifyListener(new ModifyListener() {
			@Override
			public void modifyText(ModifyEvent e) {
				getTrace().setName(name.getText());
				comboViewer.refresh();
			}
		});
		name.setLayoutData(new GridData(SWT.FILL, SWT.CENTER, true, false, 1, 1));

		Group grpAxis = new Group(this, SWT.NONE);
		grpAxis.setLayoutData(new GridData(SWT.FILL, SWT.CENTER, false, false,
				2, 1));
		grpAxis.setText("Axis");
		grpAxis.setLayout(new GridLayout(3, false));

		Label lblXAxis = new Label(grpAxis, SWT.NONE);
		lblXAxis.setLayoutData(new GridData(SWT.RIGHT, SWT.CENTER, false,
				false, 1, 1));
		lblXAxis.setText("X Axis");

		comboViewerX = new ComboViewer(grpAxis, SWT.NONE);
		comboViewerX
				.setLabelProvider(new PlotComposite.AxisTitleLabelProvider());
		comboViewerX.setContentProvider(ArrayContentProvider.getInstance());
		Combo comboX = comboViewerX.getCombo();
		comboX.setEnabled(false);
		comboX.setLayoutData(new GridData(SWT.FILL, SWT.CENTER, true, false, 2,
				1));

		Label lblYAxis = new Label(grpAxis, SWT.NONE);
		lblYAxis.setLayoutData(new GridData(SWT.RIGHT, SWT.CENTER, false,
				false, 1, 1));
		lblYAxis.setText("Y Axis");

		comboViewerY = new ComboViewer(grpAxis, SWT.NONE);
		comboViewerY
				.setLabelProvider(new PlotComposite.AxisTitleLabelProvider());
		comboViewerY.setContentProvider(ArrayContentProvider.getInstance());
		Combo comboY = comboViewerY.getCombo();
		comboY.setEnabled(false);
		comboY.setLayoutData(new GridData(SWT.FILL, SWT.CENTER, true, false, 2,
				1));

		Group grpStyle = new Group(this, SWT.NONE);
		grpStyle.setLayoutData(new GridData(SWT.FILL, SWT.FILL, false, false,
				1, 2));
		grpStyle.setText("Style");
		grpStyle.setLayout(new GridLayout(2, false));

		widthSpinner = new Spinner(grpStyle, SWT.BORDER);
		widthSpinner.addModifyListener(new ModifyListener() {

			@Override
			public void modifyText(ModifyEvent e) {
				updateStroke();
			}
		});

		stroke = new List(grpStyle, SWT.BORDER);
		stroke.setItems(new String[] { "solid", "dot", "dash", "dot-dash" });
		stroke.addSelectionListener(new SelectionAdapter() {
			@Override
			public void widgetSelected(SelectionEvent e) {
				updateStroke();
			}
		});

		Group grpColor = new Group(this, SWT.NONE);
		grpColor.setLayoutData(new GridData(SWT.FILL, SWT.FILL, true, false, 1,
				1));
		grpColor.setLayout(new GridLayout(2, false));
		grpColor.setText("Color");

		lineColor = new Composite(grpColor, SWT.BORDER);
		lineColor.addMouseListener(new MouseAdapter() {
			@Override
			public void mouseUp(MouseEvent e) {
				ColorDialog color = new ColorDialog(getShell());
				color.setRGB(lineColor.getBackground().getRGB());
				RGB newColor = color.open();
				if (newColor != null) {
					java.awt.Color awtColor = new java.awt.Color(newColor.red,
							newColor.green, newColor.blue);
					getTrace().setColor(awtColor);
				}
			}
		});
		lineColor.setBackground(SWTResourceManager.getColor(SWT.COLOR_WHITE));
		GridData gd_lineColor = new GridData(SWT.LEFT, SWT.CENTER, false,
				false, 1, 1);
		gd_lineColor.widthHint = 20;
		gd_lineColor.heightHint = 20;
		lineColor.setLayoutData(gd_lineColor);

		Label lblLine = new Label(grpColor, SWT.NONE);
		lblLine.setText("Line");

		Group grpFill = new Group(this, SWT.NONE);
		grpFill.setLayoutData(new GridData(SWT.FILL, SWT.FILL, false, false, 1,
				1));
		grpFill.setText("Fill");
		grpFill.setLayout(new GridLayout(1, false));

		plotFillCombo = new Combo(grpFill, SWT.NONE);
		plotFillCombo.setItems(new String[] { "Line", "Disc", "Vertical bar",
				"Fill" });
		plotFillCombo.addSelectionListener(new SelectionAdapter() {
			@Override
			public void widgetSelected(SelectionEvent e) {
				ITrace2D t = getTrace();
				if (t != null) {
					int i = plotFillCombo.getSelectionIndex();
					ITracePainter<?> painter;
					if (i == 0)
						painter = new TracePainterLine();
					else if (i == 1)
						painter = new TracePainterDisc();
					else if (i == 2)
						painter = new TracePainterVerticalBar(chart);
					else if (i == 3)
						painter = new TracePainterFill(chart);
					else
						painter = new TracePainterLine();
					t.setTracePainter(painter);

				}
			}
		});
	}

	protected void updateStroke() {
		try {
			int width = Integer.parseInt(widthSpinner.getText());
			String[] selection = stroke.getSelection();
			if (selection.length != 0) {
				float[] f;
				String s = selection[0];
				if (s.equals("dot"))
					f = dot(width);
				else if (s.equals("dash"))
					f = dash(width);
				else if (s.equals("dot-dash"))
					f = dot_dash(width);
				else
					f = null;
				getTrace().setStroke(
						new BasicStroke(width, BasicStroke.CAP_ROUND,
								BasicStroke.JOIN_BEVEL, 10, f, 0));
			}
		} catch (NumberFormatException ex) {
		}
	}

	protected Image getImageWidth(float f) {
		Image image = new Image(getDisplay(), new Rectangle(0, 0, 20, 10));
		GC gc = new GC(image);
		gc.fillRectangle(image.getBounds());
		gc.setForeground(getDisplay().getSystemColor(SWT.COLOR_BLACK));
		gc.drawLine(0, 0, 20, 10);
		gc.dispose();
		return image;
	}

	protected void refresh() {
		comboViewer.setInput(chart.getTraces().toArray());

		{
			ArrayList<IAxis> x = new ArrayList<IAxis>();
			x.addAll(chart.getAxesXBottom());
			x.addAll(chart.getAxesXTop());
			comboViewerX.setInput(x.toArray());
		}
		{
			ArrayList<IAxis> y = new ArrayList<IAxis>();
			y.addAll(chart.getAxesYLeft());
			y.addAll(chart.getAxesYRight());
			comboViewerY.setInput(y.toArray());
		}
		if (comboViewerX.getCombo().getSelectionIndex() == -1
				&& chart.getTraces().size() != 0)
			comboViewerX.setSelection(new StructuredSelection(chart
					.getAxesXBottom().get(0)));
		if (comboViewerY.getCombo().getSelectionIndex() == -1
				&& chart.getTraces().size() != 0)
			comboViewerY.setSelection(new StructuredSelection(chart
					.getAxesYLeft().get(0)));

		if (comboViewer.getCombo().getSelectionIndex() == -1
				&& chart.getTraces().size() != 0) {
			comboViewer.setSelection(new StructuredSelection(chart.getTraces()
					.first()));
		}
		refreshPlot();
	}

	protected void refreshPlot() {
		ITrace2D trace = getTrace();
		ITracePainter<?> painter = trace.getTracePainters().iterator().next();
		if (painter instanceof TracePainterLine)
			plotFillCombo.select(0);
		else if (painter instanceof TracePainterDisc)
			plotFillCombo.select(1);
		else if (painter instanceof TracePainterVerticalBar)
			plotFillCombo.select(2);
		else if (painter instanceof TracePainterFill)
			plotFillCombo.select(3);
		else
			plotFillCombo.select(0);

		name.setText(trace.getLabel());
		if (trace.getStroke() instanceof BasicStroke) {
			BasicStroke s = (BasicStroke) trace.getStroke();
			widthSpinner.setSelection((int) s.getLineWidth());
			float[] a = s.getDashArray();
			if (a == null)
				stroke.select(0);
			else if (a.length == 2 && a[0]==a[1])
				stroke.select(2);
			else if (a.length ==2)
				stroke.select(1);
			else if (a.length == 4)
				stroke.select(3);
			else
				stroke.select(-1);
		}
		Color color = trace.getColor();
		org.eclipse.swt.graphics.Color colorSWT = SWTResourceManager.getColor(
				color.getRed(), color.getGreen(), color.getBlue());
		lineColor.setBackground(colorSWT);
	}

	private ITrace2D getTrace() {
		ITrace2D t = ((ITrace2D) ((IStructuredSelection) comboViewer
				.getSelection()).getFirstElement());
		return t;
	}

}
