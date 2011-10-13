package org.zaluum.widget.plot;

import info.monitorenter.gui.chart.Chart2D;
import info.monitorenter.gui.chart.IAxis;
import info.monitorenter.gui.chart.IRangePolicy;
import info.monitorenter.gui.chart.ITrace2D;
import info.monitorenter.gui.chart.ITracePainter;
import info.monitorenter.gui.chart.axis.AxisLog10;
import info.monitorenter.gui.chart.traces.painters.TracePainterFill;
import info.monitorenter.gui.chart.traces.painters.TracePainterVerticalBar;

import java.awt.BasicStroke;
import java.io.PrintWriter;
import java.io.StringWriter;

public class PlotConfiguration {
	public static String javaScriptConfigure(Chart2D c) {
		StringWriter stringWriter = new StringWriter();
		PrintWriter p = new PrintWriter(stringWriter);
		p.format("c.removeAllTraces();\n");
		ITrace2D t = c.getTraces().first();
		p.format("c.setPaintLabels(%b);\n", c.isPaintLabels());
		p.format("c.setUseAntialiasing(true);\n");
		doAxis(true, p, c);
		doAxis(false, p, c);
		p.format(
				"var t = new Packages.info.monitorenter.gui.chart.traces.Trace2DLtd(100,\"%s\");\n",
				StringEscapeUtils.escapeJavaScript(t.getName()));
		p.format("t.setColor(new java.awt.Color(%d));\n", t.getColor().getRGB());
		if (t.getTracePainters().size() == 1) {
			ITracePainter<?> painter = t.getTracePainters().iterator().next();
			if (painter instanceof TracePainterVerticalBar
					|| painter instanceof TracePainterFill)
				p.format("t.setTracePainter(new Packages.%s(c));\n", painter
						.getClass().getName());
			else
				p.format("t.setTracePainter(new Packages.%s());\n", painter
						.getClass().getName());

		}

		if (t.getStroke() != null && t.getStroke() instanceof BasicStroke) {
			BasicStroke b = (BasicStroke) t.getStroke();
			float[] arr = b.getDashArray();
			if (arr != null) {
				p.format(
						"var dash = java.lang.reflect.Array.newInstance(java.lang.Float.TYPE,%d);\n",
						arr.length);
				for (int i = 0; i < arr.length; i++)
					p.format("dash[%d]=%s;\n", i, "" + arr[i]);
			} else {
				p.format("var dash=null;\n");
			}
			p.format(
					"t.setStroke(new java.awt.BasicStroke(%s, %d, %d, %s, dash, %s));\n",
					"" + b.getLineWidth(), b.getEndCap(), b.getLineJoin(), ""
							+ b.getMiterLimit(), "" + b.getDashPhase());
		}
		p.format("c.addTrace(t, xaxis,yaxis);\n");
		return stringWriter.toString();
	}

	public static void doAxis(boolean x, PrintWriter p, Chart2D c) {
		IAxis axis;
		String name;
		if (x) {
			axis = c.getAxesXBottom().get(0);
			name = "xaxis";
		} else {
			axis = c.getAxesYLeft().get(0);
			name = "yaxis";
		}
		if (axis instanceof AxisLog10)
			p.format(
					"var %s = new Packages.info.monitorenter.gui.chart.axis.AxisLog10();\n",
					name);
		else
			p.format(
					"var %s= new Packages.info.monitorenter.gui.chart.axis.AxisLinear();\n",
					name);
		IRangePolicy rangePolicy = axis.getRangePolicy();
		p.format("var policy = new Packages.%s();\n", rangePolicy.getClass()
				.getName());
		if (x)
			p.format("c.setAxisXBottom(xaxis, 0);\n");
		else
			p.format("c.setAxisYLeft(yaxis, 0);\n");
		if (axis.getFormatter() instanceof LabelFormatterDecimal) {
			String pattern = ((LabelFormatterDecimal) axis.getFormatter())
					.toPattern();
			p.format("var format = new java.text.DecimalFormat(\"%s\");\n",
					StringEscapeUtils.escapeJavaScript(pattern));
			p.format(
					"%s.setFormatter(new Packages.org.zaluum.widget.plot.LabelFormatterDecimal(format));\n",
					name);
		}
		p.format(
				"policy.setRange(new Packages.info.monitorenter.util.Range(%s,%s));\n",
				"" + rangePolicy.getRange().getMin(), ""
						+ rangePolicy.getRange().getMax());
		p.format("%s.setRangePolicy(policy);", name);
		p.format("%s.setTitle(\"%s\");\n", name, StringEscapeUtils
				.escapeJavaScript(axis.getAxisTitle().getTitle()));
		p.format("%s.setPaintScale(%b);\n", name, axis.isPaintScale());
		p.format("%s.setPaintGrid(%b);\n", name, axis.isPaintGrid());
		p.format("%s.setVisible(%b);\n", name, axis.isVisible());
	}
}
