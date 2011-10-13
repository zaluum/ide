package org.zaluum.widget.plot;

import info.monitorenter.gui.chart.IAxis;

import org.eclipse.jface.viewers.LabelProvider;

public class AxisTitleLabelProvider extends LabelProvider {
	public String getText(Object element) {
		return element == null ? "" : ((IAxis) element).getAxisTitle().getTitle();//$NON-NLS-1$
	}
}