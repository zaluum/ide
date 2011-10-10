package org.zaluum.widget.plot;

import info.monitorenter.gui.chart.labelformatters.LabelFormatterNumber;

import java.text.DecimalFormat;

public class LabelFormatterDecimal extends LabelFormatterNumber {

	private static final long serialVersionUID = 1L;

	public LabelFormatterDecimal() {
		super(new DecimalFormat());
	}

	public LabelFormatterDecimal(DecimalFormat formatter) {
		super(formatter);
	}
	public String toPattern() {
		return ((DecimalFormat)m_numberFormat).toPattern();
	}
}
