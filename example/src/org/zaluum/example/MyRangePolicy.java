package org.zaluum.example;

import info.monitorenter.gui.chart.rangepolicies.ARangePolicy;

public class MyRangePolicy extends ARangePolicy{
	private static final long serialVersionUID = -7526882866029117692L;
	private final double minMax;

	public MyRangePolicy(double minMax) {
		super();
		this.minMax = minMax;
	}
	@Override 
	public double getMax(double chartMin, double chartMax) {
		return Math.max(chartMax, minMax);
	}

	@Override
	public double getMin(double chartMin, double chartMax) {
		return chartMin;
	}

}
