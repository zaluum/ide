package org.zaluum.math;

import org.zaluum.annotation.Apply;
import org.zaluum.annotation.Box;

@Box
public class Random {
	@Apply
	public double apply() {
		return Math.random();
	}
}
