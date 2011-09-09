package org.zaluum.math;

import org.zaluum.annotation.Apply;
import org.zaluum.annotation.Box;

@Box
public class BoolSelect {
	@Apply
	public double apply(double ontrue, boolean b, double onfalse) {
		return b?ontrue:onfalse;
	}
}
