package org.zaluum.examples.tank;

import org.zaluum.annotation.Apply;
import org.zaluum.annotation.Box;

@Box
public class ChangeState {
	@Apply
	public boolean apply(double low, double currentValue, double high, boolean currentState) {
		return (currentValue<=high && currentState) || (low > currentValue && !currentState);
	}
}
