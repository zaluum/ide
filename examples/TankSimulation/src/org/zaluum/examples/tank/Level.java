package org.zaluum.examples.tank;

import org.zaluum.annotation.Apply;
import org.zaluum.annotation.Box;

@Box
public class Level {
	@Apply
	public double apply(double dt, double dm, double level) {
		return dt*dm + level;
	}
	public double d2() {
		return 5; 
	}
}
