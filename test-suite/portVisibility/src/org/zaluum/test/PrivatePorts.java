package org.zaluum.test;

import org.zaluum.annotation.Box;
import org.zaluum.annotation.In;
import org.zaluum.annotation.Out;

@Box
public class PrivatePorts {
	@In private double i;
	@Out public double o;
	public void apply() {
		o=i;
	}
}
