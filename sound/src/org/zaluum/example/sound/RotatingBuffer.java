package org.zaluum.example.sound;

import org.zaluum.annotation.Apply;
import org.zaluum.annotation.Box;

@Box
public class RotatingBuffer {
	double[] data = new double[4096];
	@Apply
	public double[] rotated(double[] incoming) {
		int len = incoming.length;
		System.arraycopy(data, len, data, 0,  data.length-len);
		System.arraycopy(incoming, 0, data, data.length-len, len);
		return data;
	}
}
