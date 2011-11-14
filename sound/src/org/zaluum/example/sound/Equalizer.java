package org.zaluum.example.sound;

import org.zaluum.annotation.Box;

@Box
public class Equalizer {
	
	private double[] data;
	private final double scale; 
	public Equalizer(int samples) {
		data = new double[]{0,0,0,0,0,0,0,0};
		scale = 1000 * samples;
	}
	public double[] apply(double[] is) {
		for (int i=0; i<data.length; i++) {
			data[i] = is[i]/scale;
		}
		return FilterFactory.expandEQ(data, 128);
	}
}
