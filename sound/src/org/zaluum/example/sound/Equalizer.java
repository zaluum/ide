package org.zaluum.example.sound;

import org.zaluum.annotation.Box;
import org.zaluum.annotation.In;
import org.zaluum.annotation.Out;

@Box
public class Equalizer {
	@Out public double[] impulseResponse;
	@In public double i0;
	@In public double i1;
	@In public double i2;
	@In public double i3;
	@In public double i4;
	@In public double i5;
	@In public double i6;
	@In public double i7;
	private double[] data;
	private final double scale; 
	public Equalizer(int samples) {
		data = new double[]{0,0,0,0,0,0,0,0};
		scale = 1000 * samples;
	}
	public void apply() {
		
		data[0]=i0/scale;
		data[1]=i1/scale;
		data[2]=i2/scale;
		data[3]=i3/scale;
		data[4]=i4/scale;
		data[5]=i5/scale;
		data[6]=i6/scale;
		data[7]=i7/scale;
		impulseResponse = FilterFactory.expandEQ(data, 128);
	}
}
