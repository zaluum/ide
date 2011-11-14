package org.zaluum.example.sound;

import java.util.Arrays;

import org.zaluum.annotation.Apply;
import org.zaluum.annotation.Box;

@Box
public class FFT {
	private double[] cached;
	private double[] window= FilterFactory.hammingWindow(4096);
	@Apply
	public double[] apply(double[] params){
		//if (Arrays.equals(params, cached)) return cached;
		double [] res = FilterFactory.fft(FilterFactory.multiply(params,window));
		//cached=Arrays.copyOf(params, params.length);
		return res;
	}
}
