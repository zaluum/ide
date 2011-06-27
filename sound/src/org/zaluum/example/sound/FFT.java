package org.zaluum.example.sound;

import java.util.Arrays;

import org.zaluum.annotation.Box;
import org.zaluum.annotation.In;
import org.zaluum.annotation.Out;

@Box
public class FFT {
	@In public double[] params;
	@Out public double[] transformed;
	private double[] cached;
	public void apply(){
		if (Arrays.equals(params, cached)) return;
		transformed = FilterFactory.fftForConvolution(params);
		cached=Arrays.copyOf(params, params.length);
	}
}
