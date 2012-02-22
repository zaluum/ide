package org.zaluum.tutorial.sound;

import org.zaluum.annotation.Apply;
import org.zaluum.annotation.Box;

import edu.emory.mathcs.jtransforms.fft.DoubleFFT_1D;

@Box
public class FFT {
	private DoubleFFT_1D fft = new DoubleFFT_1D(WavSoundInput.CHUNKSIZE);
	@Apply
	public double[] apply(double[] params){
		double[] output = new double[params.length*2];
		System.arraycopy(params, 0, output, 0, params.length);
		fft.realForward(output);
		double[] half = new double[params.length];
		System.arraycopy(output, 0, half,0, half.length);
		return half;
	}
}
