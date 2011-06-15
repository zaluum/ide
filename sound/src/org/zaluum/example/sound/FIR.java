package org.zaluum.example.sound;

public class FIR {
	final double[] coeficients;
	private final double[] buffer;
	private int count;
	public FIR(double[] coeficients) {
		this.coeficients = coeficients;
		buffer = new double[coeficients.length];
		count = 0;
	}
	public double process(double sample) {
		buffer[count] = sample;
		double out = 0;
		int index = count;
		for (int i=0;i<coeficients.length; i++) {
			out += coeficients[i] * buffer[index--];
			if (index <0) index = coeficients.length-1;
		}
		if (++count >= coeficients.length) count = 0;
		return out;
	}
}
