package org.zaluum.example.sound;

import org.zaluum.annotation.Apply;

public abstract class AbstractFIRBox {
	public double in;
	public double out;
	
	private final double[] buffer;
	private int count;
	public AbstractFIRBox(int len) {
		buffer = new double[len];
		count = 0;
	}
	@Apply
	public double filtered(double in){
		return process(in);
	}
	public abstract double[] getCoeficients();
	public double process(double sample) {
		buffer[count] = sample;
		double out = 0;
		double[] coeficients = getCoeficients();
		if (coeficients.length!=buffer.length) throw new IllegalArgumentException();
		int index = count;
		for (int i=0;i<coeficients.length; i++) {
			out += coeficients[i] * buffer[index--];
			if (index <0) index = coeficients.length-1;
		}
		if (++count >= coeficients.length) count = 0;
		return out;
	}
}
