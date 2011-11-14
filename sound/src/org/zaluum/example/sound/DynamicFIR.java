package org.zaluum.example.sound;

import org.zaluum.annotation.Apply;
import org.zaluum.annotation.Box;

@Box
public class DynamicFIR extends AbstractFIRBox {
	protected double[] coeficients;
	public DynamicFIR(int len) {
		super(len); 
	}
	@Apply
	public double filtered(double[] coeficients, double d) {
		this.coeficients = coeficients;
		return filtered(d);
	}
	@Override
	public double[] getCoeficients() {
		return coeficients;
	}
}
