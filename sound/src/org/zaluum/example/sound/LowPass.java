package org.zaluum.example.sound;

import org.zaluum.annotation.Box;
import org.zaluum.annotation.Out;

@Box
public class LowPass extends AbstractFIRBox{
	@Out public double[] coeficients ;
	public LowPass(double cutoff) {
		super(512);
		coeficients = FilterFactory.blackmanLowPass(511, 2*Math.PI*cutoff/44000.0);
	}
	@Override
	public double[] getCoeficients() {
		return coeficients;
	}
}
