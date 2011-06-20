package org.zaluum.example.sound;

import org.zaluum.annotation.Box;
import org.zaluum.annotation.Out;

@Box
public class LowPass extends AbstractFIRBox{
	@Out public double[] coeficients ;
	public LowPass(double cutoff) {
		super(511);
		coeficients = FilterFactory.blackmanLowPass(511, 2*Math.PI*cutoff/44100.0);
	}
	@Override
	public double[] getCoeficients() {
		return coeficients;
	}
}
