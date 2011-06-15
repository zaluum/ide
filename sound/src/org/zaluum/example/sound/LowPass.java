package org.zaluum.example.sound;

import org.zaluum.annotation.Box;

@Box
public class LowPass extends FIRBox{
	public LowPass(double cutoff) {
		fir = new FIR(FIRFactory.blackmanLowPass(511, 2*Math.PI*cutoff/44000.0));
	}
}
