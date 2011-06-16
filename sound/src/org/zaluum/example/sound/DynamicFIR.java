package org.zaluum.example.sound;

import org.zaluum.annotation.Box;
import org.zaluum.annotation.In;

@Box
public class DynamicFIR extends AbstractFIRBox {
	@In double[] coeficients; 
	public DynamicFIR(int len) {
		super(len); 
	}
	@Override
	public double[] getCoeficients() {
		return coeficients;
	}
}
