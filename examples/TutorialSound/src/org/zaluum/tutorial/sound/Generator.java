package org.zaluum.tutorial.sound;

import org.apache.commons.math.util.FastMath;
import org.zaluum.annotation.Apply;
import org.zaluum.annotation.Box;

@Box
public class Generator {
	double delta = 1/44100f;
	double t=0;
	@Apply
	public double[] sin(double f) {
		double[] result = new double[SoundMath.CHUNKSIZE];
		for (int i=0; i<result.length; i++) {
			result[i] = FastMath.sin(t*2*Math.PI*f) * 0.3;
			t+=delta; 
		}
		return result;
	}
	@Apply
	public double[] sawtooth(double f) {
		double[] result = new double[SoundMath.CHUNKSIZE];
		for (int i=0; i<result.length; i++) {
			double x = t*f;
			if (f==0)
				result[i]=0;
			result[i] = 2*(x - Math.floor(x+(0.5))) * 0.3;
			t+=delta; 
		}
		return result;
	}
}
