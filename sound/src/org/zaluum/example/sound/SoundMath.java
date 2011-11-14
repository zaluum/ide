package org.zaluum.example.sound;

import org.zaluum.annotation.Box;
import org.zaluum.annotation.StaticBox;
@StaticBox
public class SoundMath {
	@Box
	public static double sin(double t) {
	  return Math.sin(t*2*Math.PI)*0.2;
	}
	@Box
	public static double toothsaw(double t){
		return 2*(t - Math.floor(t+(0.5)))*0.3;
	}
	@Box
	public static double sin440(double t) {
		return Math.sin(t*2*Math.PI*440);
	}
	@Box
	public static double peakDetection(double[] data) {
		double max = Double.NEGATIVE_INFINITY;
		int maxi=0;
		for (int i=0;i<data.length;i++) {
			if (data[i]>max && data[i]>1) {
				maxi=i; max = data[i];
			}
		}
		return maxi*11025/4096;
	}
}
