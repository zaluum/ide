package org.zaluum.example;

import org.zaluum.runtime.Box;
import org.zaluum.runtime.In;
import org.zaluum.runtime.Out;

@Box
public class TicksToForce {
	@In(x=0,y=24) public double ticks;
	@Out(x=48,y=24) public double force;
	public static double resolution = 0.000025;
	public static double BLAU64x16 = 10.5; // N/mm
	public static double BLAU51x16 = 15.4; // N/mm
	public static double k = BLAU64x16*2*1000;
	
	public void apply(){
		double meters = ticks * resolution;
		force = - meters * k;
	}
}
