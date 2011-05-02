package org.zaluum.example;

import org.zaluum.runtime.Box;
import org.zaluum.runtime.In;
import org.zaluum.runtime.Out;

@Box
public class TicksToCurrent {
	@In public double currentTicks;
	@Out public double current;
	@In public double voltTicks;
	@Out public double volt;
	
	public static double vPerTick = 16.00/955; 
	
	public static double resolution = 1024;
	public static double vPerAmp = 0.13; // V/A
	public static double AREF = 5.0; // V
	
	public void apply(){
		double volts = (currentTicks / resolution)*AREF;
		current = volts / vPerAmp;
		volt = voltTicks * vPerTick;
	}
}
