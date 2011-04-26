package org.zaluum.example;

import org.zaluum.runtime.Box;
import org.zaluum.runtime.In;
import org.zaluum.runtime.Out;

@Box
public class TicksToCurrent {
	@In(x=0,y=16) public double currentTicks;
	@Out(x=48,y=16) public double current;
	@In(x=0,y=32) public double voltTicks;
	@Out(x=48,y=32) public double volt;
	
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
