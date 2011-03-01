package org.zaluum.example;

import org.zaluum.runtime.Box;
import org.zaluum.runtime.In;
import org.zaluum.runtime.Out;

@Box
public class PID {
	@In(x=0,y=10) public double setPoint = 0.0;
	@In(x=0,y=15) public double signal = 0.0;
	@In(x=0,y=20) public double p = 1.0;
	@In(x=0,y=25) public double i = 0.01;
	@In(x=0,y=30) public double d = 0.01;
	@In(x=0,y=35) public double timeStep = 0.01;
	@Out(x=48,y=24) public double out = 0.0;
	
	public void apply() {
		double err = signal - setPoint;
		out = err * p;
	}
}
