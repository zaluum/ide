package org.zaluum.example;

import org.zaluum.runtime.Box;
import org.zaluum.runtime.In;
import org.zaluum.runtime.Out;

@Box
public class PID {
	@In(x=0,y=10) public double setPoint = 0.0;
	@In(x=0,y=15) public double signal = 0.0;
	@In(x=0,y=20) public double p = 1.0;
	@In(x=0,y=25) public double ki = 0.001;
	@In(x=0,y=30) public double d = 0.01;
	@In(x=0,y=35) public double timeStep = 0.01;
	@Out(x=48,y=24) public double out = 0.0;
	@Out(x=48,y=32) public double err = 0.0;
	
	double lastErr = 0.0;
	double i = 0.0;
	public void apply() {
		err =  setPoint - signal;
		double derr = (err-lastErr)/timeStep;
		i = i + (err * timeStep);
		if (i>500) i = 500;
		if (i<-500) i = -500;
		out = err * p + derr*d + i * ki;
		if (out>800) out = 800;
		if (out<-800) out = -800;
		//System.out.println("err=" + err + " derr=" + derr + "i=" + i);
		lastErr = err;
	}
}
