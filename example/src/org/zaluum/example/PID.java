package org.zaluum.example;

import org.zaluum.annotation.*;

@Box
public class PID {
	@In public double setPoint = 0.0;
	@In public double signal = 0.0;
	@In public double p = 1.0;
	@In public double ki = 0.001;
	@In public double d = 0.01;
	@In public double timeStep = 0.01;
	@Out public double out = 0.0;
	@Out public double err = 0.0;
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
