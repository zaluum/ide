package org.zaluum.example.sound;

import org.zaluum.annotation.Box;
import org.zaluum.annotation.In;
import org.zaluum.annotation.Out;
@Box
public class Sin {
	@In public double t;
	@Out public double sin;
	public void apply() {
	  sin =Math.sin(t*2*Math.PI)*0.2;
	}
}
