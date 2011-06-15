package org.zaluum.example.sound;

import org.zaluum.annotation.Box;
import org.zaluum.annotation.Out;

@Box
public class Time {
	@Out public double t;
	long init ;
	public Time() {
		init = System.nanoTime();
	}
	public void apply(){
		t = (System.nanoTime()-init)/1000000000.0;
	}
}
