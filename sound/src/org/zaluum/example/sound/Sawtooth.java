package org.zaluum.example.sound;

import org.zaluum.annotation.*;

@Box
public class Sawtooth {
	@In public double t;
	@Out public double out;
	public void apply(){
		out = 2*(t - Math.floor(t+(0.5)));
	}
}
