package org.zaluum.example.sound;

import org.zaluum.annotation.*;

@Box
public class Toothsaw {
	@In public double t;
	@Out public double out;
	public void apply(){
		out = 2*(t - Math.floor(t+(0.5)))*0.3;
	}
}
