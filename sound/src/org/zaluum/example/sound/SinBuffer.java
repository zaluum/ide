package org.zaluum.example.sound;

import org.zaluum.annotation.Box;
import org.zaluum.annotation.In;
import org.zaluum.annotation.Out;
@Box
public class SinBuffer {
	@In double t;
	@Out double sin;
	public void apply() {
		sin =Math.sin(t*2*Math.PI*440);
	}
}
