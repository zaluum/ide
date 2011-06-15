package org.zaluum.example.sound;

import org.zaluum.annotation.In;
import org.zaluum.annotation.Out;

public abstract class FIRBox {
	@In double in;
	@Out double out;
	protected FIR fir;
	public void apply(){
		out= fir.process(in);
	}
}
