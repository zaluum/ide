package org.zaluum.basic;

import org.zaluum.annotation.Box;
import org.zaluum.annotation.Out;

@Box
public class Direct {
	 @Out public double out;
	 private double param =0.0;
	 public void setParam(double d) {
	   param = d;
	 }
	 public void apply() {
		 out = param;
	 }
}
