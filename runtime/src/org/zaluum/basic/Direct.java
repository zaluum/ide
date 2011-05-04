package org.zaluum.basic;

import org.zaluum.annotation.Box;
import org.zaluum.annotation.Out;
import org.zaluum.annotation.Param;

@Box
public class Direct {
	 @Param(defaults="0") public double param;
	 @Out public double out;
	 public void apply() {
		 out = param;
	 }
}
