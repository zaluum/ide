package org.zaluum.example;

import org.zaluum.runtime.*;
@Box
public class Direct {
	 @Param(defaults="0") public double param;
	 @Out public double out;
	 public void apply() {
		 out = param;
	 }
}
