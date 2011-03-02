package org.zaluum.example;

import org.zaluum.runtime.*;
@Box
public class Direct {
	 @Param(defaults="0") public double param;
	 @Out(x=40,y=10) public double out;
	 public void apply() {
		 out = param;
	 }
}
