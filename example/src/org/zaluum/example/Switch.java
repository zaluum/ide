package org.zaluum.example;

import org.zaluum.runtime.*;
@Box
public class Switch {
	 @In(x=0,y=24) public boolean ab;
	 @In(x=0,y=16) public double a;
	 @In(x=0,y=32) public double b;
	 @Out(x=48,y=24) public double out;
	 public void apply() {
		 if (ab) out=a; else out=b;
	 }
}
