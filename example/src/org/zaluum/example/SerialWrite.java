package org.zaluum.example;

import org.zaluum.runtime.*;
@Box
public class SerialWrite {
	@In(x=0,y=10) public SerialPort port;
	@In(x=0,y=24) public double in=0.0;
	 public void apply() {
		 System.out.println("<-" + in);
	 }
}
