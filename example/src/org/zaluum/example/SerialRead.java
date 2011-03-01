package org.zaluum.example;

import org.zaluum.runtime.*;
@Box
public class SerialRead {
	 @In(x=0,y=10) public SerialPort port;
	 @Out(x=48,y=24) public double out;
	 public void apply() {
		 out = 123.0;
	 }
}
