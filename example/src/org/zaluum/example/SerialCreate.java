package org.zaluum.example;

import org.zaluum.nide.java.Box;
import org.zaluum.nide.java.Out;

@Box
public class SerialCreate {
	@Out(x=48,y=24) public SerialPort p;
	public void apply(){
		System.out.println("Created serial port");
		p = new SerialPort();
	}
}
