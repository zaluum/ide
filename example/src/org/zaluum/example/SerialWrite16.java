package org.zaluum.example;

import java.io.IOException;

import org.zaluum.runtime.*;
@Box
public class SerialWrite16 {
	@In(x=0,y=5) public SerialPortData port;
	@Out(x=48,y=5) public SerialPortData portOut;
	@In(x=0,y=24) public int in=0;
	 public void apply() throws IOException {
		// System.out.println("writing " + in);
		 port.outputStream.write(in & 0xFF);
		 port.outputStream.write(((in&0xFF00) >> 8)&0xFF);
		 portOut=port;
	 }
}
