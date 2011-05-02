package org.zaluum.example;

import java.io.IOException;

import org.zaluum.runtime.*;
@Box
public class SerialWrite16 {
	@In public SerialPortData port;
	@Out public SerialPortData portOut;
	@In public int in=0;
	 public void apply() throws IOException {
		// System.out.println("writing " + in);
		 port.outputStream.write(in & 0xFF);
		 port.outputStream.write(((in&0xFF00) >> 8)&0xFF);
		 portOut=port;
	 }
}
