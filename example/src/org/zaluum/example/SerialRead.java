package org.zaluum.example;

import java.io.IOException;

import org.zaluum.runtime.*;
@Box
public class SerialRead {
	 @In(x=0,y=24) public SerialPortData port;
	 @Out(x=48,y=24) public int out;
	 public void apply() throws IOException {
		 out = port.inputStream.read();
	 }
}
