package org.zaluum.example;

import java.io.IOException;

import org.zaluum.runtime.*;
@Box
public class SerialRead {
	 @In public SerialPortData port;
	 @Out public int out;
	 public void apply() throws IOException {
		 out = port.inputStream.read();
	 }
}
