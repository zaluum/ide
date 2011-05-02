package org.zaluum.example;

import java.io.IOException;

import org.zaluum.runtime.*;
@Box
public class SerialHandshake {
	@In public SerialPortData port;
	@In public int in=0;
	 public void apply() throws IOException {
		 int i;
		 do{
			 i = port.inputStream.read();
		 }while((char)i!='A');
	 }
}
