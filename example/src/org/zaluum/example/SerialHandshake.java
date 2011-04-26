package org.zaluum.example;

import java.io.IOException;

import org.zaluum.runtime.*;
@Box
public class SerialHandshake {
	@In(x=0,y=10) public SerialPortData port;
	@In(x=0,y=24) public int in=0;
	 public void apply() throws IOException {
		 int i;
		 do{
			 i = port.inputStream.read();
		 }while((char)i!='A');
	 }
}
