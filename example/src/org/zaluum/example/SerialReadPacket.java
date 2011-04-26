package org.zaluum.example;

import java.io.IOException;

import org.zaluum.runtime.*;

import com.google.common.io.ByteStreams;

@Box
public class SerialReadPacket {
	@In(x = 0, y = 10)   public SerialPortData port;
	@Out(x = 48, y = 10) public SerialPortData portOut;
	@Out(x = 48, y = 16) public double cs = 0;
	@Out(x = 48, y = 24) public boolean diag;
	@Out(x = 48, y = 32) public double volt = 0;
	@Out(x = 48, y = 42) public double lin = 0;
	
	public void apply() {
		int packetLen = 7;
		byte[] buffer = new byte[packetLen];
		portOut = port;
		try {
			port.outputStream.flush();
			ByteStreams.readFully(port.inputStream,buffer);
			lin = (short) (((buffer[0] & 0xFF) + ((buffer[1] & 0xFF) << 8)) & 0xFFFF);
			cs = (short) (((buffer[2] & 0xFF) + ((buffer[3] & 0xFF) << 8)) & 0xFFFF);
			volt = (short) (((buffer[4] & 0xFF) + ((buffer[5] & 0xFF) << 8)) & 0xFFFF);
			diag = buffer[6]==0;
		} catch (IOException e) {
			// TODO Auto-generated catch block
			e.printStackTrace();
		}
	}
}
