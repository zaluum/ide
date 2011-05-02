package org.zaluum.example;

import java.io.IOException;

import org.zaluum.runtime.*;

import com.google.common.io.ByteStreams;

@Box
public class SerialReadPacket {
	@In   public SerialPortData port;
	@Out public SerialPortData portOut;
	@Out public double cs = 0;
	@Out public boolean diag;
	@Out public double volt = 0;
	@Out public double lin = 0;
	
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
