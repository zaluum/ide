package org.zaluum.example;

import gnu.io.CommPort;
import gnu.io.CommPortIdentifier;
import gnu.io.SerialPort;

import org.zaluum.annotation.*;

@Box
public class SerialCreate {
	@Out public SerialPortData p;
	public void apply(){
		try {
			CommPortIdentifier portIdentifier = CommPortIdentifier.getPortIdentifier("/dev/ttyUSB0");
			CommPort port = portIdentifier.open(this.getClass().getName(), 2000);
			if (port instanceof SerialPort) {
				SerialPort serial = (SerialPort) port;
		        serial.setSerialPortParams(1000000,SerialPort.DATABITS_8,SerialPort.STOPBITS_1,SerialPort.PARITY_NONE);
		        serial.setInputBufferSize(2048);
		        serial.setOutputBufferSize(2048);
		        p = new SerialPortData(serial);
			}
		} catch (Exception e) { 
			throw new RuntimeException(e);
		}
	}
}
