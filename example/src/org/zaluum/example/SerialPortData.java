package org.zaluum.example;

import gnu.io.SerialPort;

import java.io.BufferedInputStream;
import java.io.IOException;
import java.io.InputStream;
import java.io.OutputStream;

public class SerialPortData {
	public final SerialPort serial;
	public final InputStream inputStream;
	public final OutputStream outputStream;

	public SerialPortData(SerialPort serial) throws IOException {
		this.serial = serial;
		inputStream = serial.getInputStream();
		outputStream = serial.getOutputStream();
	}
}
