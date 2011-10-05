package org.zaluum.visa.jna;

import java.nio.ByteBuffer;

import com.sun.jna.NativeLong;
import com.sun.jna.ptr.NativeLongByReference;

import visa32.Visa32Library;

public class Jna {
	public static void main(String[] args) {
		NativeLongByReference defaultRM =  new NativeLongByReference();
		NativeLongByReference instr = new NativeLongByReference();
		Visa32Library v = Visa32Library.INSTANCE;
		NativeLong status = v.viOpenDefaultRM(defaultRM);
		ByteBuffer b = ByteBuffer.wrap("ASRL10::INSTR".getBytes());
		status = v.viOpen(defaultRM.getValue(), b, new NativeLong(0), new NativeLong(0), instr);
		ByteBuffer desc = ByteBuffer.allocate(256);
		status = v.viStatusDesc(defaultRM.getValue(), status, desc);
		System.out.println(new String(desc.array()));
		/* Use device and eventually close it. */
		v.viClose(instr.getValue());
		v.viClose(defaultRM.getValue());
		System.out.println("ok" + Long.toHexString(status.longValue()));
	}

}

