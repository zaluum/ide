package org.zaluum.visa.jna;

import java.nio.ByteBuffer;
import java.nio.charset.Charset;

import com.sun.jna.NativeLong;
import com.sun.jna.ptr.NativeLongByReference;

import visa32.Visa32Library;
import static visa32.Visa32Library.*;
public class Instrument {
	static Visa32Library v = Visa32Library.INSTANCE;
	private NativeLong rm;
	private NativeLong vi;
	
	public static long VI_SUCCESS = 0;
	public static Charset ASCII = Charset.forName("ASCII");
	public static byte[] ascii(String str) {
		return str.getBytes(ASCII);
	}
	public static ByteBuffer toBuf(String s) {
		return ByteBuffer.wrap(ascii(s));
	}
	public static void throwVi(NativeLong status) throws VisaException {
		long l = status.longValue();
		if (l!=VI_SUCCESS) {
			throw new VisaException(l);
		}
	}
	public Instrument(String id) throws VisaException {
		throwVi(v.viOpenDefaultRM(new NativeLongByReference(rm)));
		v.viOpen(rm, 
				toBuf(id), 
				new NativeLong(0), 
				new NativeLong(0), 
				new NativeLongByReference(vi));
	}
	public int writeStr(String str) throws VisaException {
		ByteBuffer buf = toBuf(str);
		NativeLongByReference ret = new NativeLongByReference();
		throwVi(v.viWrite(vi, buf , new NativeLong(buf.array().length), ret));
		return ret.getValue().intValue();
	}
	public String readStr(int len) throws VisaException {
		ByteBuffer buf = ByteBuffer.allocate(len);
		NativeLongByReference ret = new NativeLongByReference();
		throwVi(v.viRead(vi, buf, new NativeLong(len), ret));
		byte[] result = new byte[ret.getValue().intValue()];
		buf.get(result);
		return new String(result,ASCII);
	}
}
