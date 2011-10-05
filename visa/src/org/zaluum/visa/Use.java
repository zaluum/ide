package org.zaluum.visa;

import org.zaluum.visa.swig.JVisa;

public class Use {
	public static void main(String[] args) {
		System.loadLibrary("jvisa");
		long[] defaultRM = {0};
		long[] instr = {0};
		int status = JVisa.viOpenDefaultRM(defaultRM);
		if (status > 0)
			System.exit(-1);
		status = JVisa.viOpen(defaultRM[0],
				"ASRL10::INSTR", 0, 0, instr);
		/* Use device and eventually close it. */
		JVisa.viClose(instr[0]);
		JVisa.viClose(defaultRM[0]);
		System.out.println("ok" + Integer.toHexString(status));
	}

}

