public class Use {
	public static void main(String[] args) {
		System.loadLibrary("example");
		SWIGTYPE_p_unsigned_int defaultRM = new SWIGTYPE_p_unsigned_int();
		SWIGTYPE_p_unsigned_int instr = new SWIGTYPE_p_unsigned_int();
		int status = example.viOpenDefaultRM(defaultRM);
		if (status > 0)
			System.exit(-1);
		status = example.viOpen(SWIGTYPE_p_unsigned_int.getCPtr(defaultRM),
				"GPIB::1::INSTR", 0, 0, instr);

		/* Use device and eventually close it. */
		example.viClose(SWIGTYPE_p_unsigned_int.getCPtr(instr));
		example.viClose(SWIGTYPE_p_unsigned_int.getCPtr(defaultRM));
		System.out.println("ok");
	}

}

