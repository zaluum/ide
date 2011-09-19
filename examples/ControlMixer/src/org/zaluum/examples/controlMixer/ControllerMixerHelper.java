package org.zaluum.examples.controlMixer;

import java.awt.Cursor;

public class ControllerMixerHelper {
	public static void init(ControlMixer mixer) {
		System.out.println("works!!!" + mixer.a);
		mixer.filler.setCursor(Cursor.getPredefinedCursor(Cursor.HAND_CURSOR));
	}
} 
