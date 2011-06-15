package org.zaluum.example.sound;

import org.zaluum.annotation.Box;
import org.zaluum.annotation.In;

@Box
public class PrintArray {
	@In short[] in;
	public void apply() {
		for (Short s: in) {
			System.out.print(" " + s);
		}
		System.out.println(";");
	}
}
