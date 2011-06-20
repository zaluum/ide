package org.zaluum.example.sound;

public class Timer {
	public static void main(String[] args) {
		LooperJava looperJava = new LooperJava();
		long init = System.nanoTime();
		looperJava.apply();
		long end = System.nanoTime();
		System.out.println((end-init)/1000000000.0);

		
		LoopSum loopSum = new LoopSum();
		init = System.nanoTime();
		loopSum.apply();
		end = System.nanoTime();
		System.out.println((end-init)/1000000000.0);
	}
}
