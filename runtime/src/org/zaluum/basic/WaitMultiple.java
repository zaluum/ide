package org.zaluum.basic;

import org.zaluum.annotation.Apply;
import org.zaluum.annotation.Box;

@Box
public class WaitMultiple {
	@Apply
	public void apply(int ms) throws InterruptedException {
		long now = System.currentTimeMillis();
		long millis = (now - (now%ms) + ms) - now; // TODO review
		Thread.sleep(millis);
	}
}
