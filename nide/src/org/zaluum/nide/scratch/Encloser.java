package org.zaluum.nide.scratch;

public class Encloser {
	protected int i = 0;

	class Enclosing implements Runnable {
		public void run() {
			i = 1;
		}
	}

	public void caca() {
		i = 2;
	}

}
