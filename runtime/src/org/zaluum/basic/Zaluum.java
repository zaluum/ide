package org.zaluum.basic;

import java.util.concurrent.ExecutorService;
import java.util.concurrent.Executors;

public class Zaluum {
	public static ExecutorService executor = Executors.newCachedThreadPool();

	public static void execute(Runnable r) {
		executor.execute(r);
	}
}
