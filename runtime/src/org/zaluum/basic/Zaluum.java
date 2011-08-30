package org.zaluum.basic;

import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.concurrent.ExecutorService;
import java.util.concurrent.Executors;
import java.util.concurrent.Future;

import javax.swing.Timer;

public class Zaluum {
	public static ExecutorService executor = Executors.newCachedThreadPool();

	public static void execute(Runnable r) {
		executor.execute(r);
	}

	public static Future<?> submit(Runnable r) {
		return executor.submit(r);
	}

	private static Map<Object, Runnable> toUpdateMap = new HashMap<Object, Runnable>();
	public static Object sync = new Object();
	public static Timer timer = new Timer(20, new ActionListener() {
		public void actionPerformed(ActionEvent e) {
			final List<Runnable> oldb;
			synchronized (sync) {
				if (toUpdateMap.isEmpty())
					return;
				oldb = new ArrayList<Runnable>(toUpdateMap.size());
				oldb.addAll(toUpdateMap.values());
				toUpdateMap.clear();
			}
			for (Runnable r : oldb) {
				r.run();
			}
			synchronized (sync) {
				if (toUpdateMap.isEmpty())
					timer.stop();
			}
		}
	});

	public static void fastUpdate(Object uniqueSource, Runnable runnable) {
		synchronized (sync) {
			toUpdateMap.put(uniqueSource, runnable);
			timer.start();
		}
	}

}
