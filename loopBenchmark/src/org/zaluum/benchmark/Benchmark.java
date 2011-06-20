package org.zaluum.benchmark;

import org.zaluum.benchmark.allobjects.OuterLoop;

public class Benchmark {
	public static final double inner = 10000;
	public static final double outer = 100000;
	public static long start;
	public static void start() {
		start = System.nanoTime();
	}
	public static void stop(String s) {
		System.out.println(s + ": " + (System.nanoTime()-start)/1000000000.0 + "s");
	}
	public static void main(String[] args) {
	  for(int i=0; i<3; i++){
  	  start();
  	  new JavaLoop().apply();
  	  stop("java");
  	  start();
  	  new OuterLoop().apply();
  	  stop("allobjects");
  	  start();
  	  new org.zaluum.benchmark.parameters.OuterLoop().apply();
  	  stop("parameters");
  	  start();
      new org.zaluum.benchmark.returns.OuterLoop().apply();
      stop("returns");
	  }
	}
}
