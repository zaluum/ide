package org.zaluum.benchmark;

public class JavaLoop {
	public double a;
	public double b;
	public void apply(){
	
		while (a< Benchmark.outer) {
			a+=1;
			b=0;
			while(b<Benchmark.inner) {
				b+=1;
			}
		}
	}
	public static void main(String[] args) {
    new JavaLoop().apply();
  }
}
