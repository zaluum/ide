package org.zaluum.example.sound;

public class LooperJava {
	double a = 0;
	double b = 0;
	public void apply() {
		double i = 1;
		while(a<10000000){
			a = a +i;
			b=0;
			while (b<1000) {
				b +=i;
			}
		}
	}
}
