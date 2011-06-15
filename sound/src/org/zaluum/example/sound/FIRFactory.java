package org.zaluum.example.sound;
import static java.lang.Math.*;
public class FIRFactory {
	public static double[] nofilter(int order) {
		double [] f = new double[order];
		f[order/2] = 1.0/order;
		return f;
	}
	public static double[] rectangularLowPass(int len, double omega) {
		if (len<=0) throw new IllegalArgumentException();
		int m =  len -1;
		int mid = m/2;
		double [] rlp = new double[len];
		for (int n=0;n<len;n++){
			if (n-mid==0) rlp[n] = omega/PI;
			else rlp[n] = sin(omega * (n-mid)) / (PI*(n-mid));
		}
		return rlp;
	}
	public static double[] blackmanWindow(int order) {
		double [] w = new double[order];
		for (int i=0;i<order;i++) {
			w[i] = 0.42-0.5*cos(2*PI*i/order)+0.08*cos(4*PI*i/order);
		}
		return w;
	}
	public static double[] hammingWindow(int order) {
		double[] w = new double[order];
		for (int i = 0; i < order; i++)
		{
			w[i] = 0.54 - 0.46 * Math.cos((2.0 * PI * i) / (order - 1));
		}
		return w;
	}
	public static double[] multiply(double[] a, double[] b) {
		double[] res = new double[a.length];
		for (int i=0; i<a.length; i++) {
			res[i] = a[i] * b[i];
		}
		return res;
	}
	public static double[] blackmanLowPass(int order, double omega) {
		return multiply(blackmanWindow(order), rectangularLowPass(order,omega));
	}
}
