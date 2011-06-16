package org.zaluum.example.sound;
import static java.lang.Math.PI;
import static java.lang.Math.cos;
import static java.lang.Math.sin;

import org.apache.commons.math.complex.Complex;
import org.apache.commons.math.transform.FastFourierTransformer;
public class FilterFactory {
	public static double[] low(int size,int h) {
		double[] eq = new double[size];
		for (int i=0; i<h; i++){
			eq[i] = 1.0/(h*2);
			eq[size-i-1] = 1.0/(h*2);
		}
		return eq;
	}
	public static double[] expandEQ(double[] eq, int len) {
		 return makeSimmetric(resample(eq,len/2));
	}
	
	public static double[] rotateHalf(double[] arr) {
		double[] res =  new double[arr.length];
		for (int i=0; i<arr.length; i++){
			res[(i+arr.length/2)%arr.length] = arr[i];
		}
		return res;
	}
	public static double[] resample(double[] arr, int newLen) {
		if (newLen%arr.length!=0) throw new IllegalArgumentException();
		int samples = newLen/arr.length;
		double[] ret = new double[newLen];
		for (int i=0;i<arr.length;i++){
			for (int j=0;j<samples;j++){
				ret[(i*samples)+j] = arr[i];
			}
		}
		return ret;
	}
	public static double[] mid(int halflen, int start, int stop) {
		double[] full = new double[halflen*2];
		for (int i=0; i<halflen; i++) {
			int sym = full.length-1-i;
			if (i<start) 
				full[i]=0;
			else if (i>stop) 
				full[i]=0; 
			else 
				full[i] = 1.0/(stop-start);
			full[sym] = full[i];
		}
		return full;
	}
	public static double[] makeSimmetric(double[] arr) {
		double [] ret = new double[arr.length*2];
		for (int i=0; i<arr.length; i++){
			ret[i] = arr[i];
			ret[ret.length-i-1] = arr[i];
		}
		return ret;
	}
	public static double[] normalize(double[] arr) {
		double total = 0;
		double[] ret= new double[arr.length]; 
		for (int i=0; i<arr.length; i++){
			total += arr[i];
		}
		if (total!=0){
			for (int i=0; i<arr.length; i++) {
				ret[i] = arr[i]/total;
			}
		}
		return ret;
	}
	public static double[] fftForConvolution(double []coefs) {
		return rotateHalf(fft(coefs));
	}
	public static double[] fft(double[] eq) {
		FastFourierTransformer f = new FastFourierTransformer();
		Complex[] inversetransform = f.transform(eq);
		double[] r = new double[inversetransform.length];
		for (int i=0; i<inversetransform.length; i++) {
			Complex c = inversetransform[i];
			r[i] = c.getReal();//Math.sqrt(c.getReal()*c.getReal() + c.getImaginary()*c.getImaginary());
		}
		return r;
	}
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
