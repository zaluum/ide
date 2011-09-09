package org.zaluum.math;

import java.util.ArrayList;

import org.zaluum.annotation.Apply;
import org.zaluum.annotation.Box;

@Box
public class Average {
	private double sum;
	private ArrayList<Double> arrayList;
	private final int num;
	public Average(int num) {
		this.num = num;
		arrayList = new ArrayList<Double>(num);
	}
	@Apply
	public double apply(double in) {
		if (arrayList.size()==num)
			sum -= arrayList.remove(0);
		arrayList.add(in);
		sum += in;
		return sum/num;
	}
}
