package org.zaluum.example.sound;

import java.util.ArrayList;
import java.util.List;

import org.zaluum.annotation.Box;
import org.zaluum.annotation.StaticBox;

@StaticBox
public class WaveletPitchDetector {
	@Box
	public static double freq(double[] data) {
		return freq(data, 44100, 0);
	}

	public static double freq(double[] data, int fs, double oldFreq) {
		double oldMode = 0;
		if (oldFreq != 0)
			oldMode = fs / oldFreq;
		int dataLen = data.length;
		double freq = 0;
		int lev = 6;
		double globalMaxThresh = 0.75;
		double maxFreq = 3000;
		int diffLevs = 3;
		int[] maxCount = new int[lev];
		int[] minCount = new int[lev];
		maxCount[0] = 0;
		minCount[0] = 0;

		double globalMin = Double.POSITIVE_INFINITY;
		double globalMax = Double.NEGATIVE_INFINITY;
		double sum = 0;
		for (int i = 0; i < data.length; i++) {
			double d = data[i];
			if (d > globalMax)
				globalMax = d;
			if (d < globalMin)
				globalMin = d;
			sum += d;
		}
		double aver = sum / dataLen;
		double maxThresh = globalMaxThresh * (globalMax - aver) + aver;
		double minThresh = globalMaxThresh * (globalMin - aver) + aver;
		double[][] a = new double[lev][];
		double[][] d = new double[lev][];
		List<List<Integer>> maxIndices = new ArrayList<List<Integer>>();
		List<List<Integer>> minIndices = new ArrayList<List<Integer>>();
		for (int i = 0; i < lev; i++) {
			maxIndices.add(new ArrayList<Integer>());
			minIndices.add(new ArrayList<Integer>());
		}
		double mode[] = new double[lev];
		a[0] = data;
		for (int i = 1; i < lev; i++) {
			int newWidth = dataLen / ((int) Math.pow(2, i));
			d[i] = new double[newWidth];
			a[i] = new double[newWidth];
			for (int j = 0; j < newWidth; j++) {
				d[i][j] = a[i - 1][2 * j + 1] - a[i - 1][2 * j];
			}
			for (int j = 0; j < newWidth; j++) {
				a[i][j] = a[i - 1][2 * j] - d[i][j] / 2;
			}
			double minDist = Math.max(
					Math.floor(fs / maxFreq / Math.pow(2, i)), 1);
			maxCount[i] = 0;
			minCount[i] = 0;
			int climber = 0;
			if (a[i][1] - a[i][0] > 0)
				climber = 1;
			else
				climber = -1;

			boolean canExt = true;
			double tooClose = 0;
			for (int j = 1; j < newWidth; j++) {
				double test = a[i][j] - a[i][j - 1];
				if (climber >= 0 && test < 0) {
					if (a[i][j - 1] >= maxThresh && canExt && tooClose == 0) {
						maxCount[i] = maxCount[i] + 1;
						maxIndices.get(i).add(j - 1);
						canExt = false;
						tooClose = minDist;
					}
					climber = -1;
				} else if (climber <= 0 && test > 0) {
					if (a[i][j - 1] <= minThresh && canExt && tooClose == 0) {
						minCount[i] = minCount[i] + 1;
						minIndices.get(i).add(j - 1);
						canExt = false;
						tooClose = minDist;
					}
					climber = 1;
				}
				if ((a[i][j] <= aver && a[i][j - 1] > aver)
						|| (a[i][j] >= aver && a[i][j - 1] < aver)) {
					canExt = true;
				}
				if (tooClose != 0)
					tooClose = tooClose - 1;
			}

			if (maxCount[i] >= 2 && minCount[i] >= 2) {
				ArrayList<Integer> differs = new ArrayList<Integer>();
				for (int j = 1; j <= diffLevs; j++) {
					int len = maxCount[i] - j;
					if (len > 0) {
						for (int k = 0; k < len; k++) {
							differs.add(Math.abs(maxIndices.get(i).get(k + j)
									- maxIndices.get(i).get(k)));
						}
					}
					len = minCount[i] - j;
					if (len > 0) {
						for (int k = 0; k < len; k++) {
							differs.add(Math.abs(minIndices.get(i).get(k + j)
									- minIndices.get(i).get(k)));
						}
					}
				}
				int dCount = differs.size();
				int numer = 1;
				
				for (int jj = 0; jj < dCount; jj++) {
					int numerJ = findTimes(differs, minDist);
					if (numerJ >= numer
							&& numerJ > Math.floor(newWidth / differs.get(jj)) / 4) { // why?
						if (numerJ == numer) {
							if (oldMode != 0
									&& Math.abs(differs.get(jj) - oldMode
											/ (Math.pow(2, i))) < minDist)
								mode[i] = differs.get(jj);
							else if (oldMode == 0
									&& (differs.get(jj) > 1.95 * mode[i] && differs
											.get(jj) < 2.05 * mode[i]))
								mode[i] = differs.get(jj);
						} else {
							numer = numerJ;
							mode[i] = differs.get(jj);
						}
					} else if (numerJ == numer - 1
							&& oldMode != 0
							&& Math.abs(differs.get(jj) - oldMode
									/ Math.pow(2, i)) < minDist)
						mode[i] = differs.get(jj);
				}
				// set mode via averaging
				if (mode[i] != 0) {
					mode[i] = mean(differs, minDist, mode[i]);
				}
				// determine if modes are shared
				if (mode[i - 1] != 0 && maxCount[i - 1] >= 2
						&& minCount[i - 1] >= 2) {
					if (Math.abs(mode[i - 1] - 2 * mode[i]) <= minDist) {
						freq = fs / mode[i - 1] / Math.pow(2, i - 2);
						return freq;
					}
				}

			}
		}
		return freq;
	}

	public static double mean(ArrayList<Integer> differs, double minDist,
			double mode) {
		double sum = 0;
		int count = 0;

		for (double d0 : differs) {
			if (Math.abs(mode - d0) <= minDist) {
				sum += d0;
				count++;
			}
		}
		if (count == 0)
			return 0;
		else
			return sum / count;
	}

	public static int findTimes(ArrayList<Integer> differs, double mindist) {
		int count = 0;
		for (int d0 : differs) {
			for (int d1 : differs) {
				if (Math.abs(d0 - d1) <= mindist) {
					count++;
				}
			}
		}
		return count;
	}

	public static void main(String[] args) {
		double[] sin = new double[1024];
		for (int i = 0; i < sin.length; i++) {
			sin[i] = Math.sin(2 * Math.PI * 550 * i / 44100.0);
		}
		System.out.println(freq(sin));
	}
}
